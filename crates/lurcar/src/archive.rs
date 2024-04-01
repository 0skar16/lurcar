use crate::{entry::{Entry, EntryBuilder}, Directory, DirectoryEntry, MAGIC_NUMBER};
use anyhow::{anyhow, bail, Result};
use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};
use std::{
    cell::{Cell, RefCell},
    io::{self, repeat, Read, Seek, Write},
};

const START: u64 = 20;

pub struct Archive<I> {
    pub(crate) inner: RefCell<I>,
    pub(crate) directory: Directory,
    change_dir: bool,
    dir_len: u32,
    dir_pos: u64,
    pos: Cell<u64>,
}

impl<R: Read + Seek> Archive<R> {
    pub fn open_internal_dir(mut r: R) -> Result<Self> {
        assert_eq!(r.read_u64::<BigEndian>()?, MAGIC_NUMBER);
        let dir_pos = r.read_u64::<LittleEndian>()?;
        let dir_len = r.read_u32::<LittleEndian>()?;

        r.seek(std::io::SeekFrom::Start(dir_pos))?;
        let mut buf = vec![0u8; dir_len as usize];
        r.read_exact(&mut buf)?;

        let directory = bitcode::decode(&buf)?;

        r.seek(std::io::SeekFrom::Start(START))?;

        Ok(Self {
            inner: RefCell::new(r),
            directory: directory,
            dir_len,
            dir_pos,
            pos: Cell::new(START),
            change_dir: true,
        })
    }
}

impl<R: Read> Archive<R> {
    pub fn open_extern_dir(mut r: R, directory: Directory) -> Result<Self> {
        assert_eq!(r.read_u64::<BigEndian>()?, MAGIC_NUMBER);
        r.read_u64::<LittleEndian>()?;
        r.read_u32::<LittleEndian>()?;
        Ok(Self {
            inner: RefCell::new(r),
            directory,
            dir_len: 0,
            dir_pos: 0,
            pos: Cell::new(START),
            change_dir: false,
        })
    }
}

impl<W: Write + Seek> Archive<W> {
    pub fn append_entry<R: Read>(
        &mut self,
        name: String,
        entry_builder: EntryBuilder<R>,
    ) -> Result<u32> {
        if self.change_dir {
            self.inner
                .get_mut()
                .seek(io::SeekFrom::End(-(self.dir_len as i64)))?;

            self.change_dir = false;
        } else {
            self.inner.get_mut().seek(io::SeekFrom::End(0))?;
        }

        self.append_entry_unchecked(name, entry_builder)
    }
    pub fn finalize_with_dir(mut self) -> Result<()> {
        if !self.change_dir {
            let mut w = self.inner.borrow_mut();
            self.dir_pos = w.seek(io::SeekFrom::End(0))?;
            let dir = self.directory;
            let dir_buf = bitcode::encode(&dir);
            self.dir_len = dir_buf.len() as u32;
            w.write_all(&dir_buf)?;
            w.seek(io::SeekFrom::Start(8))?;
            w.write_u64::<LittleEndian>(self.dir_pos)?;
            w.write_u32::<LittleEndian>(self.dir_len)?;
        }
        Ok(())
    }
}
impl<W: Write> Archive<W> {
    pub fn new(mut w: W) -> Result<Self> {
        w.write_u64::<BigEndian>(MAGIC_NUMBER)?;
        w.write_u64::<LittleEndian>(0)?;
        w.write_u32::<LittleEndian>(0)?;
        Ok(Self {
            inner: RefCell::new(w),
            directory: Directory::default(),
            dir_len: 0,
            dir_pos: 0,
            pos: Cell::new(START),
            change_dir: false,
        })
    }

    pub fn append_entry_unchecked<R: Read>(
        &mut self,
        name: String,
        mut entry_builder: EntryBuilder<R>,
    ) -> Result<u32> {
        let len = io::copy(&mut entry_builder.r, &mut *self.inner.borrow_mut())?;

        let mut r = repeat(0).take(entry_builder.pad);
        std::io::copy(&mut r, &mut *self.inner.borrow_mut())?;

        let entry = DirectoryEntry {
            pad: entry_builder.pad,
            len,
            checksum: None,
            name,
            extra_fields: entry_builder.extra_fields,
            extra_set: entry_builder.extra_set,
        };

        self.directory.entries.push(entry);

        Ok(self.directory.entries.len() as u32 - 1)
    }

    pub fn finalize_extern_dir(self) -> Result<Directory> {
        Ok(self.directory)
    }
}
impl<I: Seek> Archive<I> {
    pub fn entry<'a>(&'a mut self, entry_id: usize) -> Result<Entry<'a, I>> {
        let mut off = START;
        if entry_id >= self.directory.entries.len() {
            bail!("entry_id exceeds number of entries");
        }
        for i in 0..entry_id {
            let e = unsafe { self.directory.entries.get_unchecked(i) };
            off += e.len;
            off += e.pad;
        }
        let entry = unsafe { self.directory.entries.get_unchecked(entry_id) }.clone();
        self.inner.get_mut().seek(io::SeekFrom::Start(off))?;
        Ok(Entry {
            i: self,
            rem_pad: entry.pad,
            off,
            len: entry.len,
            pos: 0,
            index: entry_id,
        })
    }
}
impl<I> Archive<I> {
    pub fn entry_consume_pad(&mut self, entry_id: usize, pad_to_consume: u64) -> Result<()> {
        let e = self.directory.entries.get_mut(entry_id).ok_or(anyhow!("Entry doesn't exist"))?;
        if pad_to_consume > e.pad {
            bail!("Tried to consume more pad than there is");
        }
        e.len += pad_to_consume;
        e.pad -= pad_to_consume;
        Ok(())
    }
    pub fn entry_expand_pad(&mut self, entry_id: usize, pad_to_expand: u64) -> Result<()> {
        let e = self.directory.entries.get_mut(entry_id).ok_or(anyhow!("Entry doesn't exist"))?;
        if pad_to_expand > e.len {
            bail!("Tried to consume more pad than there is");
        }
        e.len -= pad_to_expand;
        e.pad += pad_to_expand;
        Ok(())
    }
}