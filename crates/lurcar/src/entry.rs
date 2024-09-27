use std::{
    cmp,
    collections::{HashMap, HashSet},
    io::{ErrorKind, Read, Seek, Write},
};

use anyhow::{bail, Result};

use crate::archive::Archive;

pub struct FileEntry<'a, I> {
    pub(crate) i: &'a mut Archive<I>,
    pub(crate) off: u64,
    pub(crate) len: u64,
    pub(crate) rem_pad: u64,
    pub(crate) pos: u64,
    pub(crate) index: u32,
}

impl<'a, I> FileEntry<'a, I> {
    pub fn consume_pad(&mut self, pad_to_consume: u64) -> Result<()> {
        if pad_to_consume > self.rem_pad {
            bail!("Tried to consume more pad than there is");
        }
        self.rem_pad -= pad_to_consume;
        self.len += pad_to_consume;
        self.i.entry_consume_pad(self.index, pad_to_consume)?;
        Ok(())
    }
    pub fn expand_pad(&mut self, pad_to_expand: u64) -> Result<()> {
        if pad_to_expand > self.len {
            bail!("Tried to pad more than possible");
        }
        self.rem_pad += pad_to_expand;
        self.len -= pad_to_expand;
        self.i.entry_expand_pad(self.index, pad_to_expand)?;
        Ok(())
    }
    pub fn pad(&self) -> u64 {
        self.rem_pad
    }
    pub fn len(&self) -> u64 {
        self.len
    }
    pub fn pos(&self) -> u64 {
        self.pos
    }
}

impl<'a, I: Seek> Seek for FileEntry<'a, I> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        let out = match pos {
            std::io::SeekFrom::Start(i) => {
                if i > self.len {
                    return Err(std::io::Error::new(ErrorKind::Other, "Seek out of bounds"));
                }
                self.i.seek(std::io::SeekFrom::Start(i + self.off))
            }
            std::io::SeekFrom::End(i) => {
                if i > 0 {
                    return Err(std::io::Error::new(ErrorKind::Other, "Seek out of bounds"));
                } else if i.abs() > self.len as i64 {
                    return Err(std::io::Error::new(ErrorKind::Other, "Seek out of bounds"));
                }
                self.i.seek(std::io::SeekFrom::Start(
                    ((self.off + self.len) as i64 + i) as u64,
                ))
            }
            std::io::SeekFrom::Current(i) => {
                let c = self.pos as i64 + i;
                if c < 0 {
                    return Err(std::io::Error::new(ErrorKind::Other, "Seek out of bounds"));
                }
                if c > self.len as i64 {
                    return Err(std::io::Error::new(ErrorKind::Other, "Seek out of bounds"));
                }
                self.i.seek(std::io::SeekFrom::Current(i))
            }
        };
        if let Ok(i) = out {
            let pos = i - self.off;
            self.pos = pos;
            Ok(self.pos)
        } else {
            out
        }
    }
}

impl<'a, R: Read> Read for FileEntry<'a, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let rem = self.len - self.pos;
        if rem == 0 {
            return Ok(0);
        }

        let max = cmp::min(buf.len() as u64, rem) as usize;
        let n = self.i.inner.get_mut().read(&mut buf[..max])?;
        assert!(n as u64 <= rem, "number of read bytes exceeds limit");
        self.pos += n as u64;
        self.i.pos.set(self.i.pos.get() + n as u64);
        Ok(n)
    }
}

impl<'a, W: Write> Write for FileEntry<'a, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let rem = self.len - self.pos;
        if rem == 0 {
            return Ok(0);
        }
        let max = cmp::min(buf.len() as u64, rem) as usize;

        let n = self.i.inner.get_mut().write(&buf[..max])?;
        assert!(n as u64 <= rem, "number of written bytes exceeds limit");
        self.pos += n as u64;
        self.i.pos.set(self.i.pos.get() + n as u64);
        Ok(n)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.i.inner.get_mut().flush()
    }
}

pub struct FileEntryBuilder<R> {
    pub(crate) r: R,
    pub(crate) pad: u64,
    pub(crate) extra_fields: HashMap<String, Vec<u8>>,
    pub(crate) extra_set: HashSet<String>,
    pub(crate) parent: u32,
    pub(crate) known_len: Option<u64>,
}

impl<R: Read> FileEntryBuilder<R> {
    pub fn new(r: R) -> Self {
        Self {
            r,
            pad: 0,
            extra_fields: HashMap::new(),
            extra_set: HashSet::new(),
            parent: 0,
            known_len: None,
        }
    }
    pub fn pad(mut self, pad: u64) -> Self {
        self.pad = pad;
        self
    }
    pub fn extra_field(mut self, key: String, val: Vec<u8>) -> Self {
        self.extra_fields.insert(key, val);
        self
    }
    pub fn extra_set(mut self, key: String) -> Self {
        self.extra_set.insert(key);
        self
    }
    pub fn parent(mut self, parent: u32) -> Self {
        self.parent = parent;
        self
    }
    pub fn len(mut self, len: u64) -> Self {
        self.known_len = Some(len);
        self
    }
}

pub struct DirEntryBuilder {
    pub(crate) extra_fields: HashMap<String, Vec<u8>>,
    pub(crate) extra_set: HashSet<String>,
    pub(crate) parent: u32,
}
impl DirEntryBuilder {
    pub fn new() -> Self {
        Self {
            extra_fields: HashMap::new(),
            extra_set: HashSet::new(),
            parent: 0,
        }
    }
    pub fn extra_field(mut self, key: String, val: Vec<u8>) -> Self {
        self.extra_fields.insert(key, val);
        self
    }
    pub fn extra_set(mut self, key: String) -> Self {
        self.extra_set.insert(key);
        self
    }
    pub fn parent(mut self, parent: u32) -> Self {
        self.parent = parent;
        self
    }
}
