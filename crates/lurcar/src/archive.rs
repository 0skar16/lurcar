use crate::{
    entry::{DirEntryBuilder, FileEntry, FileEntryBuilder},
    Directory, DirectoryDirEntry, DirectoryEntry, DirectoryFileEntry, MAGIC_NUMBER,
};
use anyhow::{anyhow, bail, Result};
use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};
use serde::{Deserialize, Serialize};
use std::{
    cell::{Cell, RefCell},
    cmp::min,
    collections::{HashMap, HashSet},
    io::{self, repeat, Read, Seek, SeekFrom, Write},
    path::PathBuf,
};

const START: u64 = 20;

pub struct Archive<I> {
    pub(crate) inner: RefCell<I>,
    pub(crate) directory: Directory,
    dir_len: u32,
    dir_pos: u64,
    pub(crate) pos: Cell<u64>,
    last_entry_read: Option<u32>,
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
            last_entry_read: None,
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
            last_entry_read: None,
        })
    }
}
impl<W: Write + Seek> Archive<W> {
    pub fn append_file_entry<R: Read>(
        &mut self,
        name: String,
        entry_builder: FileEntryBuilder<R>,
    ) -> Result<u32> {
        if let Some(known_len) = entry_builder.known_len {
            let (entry_id, unallocated_space_after) =
                self.allocate_new_entry(known_len + entry_builder.pad);
            let mut off = 0;
            for i in 0..entry_id {
                off += unsafe {
                    let e = self.directory.file_entries.get_unchecked(i as usize);
                    e.len
                        + e.pad
                        + if i == entry_id - 1 {
                            0
                        } else {
                            e.unallocated_space_after
                        }
                };
            }
            self.seek(io::SeekFrom::Start(START + off))?;
            self.append_file_entry_unchecked_internal(
                name,
                entry_builder,
                entry_id,
                unallocated_space_after,
            )
        } else {
            self.seek(io::SeekFrom::Start(
                self.compute_full_archive_len()
                    - if let Some(last) = self.directory.file_entries.last() {
                        last.unallocated_space_after
                    } else {
                        0
                    },
            ))?;
            self.append_file_entry_unchecked(name, entry_builder)
        }
    }

    pub fn finalize_with_dir(mut self) -> Result<(W, Directory)> {
        let directory = self.write_directory()?;
        Ok((self.inner.into_inner(), directory))
    }
    pub fn write_directory(&mut self) -> Result<Directory> {
        let e = self.directory.file_entries[0].unallocated_space_after;
        self.directory.file_entries[0].unallocated_space_after = u32::MAX as u64;
        let allocated_len = bitcode::encode(&self.directory).len() as u64;
        self.directory.file_entries[0].unallocated_space_after = e;

        let (entry_id, rest_of_unallocated_space) = self.allocate_new_entry(allocated_len);
        self.dir_pos = self.compute_offset(entry_id) + rest_of_unallocated_space;
        self.directory.file_entries[entry_id as usize - 1].unallocated_space_after =
            rest_of_unallocated_space + allocated_len;
        let dir_buf = bitcode::encode(&self.directory);
        self.dir_len = dir_buf.len() as u32;
        self.seek(SeekFrom::Start(self.dir_pos))?;

        let mut w = self.inner.borrow_mut();
        w.write_all(&dir_buf)?;
        w.seek(io::SeekFrom::Start(0))?;
        w.write_u64::<BigEndian>(MAGIC_NUMBER)?;
        w.write_u64::<LittleEndian>(self.dir_pos)?;
        w.write_u32::<LittleEndian>(self.dir_len)?;
        Ok(self.directory())
    }
    pub fn pack(&mut self, dir_path: PathBuf, root_name: Option<String>) {
        if let Some(root_name) = root_name {
            if let Some(entry) = self.directory.dir_entries.get_mut(&0) {
                entry
                    .extra_fields
                    .insert("root_name".to_string(), root_name.as_bytes().to_vec());
            }
        }
        self.pack_folder(dir_path, 0)
    }

    fn pack_folder(&mut self, dir_path: PathBuf, dir_id: u32) {
        for file in std::fs::read_dir(dir_path).expect("Couldn't read dir") {
            if let Ok(file) = file {
                let Ok(file_type) = file.file_type() else {
                    continue;
                };
                let filename = file.file_name();
                let filename = filename.to_string_lossy();
                if file_type.is_dir() {
                    let dir_id = self
                        .append_dir_entry(
                            filename.to_string(),
                            DirEntryBuilder::new().parent(dir_id),
                        )
                        .expect(&format!(
                            "Couldn't append archive entry: [{}]",
                            file.path().display()
                        ));
                    self.pack_folder(file.path(), dir_id);
                } else {
                    let f = std::fs::File::open(file.path()).expect("Couldn't read file");
                    self.append_file_entry(
                        filename.to_string(),
                        FileEntryBuilder::new(f).parent(dir_id),
                    )
                    .expect(&format!(
                        "Couldn't append archive entry: [{}]",
                        file.path().display()
                    ));
                }
            }
        }
    }

    pub fn rebuild_with_readhead<R: Read + Seek>(&mut self, r: &mut R) -> Result<()> {
        let mut skipped = self.directory.unallocated_space;
        self.directory.unallocated_space = 0;
        let mut off = 0;
        let mut inner = self.inner.borrow_mut();
        inner.seek(SeekFrom::Start(START + off))?;
        let mut seeked = false;
        let mut pos = self.pos.get();
        for e in self.directory.file_entries.iter_mut() {
            if skipped > 0 {
                if !seeked {
                    pos = inner.seek(SeekFrom::Start(START + off))?;
                    seeked = true;
                }

                r.seek(SeekFrom::Start(START + off + skipped))?;

                std::io::copy(&mut r.take(e.len + e.pad), inner.by_ref())?;
                pos += e.len + e.pad;
            }
            skipped += e.unallocated_space_after;
            e.unallocated_space_after = 0;
            off += e.len + e.pad;
        }
        self.pos.set(pos);
        Ok(())
    }
}
impl<W: Write> Archive<W> {
    pub fn new(mut w: W) -> Result<Self> {
        w.write_u64::<BigEndian>(MAGIC_NUMBER)?;
        w.write_u64::<LittleEndian>(0)?;
        w.write_u32::<LittleEndian>(0)?;
        let mut dir = Directory::default();
        dir.dir_entries.insert(
            0,
            DirectoryDirEntry {
                name: "/".to_string(),
                extra_fields: HashMap::new(),
                extra_set: HashSet::from(["root".to_string()]),
                parent: None,
            },
        );
        Ok(Self {
            inner: RefCell::new(w),
            directory: dir,
            dir_len: 0,
            dir_pos: 0,
            pos: Cell::new(START),
            last_entry_read: None,
        })
    }

    pub fn append_file_entry_unchecked<R: Read>(
        &mut self,
        name: String,
        entry_builder: FileEntryBuilder<R>,
    ) -> Result<u32> {
        self.append_file_entry_unchecked_internal(
            name,
            entry_builder,
            self.directory.file_entries.len() as u32,
            0,
        )
    }
    fn append_file_entry_unchecked_internal<R: Read>(
        &mut self,
        name: String,
        mut entry_builder: FileEntryBuilder<R>,
        entry_id: u32,
        unallocated_space_after: u64,
    ) -> Result<u32> {
        let len = io::copy(&mut entry_builder.r, &mut *self.inner.borrow_mut())?;

        let mut r = repeat(0).take(entry_builder.pad);
        std::io::copy(&mut r, &mut *self.inner.borrow_mut())?;

        let entry = DirectoryFileEntry {
            pad: entry_builder.pad,
            len,
            checksum: None,
            name,
            extra_fields: entry_builder.extra_fields,
            extra_set: entry_builder.extra_set,
            parent: entry_builder.parent,
            unallocated_space_after,
        };

        self.directory.file_entries.insert(entry_id as usize, entry);
        if entry_id != 0 {
            unsafe {
                self.directory
                    .file_entries
                    .get_unchecked_mut(entry_id as usize - 1)
            }
            .unallocated_space_after = 0;
        } else {
            self.directory.unallocated_space = 0;
        }
        Ok(entry_id)
    }
    pub fn create_file_entry(&mut self, mut entry: DirectoryFileEntry) -> (u32, u64) {
        let (entry_id, rest_of_allocated_space) = self.allocate_new_entry(entry.len + entry.pad);
        entry.unallocated_space_after = rest_of_allocated_space;

        self.directory.file_entries.insert(entry_id as usize, entry);
        if entry_id != 0 {
            self.directory.file_entries[entry_id as usize - 1].unallocated_space_after = 0;
        } else {
            self.directory.unallocated_space = 0;
        }

        (entry_id, self.compute_offset(entry_id))
    }

    pub fn finalize_extern_dir(self) -> Result<(Directory, W)> {
        Ok((self.directory, self.inner.into_inner()))
    }
}
impl<I: Seek> Archive<I> {
    pub fn entry<'a>(&'a mut self, entry_id: u32) -> Result<Option<FileEntry<'a, I>>> {
        let mut off = START;
        if entry_id >= self.directory.file_entries.len() as u32 {
            return Ok(None);
        }
        for i in 0..entry_id {
            let e = unsafe { self.directory.file_entries.get_unchecked(i as usize) };
            off += e.len;
            off += e.pad;
            off += e.unallocated_space_after;
        }
        let entry = unsafe { self.directory.file_entries.get_unchecked(entry_id as usize) }.clone();
        self.seek(io::SeekFrom::Start(off))?;
        Ok(Some(FileEntry {
            i: self,
            rem_pad: entry.pad,
            off,
            len: entry.len,
            pos: 0,
            index: entry_id,
        }))
    }
    pub fn entry_by_path<'a>(
        &'a mut self,
        entry_path: PathBuf,
    ) -> Result<Option<FileEntry<'a, I>>> {
        let Some((entry_id, entry)) = self.entry_data_by_path(entry_path)? else {
            return Ok(None);
        };
        match entry {
            DirectoryEntry::File(_) => self.entry(entry_id),
            DirectoryEntry::Dir(_) => bail!("Not a file!"),
        }
    }
    pub(crate) fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let pos = self.inner.get_mut().seek(pos)?;
        self.pos.set(pos);
        Ok(pos)
    }
}
impl<I> Archive<I> {
    pub fn compute_full_archive_len(&self) -> u64 {
        self.compute_offset(self.directory.file_entries.len() as u32)
    }
    pub fn compute_full_archive_len_from_dir(dir: &Directory) -> u64 {
        let mut len = START;
        for e in dir.file_entries.iter() {
            len += e.len + e.pad + e.unallocated_space_after;
        }
        len
    }
    pub fn compute_offset(&self, max_entry: u32) -> u64 {
        let mut len = START + self.directory.unallocated_space;
        for (i, e) in self.directory.file_entries.iter().enumerate() {
            if i as u32 >= max_entry {
                break;
            }
            len += e.len + e.pad + e.unallocated_space_after;
        }
        len
    }
    fn allocate_new_entry(&self, len: u64) -> (u32, u64) {
        if self.directory.unallocated_space >= len {
            return (0, self.directory.unallocated_space - len);
        }
        if self.directory.file_entries.len() == 0 {
            return (0, 0);
        }
        for (eid, entry) in self.directory.file_entries.iter().enumerate() {
            if entry.unallocated_space_after >= len && eid < self.directory.file_entries.len() - 1 {
                return (eid as u32 + 1, entry.unallocated_space_after - len);
            }
            if eid == self.directory.file_entries.len() - 1 {
                if entry.unallocated_space_after >= len {
                    return (eid as u32 + 1, entry.unallocated_space_after - len);
                } else {
                    return (eid as u32 + 1, 0);
                }
            }
        }
        unreachable!();
    }
    pub fn entry_consume_pad(&mut self, entry_id: u32, pad_to_consume: u64) -> Result<()> {
        let e = self
            .directory
            .file_entries
            .get_mut(entry_id as usize)
            .ok_or(anyhow!("Entry doesn't exist"))?;
        if pad_to_consume > e.pad {
            bail!("Tried to consume more pad than there is");
        }
        e.len += pad_to_consume;
        e.pad -= pad_to_consume;
        Ok(())
    }
    pub fn entry_expand_pad(&mut self, entry_id: u32, pad_to_expand: u64) -> Result<()> {
        let e = self
            .directory
            .file_entries
            .get_mut(entry_id as usize)
            .ok_or(anyhow!("Entry doesn't exist"))?;
        if pad_to_expand > e.len {
            bail!("Tried to consume more pad than there is");
        }
        e.len -= pad_to_expand;
        e.pad += pad_to_expand;
        Ok(())
    }
    pub fn append_dir_entry(
        &mut self,
        name: String,
        entry_builder: DirEntryBuilder,
    ) -> Result<u32> {
        let free = {
            let mut free = 1;
            for i in 1..=u32::MAX {
                if !self.directory.dir_entries.contains_key(&i) {
                    free = i;
                    break;
                }
            }
            free
        };
        self.directory.dir_entries.insert(
            free,
            DirectoryDirEntry {
                name,
                extra_fields: entry_builder.extra_fields,
                extra_set: entry_builder.extra_set,
                parent: Some(entry_builder.parent),
            },
        );
        Ok(free)
    }
    pub fn entry_data_by_path(
        &mut self,
        mut entry_path: PathBuf,
    ) -> Result<Option<(u32, DirectoryEntry)>> {
        if !entry_path.is_absolute() {
            entry_path = PathBuf::from("/").join(entry_path);
        }
        let mut current_dir = ArchiveTree {
            id: u32::MAX,
            entry: DirectoryDirEntry {
                name: "unreachable_root".to_string(),
                extra_fields: HashMap::new(),
                extra_set: HashSet::new(),
                parent: None,
            },
            dirs: vec![self.tree()],
            files: HashMap::new(),
        };
        let parts = entry_path.iter().count();
        for (i, path_part) in entry_path.iter().enumerate() {
            let path_part = path_part.to_string_lossy().to_string();
            if i < parts - 1 {
                if let Some(new_dir) = current_dir
                    .dirs
                    .iter()
                    .find(|dir| dir.entry.name == path_part)
                {
                    current_dir = new_dir.clone();
                } else {
                    return Ok(None);
                }
            } else {
                if let Some(entry) = current_dir
                    .files
                    .iter()
                    .find(|(_, file)| file.name == path_part)
                    .map(|(a, b)| (*a, b.clone()))
                {
                    return Ok(Some((entry.0 as u32, DirectoryEntry::File(entry.1))));
                };
                if let Some(tree) = current_dir
                    .dirs
                    .iter()
                    .find(|dir| dir.entry.name == path_part)
                    .map(|dir| dir.clone())
                {
                    return Ok(Some((tree.id as u32, DirectoryEntry::Dir(tree.entry))));
                };
                return Ok(None);
            }
        }
        Ok(None)
    }
    pub fn remove_entry(&mut self, entry_id: u32) -> Result<()> {
        if entry_id >= self.directory.file_entries.len() as u32 {
            bail!("entry doesn't exist");
        }
        let entry = unsafe { self.directory.file_entries.get_unchecked(entry_id as usize) };
        let off = entry.len + entry.pad + entry.unallocated_space_after;
        if entry_id == 0 {
            self.directory.unallocated_space += off;
        } else {
            let previous_entry = unsafe {
                self.directory
                    .file_entries
                    .get_unchecked_mut((entry_id - 1) as usize)
            };
            previous_entry.unallocated_space_after += off;
        }
        self.directory.file_entries.remove(entry_id as usize);

        Ok(())
    }
    pub fn remove_dir_entry(&mut self, dir_id: u32, entry: DirectoryDirEntry) -> Result<()> {
        let tree = ArchiveTree::assmeble_dir(&self.directory, dir_id, entry);
        for dir in tree.dirs {
            self.remove_dir_entry(dir.id, dir.entry)?;
        }
        for (entry_id, _) in tree.files {
            self.remove_entry(entry_id)?;
        }
        self.directory.dir_entries.remove(&dir_id);
        Ok(())
    }
    pub fn remove_entry_by_path(&mut self, mut entry_path: PathBuf) -> Result<()> {
        let Some((entry_id, entry)) = self.entry_data_by_path(entry_path)? else {
            bail!("entry doesn't exist");
        };
        match entry {
            DirectoryEntry::File(_) => self.remove_entry(entry_id),
            DirectoryEntry::Dir(dir) => self.remove_dir_entry(entry_id, dir),
        }
    }

    pub fn resolve_entry_path(&self, entry: DirectoryEntry) -> Option<PathBuf> {
        let mut path;
        let mut entry_parent = match entry {
            DirectoryEntry::File(f) => {
                path = PathBuf::from(f.name);
                self.directory.dir_entries.get(&f.parent)?
            }
            DirectoryEntry::Dir(d) => {
                let Some(parent) = d.parent else {
                    return Some(PathBuf::from("/"));
                };
                path = PathBuf::from(d.name);
                self.directory.dir_entries.get(&parent)?
            }
        };
        while entry_parent.parent.is_some() {
            path = PathBuf::from(&entry_parent.name).join(path);
            entry_parent = self.directory.dir_entries.get(&entry_parent.parent?)?;
        }
        Some(PathBuf::from("/").join(path))
    }
    pub fn resolve_position(&self) -> Option<(u32, DirectoryFileEntry, u64, u64)> {
        let mut pos = START;
        for (id, e) in self.directory.file_entries.iter().enumerate() {
            if pos < START + e.len + e.pad {
                return Some((
                    id as u32,                      // Entry ID
                    e.clone(),                      // Entry
                    self.pos.get() - (pos - START), // Remaining length
                    pos,                            // Position in entry
                ));
            }
            pos += e.len + e.pad;
        }
        None
    }
    pub fn tree(&self) -> ArchiveTree {
        ArchiveTree::assemble(&self.directory)
    }
    pub fn directory(&self) -> Directory {
        self.directory.clone()
    }
    pub fn directory_mut(&mut self) -> &mut Directory {
        &mut self.directory
    }
}
impl<I: Read + Write + Seek> Archive<I> {
    pub fn rebuild(&mut self) -> Result<()> {
        const REBUILD_MAX_CHUNK_SIZE: u64 = 1024 * 1024 * 64;
        let mut skipped = self.directory.unallocated_space;
        self.directory.unallocated_space = 0;
        let mut off = 0;
        let mut inner = self.inner.borrow_mut();
        let mut pos = self.pos.get();
        for e in self.directory.file_entries.iter_mut() {
            if skipped > 0 {
                let mut left = e.len;
                while left > 0 {
                    inner.seek(SeekFrom::Start(START + off + skipped))?;
                    let read = min(e.len, REBUILD_MAX_CHUNK_SIZE);
                    let mut buf = vec![0u8; read as usize];
                    inner.read_exact(&mut buf)?;
                    pos = inner.seek(SeekFrom::Start(START + off))?;
                    inner.write_all(&buf)?;
                    pos += buf.len() as u64;
                    left -= read;
                }
            }
            skipped += e.unallocated_space_after;
            e.unallocated_space_after = 0;
            off += e.len + e.pad;
        }
        self.pos.set(pos);
        Ok(())
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArchiveTree {
    pub id: u32,
    pub entry: DirectoryDirEntry,
    pub dirs: Vec<ArchiveTree>,
    pub files: HashMap<u32, DirectoryFileEntry>,
}
impl ArchiveTree {
    pub fn assemble(directory: &Directory) -> Self {
        let root = directory.dir_entries.get(&0).unwrap().clone();
        Self::assmeble_dir(directory, 0, root)
    }
    fn assmeble_dir(directory: &Directory, dir_id: u32, entry: DirectoryDirEntry) -> Self {
        Self {
            id: dir_id,
            dirs: Self::get_all_children_dirs(directory, dir_id)
                .into_iter()
                .map(|(dir_id, entry)| Self::assmeble_dir(directory, dir_id, entry))
                .collect(),
            files: Self::get_all_children_files(directory, dir_id),
            entry,
        }
    }
    fn get_all_children_files(directory: &Directory, id: u32) -> HashMap<u32, DirectoryFileEntry> {
        directory
            .file_entries
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, d)| d.parent == id)
            .map(|(i, a)| (i as u32, a))
            .collect()
    }
    fn get_all_children_dirs(directory: &Directory, id: u32) -> HashMap<u32, DirectoryDirEntry> {
        directory
            .dir_entries
            .clone()
            .into_iter()
            .filter(|(_, d)| d.parent.is_some())
            .filter(|(_, d)| unsafe { d.parent.unwrap_unchecked() == id })
            .collect()
    }
}
