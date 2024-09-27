use fuser::*;
use std::{
    ffi::c_int,
    io::{Read, Seek, Write},
    time::Duration,
};

use crate::archive::Archive;

pub struct ArchiveFS<I> {
    inner: Archive<I>,
    uid: u32,
    gid: u32,
}

#[derive(Debug, Clone, Copy)]
enum EntryID {
    File(u32),
    Dir(u32),
}

fn to_ino(eid: EntryID) -> u64 {
    match eid {
        EntryID::File(id) => (id << 1) as u64,
        EntryID::Dir(id) => (1 + id << 1) as u64,
    }
}
fn from_ino(ino: u64) -> EntryID {
    if ino % 2 == 0 {
        EntryID::File((ino >> 1) as u32)
    } else {
        EntryID::Dir((ino >> 1) as u32)
    }
}

impl<I: Read + Write + Seek> ArchiveFS<I> {
    pub fn new(inner: Archive<I>) -> Self {
        Self {
            inner,
            uid: 0,
            gid: 0,
        }
    }
}

impl<I: Read + Write + Seek> fuser::Filesystem for ArchiveFS<I> {
    fn init(&mut self, req: &Request<'_>, config: &mut KernelConfig) -> Result<(), c_int> {
        self.uid = req.uid();
        self.gid = req.gid();
        Ok(())
    }

    fn readdir(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: ReplyDirectory,
    ) {
        match from_ino(ino) {
            EntryID::Dir(dir) => {
                if !self.inner.directory.dir_entries.contains_key(&dir) {
                    reply.error(libc::ENOENT);
                } else {
                    let dirs = self
                        .inner
                        .directory
                        .dir_entries
                        .iter()
                        .filter(|(_, entry)| entry.parent == Some(dir))
                        .map(|(id, dir)| {
                            (
                                to_ino(EntryID::Dir(*id)),
                                FileType::Directory,
                                dir.name.clone(),
                            )
                        });
                    let files = self
                        .inner
                        .directory
                        .file_entries
                        .iter()
                        .enumerate()
                        .filter(|(_, entry)| entry.parent == dir)
                        .map(|(id, dir)| {
                            (
                                to_ino(EntryID::File(id as u32)),
                                FileType::RegularFile,
                                dir.name.clone(),
                            )
                        });
                    let mut entries: Vec<(_, _, _)> =
                        dirs.chain(files).skip(offset as usize).collect();
                    entries.sort_by(|(ino, _, _), (ino2, _, _)| ino.cmp(ino2));
                    for (offset, (ino, kind, name)) in entries.into_iter().enumerate() {
                        let _ = reply.add(ino, offset as i64, kind, name);
                    }
                    reply.ok();
                }
            }
            EntryID::File(_) => reply.error(libc::ENOTDIR),
        }
    }

    fn lookup(
        &mut self,
        _req: &Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: ReplyEntry,
    ) {
        reply.entry(
            &Duration::from_secs(1),
            &FileAttr {
                ino: todo!(),
                size: todo!(),
                blocks: todo!(),
                atime: todo!(),
                mtime: todo!(),
                ctime: todo!(),
                crtime: todo!(),
                kind: todo!(),
                perm: todo!(),
                nlink: todo!(),
                uid: todo!(),
                gid: todo!(),
                rdev: todo!(),
                blksize: todo!(),
                flags: todo!(),
            },
            0,
        );
    }
}
