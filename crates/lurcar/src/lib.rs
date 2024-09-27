#![feature(let_chains)]
pub mod archive;
pub mod entry;
#[cfg(feature = "fuse")]
pub mod fuse;

const MAGIC_NUMBER: u64 = 0x006C757263617200;

use std::collections::{BTreeMap, HashMap, HashSet};

use bitcode::{Decode, Encode};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Decode, Encode, Serialize, Deserialize, Clone, Default)]
pub struct DirectoryFileEntry {
    pub pad: u64,
    pub len: u64,
    /// CRC-32 Checksum of uncompressed data
    pub checksum: Option<u32>,
    pub name: String,
    /// extra fields, normally not present
    pub extra_fields: HashMap<String, Vec<u8>>,
    pub extra_set: HashSet<String>,
    pub parent: u32,
    pub unallocated_space_after: u64,
}

#[derive(Debug, PartialEq, Decode, Encode, Serialize, Deserialize, Clone, Default)]
pub struct Directory {
    pub unallocated_space: u64,
    pub file_entries: Vec<DirectoryFileEntry>,
    pub dir_entries: BTreeMap<u32, DirectoryDirEntry>,
}

#[derive(Debug, PartialEq, Decode, Encode, Serialize, Deserialize, Clone, Default)]
pub struct DirectoryDirEntry {
    pub name: String,
    pub extra_fields: HashMap<String, Vec<u8>>,
    pub extra_set: HashSet<String>,
    pub parent: Option<u32>,
}
#[derive(Debug, PartialEq, Decode, Encode, Serialize, Deserialize, Clone)]
pub enum DirectoryEntry {
    File(DirectoryFileEntry),
    Dir(DirectoryDirEntry),
}
