pub mod archive;
pub mod entry;

const MAGIC_NUMBER: u64 = 0x006C757263617200;

use std::collections::{HashMap, HashSet};

use bitcode::{Decode, Encode};

#[derive(Debug, PartialEq, Decode, Encode, Clone)]
pub struct DirectoryEntry {
    pad: u64,
    len: u64,
    /// CRC-32 Checksum of uncompressed data
    checksum: Option<u32>,
    name: String,
    /// extra fields, normally not present
    extra_fields: HashMap<String, Vec<u8>>,
    extra_set: HashSet<String>,
}

#[derive(Debug, PartialEq, Decode, Encode, Clone, Default)]
pub struct Directory {
    entries: Vec<DirectoryEntry>,
}
