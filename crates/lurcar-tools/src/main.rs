use base64::Engine;
use clap::{arg, Arg, ArgMatches, Command, ValueEnum};
use colorize::AnsiColor;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use lurcar::{
    archive::{Archive, ArchiveTree},
    entry::{DirEntryBuilder, FileEntryBuilder},
    Directory,
};
use serde::Serialize;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::{File, OpenOptions},
    io::{stdout, Read, Seek, Write},
    path::PathBuf,
};
const DEFAULT_PAD: u64 = 4096;
fn cli() -> Command {
    Command::new("lurcar-tools")
        .about("Tools for creating and reading LURCARs")
        .subcommand_required(true)
        .allow_external_subcommands(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("pack")
                .about("Pack a directory into a LURCAR")
                .arg(arg!(source: <SOURCE>))
                .arg(arg!(-o - -out[OUT]))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(arg!(-w - -overwrite))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("list")
                .about("List files in a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(-j - -json))
                .arg(arg!(-p - -pretty))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("read")
                .about("Read a file in a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(file: <FILE>))
                .arg(arg!(-s - -start[START]))
                .arg(arg!(-e - -end[END]))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("info")
                .about("Get info about a file in a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(file: <FILE>))
                .arg(arg!(-j - -json))
                .arg(arg!(-p - -pretty))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("tree")
                .about("Display a file tree of a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(-j - -json))
                .arg(arg!(-p - -pretty))
                .arg(arg!(-n - -noids))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("unpack")
                .about("unpack a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(-o - -out[OUT]))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("remove")
                .about("Remove a file from a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(file: <FILE>))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("add")
                .about("Add a file to a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(file: <FILE>))
                .arg(arg!(new_path: [NEW_PATH]))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(arg!(-p - -pad[PAD]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
        .subcommand(
            Command::new("rebuild")
                .about("Rebuild a LURCAR")
                .arg(arg!(archive_path: <ARCHIVE>))
                .arg(arg!(-d - -dir[DIR_PATH]))
                .arg(
                    Arg::new("dir_format")
                        .long("dirformat")
                        .short('f')
                        .value_parser(clap::value_parser!(DirFormat)),
                ),
        )
}

pub fn main() {
    let matches = cli().get_matches();
    match matches.subcommand() {
        Some(("info", sub_matches)) => {
            let (mut archive, _) = open_archive(sub_matches);

            let file_entry_path = sub_matches
                .get_one::<String>("file")
                .expect("Couldn't get file from args")
                .clone();
            let (entry_id, entry) = archive
                .entry_data_by_path(PathBuf::from(file_entry_path))
                .expect("Couldn't get the file from the archive")
                .expect("File not found in the archive");

            let extra_fields = match &entry {
                lurcar::DirectoryEntry::File(f) => f.extra_fields.clone(),
                lurcar::DirectoryEntry::Dir(d) => d.extra_fields.clone(),
            };
            let extra_fields = extra_fields
                .into_iter()
                .map(|(name, data)| match name.as_str() {
                    "root_name" => (name, String::from_utf8_lossy(&data).to_string()),
                    _ => (name, base64::prelude::BASE64_STANDARD.encode(data)),
                })
                .collect();

            let entry = match entry {
                lurcar::DirectoryEntry::File(f) => EntryInfo {
                    id: entry_id,
                    _type: EntryType::File,
                    pad: Some(f.pad),
                    len: Some(f.len),
                    checksum: f.checksum,
                    name: f.name,
                    extra_fields,
                    extra_set: f.extra_set,
                    parent: Some(f.parent),
                    unallocated_space_after: Some(f.unallocated_space_after),
                },
                lurcar::DirectoryEntry::Dir(d) => EntryInfo {
                    id: entry_id,
                    _type: EntryType::Directory,
                    pad: None,
                    len: None,
                    checksum: None,
                    name: d.name,
                    extra_fields,
                    extra_set: d.extra_set,
                    parent: d.parent,
                    unallocated_space_after: None,
                },
            };
            if sub_matches.get_flag("json") {
                if sub_matches.get_flag("pretty") {
                    serde_json::to_writer_pretty(stdout().lock(), &entry)
                        .expect("Couldn't encode info");
                } else {
                    serde_json::to_writer(stdout().lock(), &entry).expect("Couldn't encode info");
                }
            } else {
                serde_yaml::to_writer(stdout().lock(), &entry).expect("Couldn't encode info");
            }
            println!();
        }
        Some(("read", sub_matches)) => {
            let (mut archive, _) = open_archive(sub_matches);

            let file_entry_path = sub_matches
                .get_one::<String>("file")
                .expect("Couldn't get file from args")
                .clone();
            let mut entry = archive
                .entry_by_path(PathBuf::from(file_entry_path))
                .expect("Couldn't get the file from the archive")
                .expect("File not found in the archive");
            let start: u64 = sub_matches
                .get_one::<String>("start")
                .unwrap_or(&"0".to_string())
                .parse::<u64>()
                .expect("Start wrong");
            let end: u64 = sub_matches
                .get_one::<String>("end")
                .unwrap_or(&entry.len().to_string())
                .parse::<u64>()
                .expect("End wrong");
            entry
                .seek(std::io::SeekFrom::Start(start))
                .expect("Couldn't seek to start of file");
            std::io::copy(&mut entry.take(end - start), &mut stdout().lock())
                .expect("Couldn't copy into stdout");
        }
        Some(("pack", sub_matches)) => {
            let source = sub_matches
                .get_one::<String>("source")
                .expect("Couldn't get source from args")
                .clone();
            let arc_path = sub_matches
                .get_one::<String>("out")
                .map(|p| p.clone())
                .unwrap_or_else(|| {
                    let source_path =
                        std::fs::canonicalize(&source).expect("Couldn't canonicalize dir path");
                    format!(
                        "{}.lurcar",
                        source_path
                            .file_name()
                            .expect("Couldn't get dir name")
                            .to_string_lossy()
                    )
                })
                .clone();
            let w = File::create(&arc_path).expect("Couldn't open output file");
            let mut archive = Archive::new(w).expect("Couldn't open archive");

            let root_name = if let Ok(path) = std::fs::canonicalize(&source) {
                let filename = path
                    .file_name()
                    .expect("Couldn't get dir name")
                    .to_string_lossy();
                Some(filename.to_string())
            } else {
                None
            };

            let multi_proogress = MultiProgress::new();
            let mut len = 0;
            let mut entry_count = 0;
            let len_spinner = multi_proogress.add(ProgressBar::new_spinner());
            let lens = walkdir::WalkDir::new(&source)
                .into_iter()
                .filter_map(|e| e.ok())
                .map(|e| {
                    len_spinner.inc(1);
                    e.metadata()
                })
                .filter_map(|e| e.ok())
                .map(|e| e.len());
            for _len in lens {
                len += _len;
                entry_count += 1;
            }
            len_spinner.finish();
            let entry_progress_bar = multi_proogress.add(ProgressBar::new(entry_count));
            let data_progress_bar = multi_proogress.add(ProgressBar::new(len)
                .with_style(ProgressStyle::with_template(
                    "[{elapsed_precise}] [{eta}] {wide_bar} {bytes}/{total_bytes} ({percent_precise}%)"
                ).unwrap()));
            if let Some(root_name) = root_name {
                if let Some(entry) = archive.directory_mut().dir_entries.get_mut(&0) {
                    entry
                        .extra_fields
                        .insert("root_name".to_string(), root_name.as_bytes().to_vec());
                }
            }
            pack_folder(
                &mut archive,
                PathBuf::from(&source),
                0,
                &entry_progress_bar,
                &data_progress_bar,
            );

            if let Some((dir_path, dir_format)) = resolve_dir(sub_matches) {
                let (dir, _) = archive
                    .finalize_extern_dir()
                    .expect("Couldn't finalize archive");
                let mut f = File::create(dir_path).expect("Couldn't create directory file");
                match dir_format {
                    DirFormat::Bitcode => f
                        .write_all(&bitcode::encode(&dir))
                        .expect("Couldn't write directory"),
                    DirFormat::Json => {
                        serde_json::to_writer(&mut f, &dir).expect("Couldn't write directory")
                    }
                }
            } else {
                archive
                    .finalize_with_dir()
                    .expect("Couldn't finalize archive");
            }
        }
        Some(("unpack", sub_matches)) => {
            let (mut archive, arc_path) = open_archive(sub_matches);
            let tree = archive.tree();

            let path = if let Some(path) = sub_matches.get_one::<String>("out") {
                PathBuf::from(path)
            } else if let Some(root_name) = tree.entry.extra_fields.get("root_name") {
                PathBuf::from(String::from_utf8_lossy(&root_name).to_string())
            } else {
                let arc_filename = arc_path
                    .file_name()
                    .expect("Couldn't get archive filename")
                    .to_string_lossy()
                    .to_string();
                PathBuf::from(
                    arc_filename
                        .split_once(".")
                        .expect("Couldn't establish an extraction point")
                        .0,
                )
            };

            std::fs::create_dir_all(&path).expect("Couldn't create directory");

            unpack_tree(&tree, &mut archive, path)
        }
        Some(("tree", sub_matches)) => {
            let (archive, _) = open_archive(sub_matches);

            let tree = archive.tree();
            if sub_matches.get_flag("json") {
                if sub_matches.get_flag("pretty") {
                    serde_json::to_writer_pretty(stdout().lock(), &tree)
                        .expect("Couldn't send entries");
                } else {
                    serde_json::to_writer(stdout().lock(), &tree).expect("Couldn't send entries");
                }
                println!();
            } else {
                if let Some(root_name) = tree.entry.extra_fields.get(&"root_name".to_string()) {
                    let root_name = root_name.clone();
                    let root_name = String::from_utf8_lossy(&root_name).to_string();
                    println!("{} {}", root_name.blue().bold(), "[0]".bold().red());
                } else {
                    println!(
                        "{} {}",
                        tree.entry.name.clone().blue().bold(),
                        "[0]".bold().red()
                    );
                }
                print_tree(
                    &tree,
                    &mut BTreeSet::new(),
                    0,
                    !sub_matches.get_flag("noids"),
                );
            }
        }
        Some(("list", sub_matches)) => {
            let (archive, _) = open_archive(sub_matches);
            if sub_matches.get_flag("json") {
                if sub_matches.get_flag("pretty") {
                    serde_json::to_writer_pretty(stdout().lock(), &archive.directory())
                        .expect("Couldn't send entries");
                } else {
                    serde_json::to_writer(stdout().lock(), &archive.directory())
                        .expect("Couldn't send entries");
                }
                println!();
            } else {
                if sub_matches.get_flag("pretty") {
                    let tree = archive.tree();
                    println!("{}", tree.entry.name);
                    list_tree(&tree, PathBuf::from("/"));
                } else {
                    let directory = archive.directory();
                    if directory.unallocated_space > 0 {
                        println!(
                            "{}",
                            AnsiColor::redb(format!(
                                "Unallocated Space: {}",
                                directory.unallocated_space
                            ))
                        );
                    }
                    for (eid, e) in directory.file_entries.iter().enumerate() {
                        let path = entry_to_path(&directory, eid as u32);
                        println!("{}", path.display());
                        if e.unallocated_space_after > 0 {
                            println!(
                                "{}",
                                AnsiColor::redb(format!(
                                    "Unallocated Space: {}",
                                    e.unallocated_space_after
                                ))
                            );
                        }
                    }
                }
            }
        }
        Some(("remove", sub_matches)) => {
            let (mut archive, _) = open_archive(sub_matches);
            let file_entry_path = sub_matches
                .get_one::<String>("file")
                .expect("Couldn't get file from args")
                .clone();
            archive
                .remove_entry_by_path(PathBuf::from(file_entry_path))
                .expect("Couldn't remove entry");
            if let Some((dir_path, dir_format)) = resolve_dir(sub_matches) {
                let (dir, _) = archive
                    .finalize_extern_dir()
                    .expect("Couldn't finalize archive");
                let mut f = File::create(dir_path).expect("Couldn't create directory file");
                match dir_format {
                    DirFormat::Bitcode => f
                        .write_all(&bitcode::encode(&dir))
                        .expect("Couldn't write directory"),
                    DirFormat::Json => {
                        serde_json::to_writer(&mut f, &dir).expect("Couldn't write directory")
                    }
                }
            } else {
                archive
                    .finalize_with_dir()
                    .expect("Couldn't finalize archive");
            }
        }
        Some(("add", sub_matches)) => {
            let (mut archive, _) = open_archive(sub_matches);
            let file_path = PathBuf::from(
                sub_matches
                    .get_one::<String>("file")
                    .expect("Couldn't get file from args"),
            );
            let original_filename = file_path
                .file_name()
                .expect("Couldn't get file filename")
                .to_string_lossy()
                .to_string();
            let new_path = PathBuf::from(
                sub_matches
                    .get_one::<String>("new_path")
                    .unwrap_or(&original_filename),
            );
            let new_filename = new_path
                .file_name()
                .expect("Couldn't get new filename")
                .to_string_lossy()
                .to_string();
            let len = std::fs::metadata(&file_path)
                .expect("Couldn't get file metadata")
                .len();
            archive
                .append_file_entry(
                    new_filename,
                    FileEntryBuilder::new(File::open(&file_path).expect("Couldn't open file"))
                        .len(len)
                        .pad(*sub_matches.get_one::<u64>("pad").unwrap_or(&DEFAULT_PAD)),
                )
                .expect("Couldn't append file entry");
            if let Some((dir_path, dir_format)) = resolve_dir(sub_matches) {
                let (dir, _) = archive
                    .finalize_extern_dir()
                    .expect("Couldn't finalize archive");
                let mut f = File::create(dir_path).expect("Couldn't create directory file");
                match dir_format {
                    DirFormat::Bitcode => f
                        .write_all(&bitcode::encode(&dir))
                        .expect("Couldn't write directory"),
                    DirFormat::Json => {
                        serde_json::to_writer(&mut f, &dir).expect("Couldn't write directory")
                    }
                }
            } else {
                archive
                    .finalize_with_dir()
                    .expect("Couldn't finalize archive");
            }
        }
        Some(("rebuild", sub_matches)) => {
            let (mut archive, _) = open_archive(sub_matches);
            let arc_path = sub_matches
                .get_one::<String>("archive_path")
                .expect("Couldn't get archive path from args")
                .clone();
            archive
                .rebuild_with_readhead(&mut File::open(arc_path).expect("Couldn't open read head"))
                .expect("Couldn't rebuild");
            if let Some((dir_path, dir_format)) = resolve_dir(sub_matches) {
                let (dir, w) = archive
                    .finalize_extern_dir()
                    .expect("Couldn't finalize archive");
                let mut f = File::create(dir_path).expect("Couldn't create directory file");
                match dir_format {
                    DirFormat::Bitcode => f
                        .write_all(&bitcode::encode(&dir))
                        .expect("Couldn't write directory"),
                    DirFormat::Json => {
                        serde_json::to_writer(&mut f, &dir).expect("Couldn't write directory")
                    }
                }
                w.set_len(Archive::<()>::compute_full_archive_len_from_dir(&dir))
                    .expect("Couldn't set new length of file");
            } else {
                let (f, dir) = archive
                    .finalize_with_dir()
                    .expect("Couldn't finalize archive");
                f.set_len(Archive::<()>::compute_full_archive_len_from_dir(&dir))
                    .expect("Couldn't set new length of file");
            }
        }
        _ => unreachable!(),
    }
}
#[derive(Debug, Clone, Serialize)]
struct EntryInfo {
    id: u32,
    #[serde(rename = "type")]
    _type: EntryType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pad: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    len: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    checksum: Option<u32>,
    name: String,
    extra_fields: HashMap<String, String>,
    extra_set: HashSet<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    parent: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    unallocated_space_after: Option<u64>,
}
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum EntryType {
    Directory,
    File,
}
fn print_tree(
    archive_tree: &ArchiveTree,
    last_depths: &mut BTreeSet<usize>,
    depth: usize,
    ids: bool,
) {
    let mut len = archive_tree.files.len() + archive_tree.dirs.len();

    for (id, file) in archive_tree.files.iter() {
        len -= 1;
        print!("{} {}", tree_ident(depth, last_depths, len == 0), file.name);
        if ids {
            println!(" {}", format!("[{id}]").red().bold());
        } else {
            println!();
        }
    }
    for dir in archive_tree.dirs.iter() {
        len -= 1;
        print!(
            "{} {}",
            tree_ident(depth, last_depths, len == 0),
            dir.entry.name.clone().bold().blue()
        );
        if ids {
            println!(" {}", format!("[{}]", dir.id).red().bold());
        } else {
            println!();
        }
        if len != 0 {
            last_depths.insert(depth);
        }
        print_tree(dir, last_depths, depth + 1, ids);
        if len != 0 {
            last_depths.remove(&(depth));
        }
    }
}
fn entry_to_path(dir: &Directory, id: u32) -> PathBuf {
    let e = &dir.file_entries[id as usize];
    let mut parent = e.parent;
    let mut path = PathBuf::from(&e.name);
    loop {
        let parent_entry = &dir.dir_entries[&parent];
        path = PathBuf::from(&parent_entry.name).join(path);
        if let Some(_parent) = parent_entry.parent {
            parent = _parent;
        } else {
            break;
        }
    }
    path
}
fn tree_ident(depth: usize, last_depths: &mut BTreeSet<usize>, is_last: bool) -> String {
    let mut ident = String::new();
    for i in 0..depth {
        if last_depths.contains(&i) {
            ident.push_str("│   ");
        } else {
            ident.push_str("    ");
        }
    }
    if is_last {
        ident.push_str("└")
    } else {
        ident.push_str("├")
    }
    ident.push_str(&"─".repeat(2));
    ident
}
fn list_tree(archive_tree: &ArchiveTree, relative_path: PathBuf) {
    let s = if relative_path == PathBuf::from("/") {
        ""
    } else {
        "/"
    };
    for (_, file) in archive_tree.files.iter() {
        println!("{}{s}{}", relative_path.display(), file.name);
    }
    for dir in archive_tree.dirs.iter() {
        println!("{}{s}{}/", relative_path.display(), dir.entry.name);
        let dir_name = dir.entry.name.clone();
        list_tree(dir, relative_path.join(dir_name));
    }
}
fn unpack_tree<I: Read + Seek>(
    archive_tree: &ArchiveTree,
    archive: &mut Archive<I>,
    path: PathBuf,
) {
    unpack_dirs(archive_tree, path.clone());

    for (entry_id, file) in archive_tree.files.iter() {
        let mut entry = archive
            .entry(*entry_id)
            .expect("Couldn't open entry")
            .unwrap();
        std::io::copy(
            &mut entry,
            &mut File::create(path.join(&file.name)).expect("Couldn't open file"),
        )
        .expect("Couldn't write entry to file");
    }

    for dir in archive_tree.dirs.iter() {
        let dir_path = path.join(&dir.entry.name);
        unpack_tree(dir, archive, dir_path);
    }
}

fn unpack_dirs(archive_tree: &ArchiveTree, path: PathBuf) {
    for dir in archive_tree.dirs.iter() {
        let dir_name = dir.entry.name.clone();
        let dir_path = path.join(dir_name);
        std::fs::create_dir(&dir_path).expect("Couldn't create directory");
        unpack_dirs(dir, dir_path);
    }
}

fn open_archive(sub_matches: &ArgMatches) -> (Archive<File>, PathBuf) {
    let arc_path = sub_matches
        .get_one::<String>("archive_path")
        .expect("Couldn't get archive path from args")
        .clone();
    let arc_path = PathBuf::from(arc_path);
    let arc_file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(&arc_path)
        .expect("Couldn't open file");
    (
        if let Some((dir_path, dir_format)) = resolve_dir(sub_matches) {
            let dir_data = std::fs::read(dir_path).expect("Couldn't read dirfile");
            let dir: Directory = match dir_format {
                DirFormat::Bitcode => bitcode::decode(&dir_data).expect("Couldn't decode dir data"),
                DirFormat::Json => {
                    serde_json::from_slice(&dir_data).expect("Couldn't decode dir data")
                }
            };
            Archive::open_extern_dir(arc_file, dir)
        } else {
            Archive::open_internal_dir(arc_file)
        }
        .expect("Couldn't open archive"),
        arc_path,
    )
}

fn resolve_dir(matches: &ArgMatches) -> Option<(PathBuf, DirFormat)> {
    if let Some(dir_path) = matches.get_one::<String>("dir") {
        let dir_format = matches
            .get_one::<DirFormat>("dir_format")
            .map(|df| *df)
            .unwrap_or_default();
        Some((PathBuf::from(dir_path), dir_format))
    } else {
        None
    }
}

#[derive(Debug, ValueEnum, Clone, Copy, Default)]
#[clap(rename_all = "snake_case")]
pub enum DirFormat {
    #[default]
    Bitcode,
    Json,
}
fn pack_folder(
    arc: &mut Archive<File>,
    dir_path: PathBuf,
    dir_id: u32,
    entry_progress_bar: &ProgressBar,
    data_progress_bar: &ProgressBar,
) {
    for file in std::fs::read_dir(dir_path).expect("Couldn't read dir") {
        if let Ok(file) = file {
            let Ok(file_type) = file.file_type() else {
                continue;
            };
            let filename = file.file_name();
            let filename = filename.to_string_lossy();
            if file_type.is_dir() {
                let dir_id = arc
                    .append_dir_entry(filename.to_string(), DirEntryBuilder::new().parent(dir_id))
                    .expect(&format!(
                        "Couldn't append archive entry: [{}]",
                        file.path().display()
                    ));
                entry_progress_bar.inc(1);
                entry_progress_bar.println(file.path().to_string_lossy());
                pack_folder(
                    arc,
                    file.path(),
                    dir_id,
                    entry_progress_bar,
                    data_progress_bar,
                );
            } else {
                entry_progress_bar.inc(1);
                entry_progress_bar.println(file.path().to_string_lossy());
                let f = std::fs::File::open(file.path()).expect("Couldn't read file");
                arc.append_file_entry(
                    filename.to_string(),
                    FileEntryBuilder::new(ProgressTrackingReader(f, data_progress_bar))
                        .parent(dir_id)
                        .pad(DEFAULT_PAD),
                )
                .expect(&format!(
                    "Couldn't append archive entry: [{}]",
                    file.path().display()
                ));
            }
        }
    }
}
struct ProgressTrackingReader<'a, R>(R, &'a ProgressBar);
impl<'a, R: Read> Read for ProgressTrackingReader<'a, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let read = self.0.read(buf)?;
        self.1.inc(read as u64);
        Ok(read)
    }
}
