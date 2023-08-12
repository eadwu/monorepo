use std::sync::Mutex;

use filemanager::FileManager;
use once_cell::sync::Lazy;

pub mod primitives;

const FILE_CACHE_SIZE: usize = 1024;
static FILE_MANAGER: Lazy<Mutex<FileManager>> =
    Lazy::new(|| Mutex::new(FileManager::new(FILE_CACHE_SIZE)));
