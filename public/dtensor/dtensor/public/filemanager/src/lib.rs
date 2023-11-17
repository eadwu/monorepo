use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use hashlink::{LinkedHashMap, LruCache};
use memmap2::{Mmap, MmapOptions};
use tempfile::{NamedTempFile, TempPath};

pub struct FileManager {
    cache: LruCache<PathBuf, File>,
    history: LinkedHashMap<PathBuf, TempPath>,
}

impl FileManager {
    pub fn new(cache_size: usize) -> FileManager {
        let cache_size = if cache_size == 0 {
            log::warn!("FileManager cache size must not be 0, transparently increasing to 1");
            1
        } else {
            cache_size
        };

        FileManager {
            cache: LruCache::new(cache_size),
            history: LinkedHashMap::new(),
        }
    }

    pub fn open(&mut self, path: &Path, offset: usize) -> io::Result<Mmap> {
        if !self.cache.contains_key(path) {
            let file_path = self
                .history
                .get(path)
                .map(|x| x.to_path_buf())
                .unwrap_or(path.to_path_buf());
            let file = File::options().read(true).write(true).open(&file_path)?;
            self.cache.insert(file_path, file);
        }

        let file = self.cache.get(path).unwrap();
        Ok(unsafe { MmapOptions::new().offset(offset as u64).map(file).unwrap() })
    }

    pub fn create(&mut self, size: usize) -> io::Result<PathBuf> {
        let file = NamedTempFile::new()?;
        file.as_file().set_len(size as u64)?;

        let file_path = file.into_temp_path();
        let path = file_path.to_path_buf();
        self.history.insert(path.clone(), file_path);
        Ok(path)
    }

    pub fn create_with_bytes(&mut self, bytes: &[u8]) -> io::Result<PathBuf> {
        let file_path = self.create(bytes.len())?;
        let mmap = self.open(&file_path, 0)?;
        let mut mmap = mmap.make_mut()?;
        mmap[..].copy_from_slice(bytes);
        Ok(file_path)
    }

    pub fn close(&mut self, path: &Path) {
        self.cache.remove(path);
        if let Some(temp_path) = self.history.remove(path) {
            let _ = temp_path.close();
        };
    }
}
