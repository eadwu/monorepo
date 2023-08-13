use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use hashlink::LruCache;
use memmap2::{Mmap, MmapOptions};

pub struct FileManager {
    cache: LruCache<PathBuf, File>,
}

impl FileManager {
    pub fn new(cache_size: usize) -> FileManager {
        FileManager {
            cache: LruCache::new(cache_size),
        }
    }

    pub fn open(&mut self, path: &Path, offset: usize) -> io::Result<Mmap> {
        if !self.cache.contains_key(path) {
            let file = File::open(&path)?;
            self.cache.insert(path.to_path_buf(), file);
        }

        let file = self.cache.get(path).unwrap();
        Ok(unsafe { MmapOptions::new().offset(offset as u64).map(file).unwrap() })
    }

    pub fn create(&mut self, path: &Path, size: usize, offset: usize) -> io::Result<Mmap> {
        if !self.cache.contains_key(path) {
            let file = tempfile::tempfile()?;
            file.set_len(size as u64)?;

            self.cache.insert(path.to_path_buf(), file);
        }

        self.open(path, offset)
    }

    pub fn create_with_bytes(&mut self, path: &Path, bytes: &[u8]) -> io::Result<Mmap> {
        let mmap = self.create(path, bytes.len(), 0)?;
        let mut mmap = mmap.make_mut()?;
        mmap[..].copy_from_slice(bytes);

        mmap.make_read_only()
    }

    pub fn close(&mut self, path: &Path) {
        self.cache.remove(path);
    }
}
