module Boxpub.Internal.FileSystem
( mkdirp ) where
  import System.Directory ( createDirectoryIfMissing )

  -- | Creates a directory and its parents if missing.
  mkdirp :: FilePath -> IO ()
  mkdirp = createDirectoryIfMissing True
