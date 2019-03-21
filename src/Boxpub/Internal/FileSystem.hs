module Boxpub.Internal.FileSystem
( mkdirp ) where
  import System.Directory ( createDirectoryIfMissing )

  mkdirp :: FilePath -> IO ()
  mkdirp = createDirectoryIfMissing True
