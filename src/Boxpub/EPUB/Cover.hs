module Boxpub.EPUB.Cover
( addCover ) where
  import Boxpub.EPUB.Constants ( mediaSubDirectory )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import System.FilePath ( (</>) )

  addCover :: FilePath -> IO ()
  addCover root = do
    mkdirp mediaDir
    where
      mediaDir = root </> mediaSubDirectory
