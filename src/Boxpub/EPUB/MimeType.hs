module Boxpub.EPUB.MimeType
( generateMimeType ) where
  import System.FilePath ( (</>) )

  mimeType :: String
  mimeType = "application/epub+zip"

  generateMimeType :: FilePath -> IO ()
  generateMimeType root = writeFile (root </> "mimetype") mimeType
