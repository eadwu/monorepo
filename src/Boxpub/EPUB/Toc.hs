module Boxpub.EPUB.Toc
( generateToc ) where
  import Boxpub.EPUB.Constants ( epubSubDirectory )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import System.FilePath ( (</>) )
  import Text.Printf ( printf )

  -- chapter #, chapter name, href
  -- TODO: Check if navPoint.id is needed...
  navPoint :: Int -> String -> String -> String
  navPoint = printf "<navPoint id=\"navPoint-%d\">\
  \  <navLabel>\
  \    <text>%s</text>\
  \  </navLabel>\
  \  <content src=\"%s\" />\
  \</navPoint>"

  -- uuid, title, navPoint(s)
  toc :: String -> String -> String -> String
  toc = printf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<ncx version=\"2005-1\" xmlns=\"http://www.daisy.org/z3986/2005/ncx/\">\
  \  <head>\
  \    <meta name=\"dtb:uid\" content=\"urn:uuid:%s\" />\
  \    <meta name=\"dtb:depth\" content=\"1\" />\
  \    <meta name=\"dtb:totalPageCount\" content=\"0\" />\
  \    <meta name=\"dtb:maxPageNumber\" content=\"0\" />\
  \  </head>\
  \  <docTitle>\
  \    <text>%s</text>\
  \  </docTitle>\
  \  <navMap>\
  \    %s\
  \  </navMap>\
  \</ncx>"

  generateToc :: FilePath -> IO ()
  generateToc root = do
    mkdirp epubDirectory
    where
      epubDirectory = root </> epubSubDirectory
