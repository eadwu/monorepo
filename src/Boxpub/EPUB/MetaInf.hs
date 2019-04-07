module Boxpub.EPUB.MetaInf
( generateMetaInf ) where
  import Boxpub.EPUB.Constants ( epubSubDirectory, metaInfSubDirectory )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import System.FilePath ( (</>) )
  import Text.Printf ( printf )

  container :: String
  container = printf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">\
  \  <rootfiles>\
  \    <rootfile full-path=\"%s/content.opf\" media-type=\"application/oebps-package+xml\" />\
  \  </rootfiles>\
  \</container>" epubSubDirectory

  ibooksDisplayOptions :: String
  ibooksDisplayOptions = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<display_options>\
  \  <platform name=\"*\">\
  \    <option name=\"specified-fonts\">true</option>\
  \  </platform>\
  \</display_options>"

  generateMetaInf root = do
    mkdirp metaInf
    writeFile (metaInf </> "com.apple.ibooks.display-options.xml") ibooksDisplayOptions
    writeFile (metaInf </> "container.xml") container
    where
      metaInf = root </> metaInfSubDirectory
