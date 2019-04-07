module Boxpub.EPUB.Constants
( uuid
, epubSubDirectory, fontSubDirectory, mediaSubDirectory, styleSubDirectory, chapterSubDirectory
, metaInfSubDirectory ) where
  import Data.UUID ( toString )
  import Data.UUID.V4 ( nextRandom )

  uuid :: IO String
  uuid = toString <$> nextRandom

  -- relative to root
  epubSubDirectory :: String
  epubSubDirectory = "EPUB"

  metaInfSubDirectory :: String
  metaInfSubDirectory = "META-INF"

  -- relative to epubSubDirectory
  fontSubDirectory :: String
  fontSubDirectory = "fonts"

  mediaSubDirectory :: String
  mediaSubDirectory = "media"

  styleSubDirectory :: String
  styleSubDirectory = "styles"

  chapterSubDirectory :: String
  chapterSubDirectory = "text"
