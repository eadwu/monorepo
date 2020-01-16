module Boxpub.Client.ProviderType
( Chapter(..), Metadata(..)
, ProviderEnv(..), ProviderConfig(..)
, req, defaultFetchMetadata, defaultFetchChapter, defaultFetchChapterList ) where
  import Data.Text as T
  import Data.Maybe ( fromJust, fromMaybe )
  import Network.HTTP.Client ( ManagerSettings )
  import Network.HTTP.Client.TLS ( newTlsManagerWith, tlsManagerSettings )
  import Text.HTML.Scalpel ( URL, Scraper, Config(..), utf8Decoder, scrapeURLWithConfig )
  import Text.Printf ( printf )

  data Chapter = Chapter
    { name :: Text
    , content :: Text }

  data Metadata = Metadata
    { title :: Text
    , cover :: Text
    , author :: Text }

  data ProviderConfig = ProviderConfig
    { novelPath :: Text
    , fetchMetadata :: URL -> IO Metadata
    , fetchChapter :: URL -> IO Chapter
    , fetchChapterList :: URL -> IO [Text] }

  data ProviderEnv = ProviderEnv
    { url :: URL
    , provider :: ProviderConfig
    , metadata :: Metadata
    , chapterList :: [Text] }

  customManagerSettings :: ManagerSettings
  customManagerSettings = tlsManagerSettings

  req :: URL -> Scraper Text a -> IO (Maybe a)
  req url scraper = do
    manager <- Just <$> newTlsManagerWith customManagerSettings
    scrapeURLWithConfig (Config { decoder = utf8Decoder, manager = manager }) url scraper

  defaultFetchMetadata :: Scraper Text Text -> Scraper Text Text -> Scraper Text Text -> URL -> IO Metadata
  defaultFetchMetadata titleLayout coverLayout authorLayout url = do
    title <- req url titleLayout
    cover <- req url coverLayout
    author <- req url authorLayout
    return Metadata
      { title = strip $ fromJust title
      , cover = strip $ fromJust cover
      , author = strip $ fromJust author }

  defaultFetchChapter :: Scraper Text Text -> Scraper Text Text -> URL -> IO Chapter
  defaultFetchChapter chapterName chapterContents chapterURL = do
    name <- req chapterURL chapterName
    contents <- req chapterURL chapterContents
    -- fromMaybe shouldn't be required here anymore but the overhead isn't much anyway
    return Chapter
      { name = strip $ fromMaybe "INVALID_CHAPTER" name
      , content = strip $ fromMaybe "INVALID_CHAPTER" contents }

  defaultFetchChapterList :: Scraper Text [Text] -> URL -> IO [Text]
  defaultFetchChapterList chapterList novelURL = do
    cL <- req novelURL chapterList
    return $ fromJust cL
