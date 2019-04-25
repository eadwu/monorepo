module Boxpub.Client.Provider
( Provider, Chapter(..), Metadata(..), ProviderEnv(..)
, mkEnv, fetchChapter ) where
  import Data.Text as T
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser as B ( BoxpubOptions(..) )
  import Data.Maybe ( fromJust, fromMaybe )
  import Network.HTTP.Client ( ManagerSettings )
  import Network.HTTP.Client.TLS ( newTlsManagerWith, tlsManagerSettings )
  import Text.HTML.Scalpel ( URL, Scraper, Config(..), utf8Decoder, scrapeURLWithConfig )
  import Text.Printf ( printf )
  import qualified Boxpub.Client.Provider.BoxNovel as BoxNovel

  type Provider = BoxNovel.BoxNovelEnv

  data Chapter = Chapter
    { name :: Text
    , content :: Text }

  data Metadata = Metadata
    { title :: Text
    , cover :: Text
    , author :: Text }

  data ProviderEnv = ProviderEnv
    { metadata :: Metadata }

  customManagerSettings :: ManagerSettings
  customManagerSettings = tlsManagerSettings

  -- In theory, this should be
  -- URL -> Scraper Text a -> IO (Maybe a)
  req :: URL -> Scraper Text Text -> IO (Maybe Text)
  req url scraper = do
    manager <- Just <$> newTlsManagerWith customManagerSettings
    scrapeURLWithConfig (Config { decoder = utf8Decoder, manager = manager }) url scraper

  fetchMetadata :: URL -> Scraper Text Text -> Scraper Text Text -> Scraper Text Text -> IO Metadata
  fetchMetadata url titleLayout coverLayout authorLayout = do
    title <- req url titleLayout
    cover <- req url coverLayout
    author <- req url authorLayout
    return Metadata
      { title = strip $ fromJust title
      , cover = strip $ fromJust cover
      , author = strip $ fromJust author }

  fetchChapter :: Env -> Provider -> Int -> IO Chapter
  fetchChapter env pEnv chapterN = do
    chapterName <- reqChapter BoxNovel.chapterName
    chapterContents <- reqChapter BoxNovel.chapterContents
    -- fromMaybe shouldn't be required here anymore but the overhead isn't much anyway
    return Chapter
      { name = strip $ fromMaybe "INVALID_CHAPTER" chapterName
      , content = strip $ fromMaybe "INVALID_CHAPTER" chapterContents }
    where
      novel = fromJust $ (B.novel . options) env
      chapterURL = printf (unpack $ T.concat [ BoxNovel.getRootPath pEnv, BoxNovel.getChapterPath pEnv ]) novel chapterN
      reqChapter = req chapterURL

  mkEnv :: Env -> Provider -> IO ProviderEnv
  mkEnv env pEnv = do
    -- We want the value if its given, which in Haskell is `Just` not `Maybe`
    -- This also does some neat things with partial functions
    -- fetchMetadata(printf(createNovelURL(ProviderEnv), []), Scraper, Scraper)
    metadata <- fetchMetadata (createNovelURL pEnv (fromJust $ (novel . options) env))
      BoxNovel.novelTitle BoxNovel.coverImage BoxNovel.novelAuthor
    return ProviderEnv
      { metadata = metadata }
    where createNovelURL env = printf (unpack $ T.concat [ BoxNovel.getRootPath env, BoxNovel.getNovelPath env ])
