module Boxpub.Client.Provider
( Provider, Chapter(..), Metadata(..), ProviderEnv(..)
, mkEnv, fetchChapter ) where
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Data.Default ( def )
  import Data.Maybe ( fromJust ) -- fromJust errors with an Exception when value is `Nothing`
  import Data.Text as T ( Text, strip, concat, unpack )
  import Network.HTTP.Client ( Manager, ManagerSettings )
  import Network.HTTP.Client.TLS ( newTlsManagerWith, tlsManagerSettings )
  import Text.HTML.Scalpel ( URL, Scraper, Config(..), scrapeURLWithConfig )
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
    scrapeURLWithConfig (def { manager = manager }) url scraper

  fetchMetadata :: URL -> Scraper Text Text -> Scraper Text Text -> Scraper Text Text -> IO Metadata
  fetchMetadata url titleLayout coverLayout authorLayout = do
    title <- req url titleLayout
    cover <- req url coverLayout
    author <- req url authorLayout
    return Metadata
      { title = T.strip $ fromJust title
      , cover = T.strip $ fromJust cover
      , author = T.strip $ fromJust author }

  -- https://www.fpcomplete.com/blog/2013/06/haskell-from-c
  fetchChapter :: Env -> Provider -> Int -> IO Chapter
  fetchChapter env pEnv chapterN = do
    chapterName <- reqChapter BoxNovel.chapterName
    chapterContents <- reqChapter BoxNovel.chapterContents
    return Chapter
      { name = T.strip $ fromJust chapterName
      , content = T.strip $ fromJust chapterContents }
    where
      createChapterURL env = printf (T.unpack $ T.concat [ BoxNovel.getRootPath env, BoxNovel.getChapterPath env ])
      reqChapter = req (createChapterURL pEnv (fromJust $ (novel . options) env) chapterN)

  mkEnv :: Env -> Provider -> IO ProviderEnv
  mkEnv env pEnv = do
    -- We want the value if its given, which in Haskell is `Just` not `Maybe`
    -- This also does some neat things with partial functions
    -- fetchMetadata(printf(createNovelURL(ProviderEnv), []), Scraper, Scraper)
    metadata <- fetchMetadata (createNovelURL pEnv (fromJust $ (novel . options) env))
      BoxNovel.novelTitle BoxNovel.coverImage BoxNovel.novelAuthor
    return ProviderEnv
      { metadata = metadata }
    where createNovelURL env = printf (T.unpack $ T.concat [ BoxNovel.getRootPath env, BoxNovel.getNovelPath env ])
