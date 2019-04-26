module Boxpub.Client.Provider
( Chapter(..), Metadata(..), ProviderEnv(..)
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

  -- "Safe" variant of !! by @missingfaktor
  -- https://stackoverflow.com/questions/8861101/haskell-how-to-create-a-function-that-returns-the-fifth-element-from-a-list
  (!!!) :: [a] -> Int -> Maybe a
  -- [a] == []
  [] !!! _ = Nothing
  -- n > [a].length
  xs !!! n | n < 0 = Nothing
  -- [a] !! n
  (x:_) !!! 0 = Just x
  (_:xs) !!! n = xs !!! (n - 1)

  data Chapter = Chapter
    { name :: Text
    , content :: Text }

  data Metadata = Metadata
    { title :: Text
    , cover :: Text
    , author :: Text }

  data ProviderEnv = ProviderEnv
    { metadata :: Metadata
    , chapterList :: [Text] }

  customManagerSettings :: ManagerSettings
  customManagerSettings = tlsManagerSettings

  req :: URL -> Scraper Text a -> IO (Maybe a)
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

  fetchChapter :: Env -> ProviderEnv -> Int -> IO Chapter
  fetchChapter env pEnv chapterN = do
    chapterName <- reqChapter BoxNovel.chapterName
    chapterContents <- reqChapter BoxNovel.chapterContents
    -- fromMaybe shouldn't be required here anymore but the overhead isn't much anyway
    return Chapter
      { name = strip $ fromMaybe "INVALID_CHAPTER" chapterName
      , content = strip $ fromMaybe "INVALID_CHAPTER" chapterContents }
    where
      -- If fromJust errors out, it means that the chapter [`number`] requested
      -- Otherwise adjust for 0-based indexes
      chapterURL = unpack $ fromJust $ chapterList pEnv !!! (chapterN - 1)
      reqChapter = req chapterURL

  mkEnv :: Env -> IO ProviderEnv
  mkEnv env = do
    metadata <- fetchMetadata novelURL BoxNovel.novelTitle BoxNovel.coverImage BoxNovel.novelAuthor
    chapterList <- req novelURL BoxNovel.chapterList
    return ProviderEnv
      { metadata = metadata
      -- Might as well just quit if Nothing is returned, no chapters are found anyway
      -- TODO: Fix this hard coded workaround explicitly for BoxNovel (`reverse`)
      , chapterList = Prelude.reverse $ fromJust chapterList }
    where
      novel = fromJust $ (B.novel . options) env
      novelURL = printf (unpack BoxNovel.novelPath) novel
