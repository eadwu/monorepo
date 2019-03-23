module Boxpub.Client.Provider
( Metadata(..)
, main ) where
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Data.Default ( def )
  import Data.Maybe ( fromJust ) -- fromJust errors with an Exception when value is `Nothing`
  import Network.HTTP.Client ( Manager, ManagerSettings )
  import Network.HTTP.Client.TLS ( newTlsManagerWith, tlsManagerSettings )
  import Text.HTML.Scalpel ( URL, Scraper, Config(..), scrapeURLWithConfig )
  import Text.Printf ( printf )
  import qualified Boxpub.Client.Provider.BoxNovel as BoxNovel

  data Chapter = Chapter
    { name :: String
    , content :: String }

  data Metadata = Metadata
    { title :: String
    , cover :: String
    , author :: String }

  customManagerSettings :: ManagerSettings
  customManagerSettings = tlsManagerSettings

  -- In theory, this should be
  -- URL -> Scraper String a -> IO (Maybe a)
  req :: URL -> Scraper String String -> IO (Maybe String)
  req url scraper = do
    manager <- Just <$> newTlsManagerWith customManagerSettings
    scrapeURLWithConfig (def { manager = manager }) url scraper

  fetchMetadata :: URL -> Scraper String String -> Scraper String String -> Scraper String String -> IO Metadata
  fetchMetadata url titleLayout coverLayout authorLayout = do
    title <- req url titleLayout
    cover <- req url coverLayout
    author <- req url authorLayout
    return Metadata
      { title = fromJust title
      , cover = fromJust cover
      , author = fromJust author }

  -- https://www.fpcomplete.com/blog/2013/06/haskell-from-c
  fetchChapter :: Env -> BoxNovel.BoxNovelEnv -> Integer -> IO Chapter
  fetchChapter env pEnv chapterN = do
    chapterName <- reqChapter BoxNovel.chapterName
    chapterContents <- reqChapter BoxNovel.chapterContents
    return Chapter
      { name = fromJust chapterName
      , content = fromJust chapterContents }
    where
      createChapterURL env = printf (BoxNovel.getRootPath env ++ BoxNovel.getChapterPath env)
      reqChapter = req (createChapterURL pEnv (fromJust $ (novel . options) env) chapterN)

  main :: Env -> IO ()
  main env = putStrLn "Hello World"
