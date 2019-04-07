module Boxpub.Client.Provider.BoxNovel
( Paths(..), BoxNovelEnv(..)
, mkEnv
, getRootPath, getNovelPath, getChapterPath
, novelTitle, novelAuthor, coverImage, chapterName, chapterContents ) where
  import Data.Text as T
  import Text.HTML.Scalpel
  import Text.HTML.TagSoup

  data Paths = Paths
    { root :: String
    , novel :: String
    , chapter :: String }

  data BoxNovelEnv = BoxNovelEnv
    { paths :: Paths }

  protocol :: String
  protocol = "https"

  domain :: String
  domain = "boxnovel.com"

  mkEnv :: IO BoxNovelEnv
  mkEnv = return BoxNovelEnv
    { paths = paths }
    where
      novel = "/novel/%s"
      paths = Paths
        { root = protocol ++ "://" ++ domain
        , novel = novel
        , chapter = novel ++ "/chapter-%d" }

  getRootPath :: BoxNovelEnv -> String
  getRootPath = root . paths

  getNovelPath :: BoxNovelEnv -> String
  getNovelPath = novel . paths

  getChapterPath :: BoxNovelEnv -> String
  getChapterPath = chapter . paths

  novelTitle :: Scraper Text Text
  novelTitle = text $ tagSelector "title"

  novelAuthor :: Scraper Text Text
  novelAuthor = text $ TagString "div" @: [ hasClass "author-content" ]

  coverImage :: Scraper Text Text
  coverImage = chroot (TagString "div" @: [ hasClass "summary_image" ]) $ attr "src" (tagSelector "img")

  chapterName :: Scraper Text Text
  chapterName = text $ TagString "li" @: [ hasClass "active" ]

  chapterContents :: Scraper Text Text
  chapterContents = innerHTML $ TagString "div" @: [ hasClass "text-left" ]

  sanitize :: Text -> Text
  sanitize str = str
