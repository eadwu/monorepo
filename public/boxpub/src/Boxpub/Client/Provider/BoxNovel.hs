module Boxpub.Client.Provider.BoxNovel
( Paths(..)
, BoxNovelEnv(..)
, mkEnv
, getRootPath, getNovelPath, getChapterPath
, novelTitle, coverImage, chapterName, chapterContents ) where
  import Text.HTML.Scalpel ( Scraper, TagName(..), (@:), attr, text, chroot, hasClass, innerHTML, tagSelector )

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

  novelTitle :: Scraper String String
  novelTitle = text $ tagSelector "title"

  coverImage :: Scraper String String
  coverImage = chroot (TagString "div" @: [ hasClass "summary_image" ]) $ attr "src" (tagSelector "img")

  chapterName :: Scraper String String
  chapterName = text $ TagString "li" @: [ hasClass "active" ]

  chapterContents :: Scraper String String
  chapterContents = innerHTML $ TagString "div" @: [ hasClass "text-left" ]

  sanitize :: String -> String
  sanitize str = str
