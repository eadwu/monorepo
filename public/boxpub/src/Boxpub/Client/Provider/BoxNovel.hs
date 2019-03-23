module Boxpub.Client.Provider.BoxNovel
( Paths(..), BoxNovelEnv(..)
, mkEnv
, getRootPath, getNovelPath, getChapterPath
, novelTitle, novelAuthor, coverImage, chapterName, chapterContents ) where
  import Prelude hiding ( concat )
  import Data.Text as Text ( Text, concat )
  import Text.HTML.Scalpel as Scalpel ( Scraper, TagName(..), (@:), attr, text, chroot, hasClass, innerHTML, tagSelector )

  data Paths = Paths
    { root :: Text
    , novel :: Text
    , chapter :: Text }

  data BoxNovelEnv = BoxNovelEnv
    { paths :: Paths }

  protocol :: Text
  protocol = "https"

  domain :: Text
  domain = "boxnovel.com"

  mkEnv :: IO BoxNovelEnv
  mkEnv = return BoxNovelEnv
    { paths = paths }
    where
      novel = "/novel/%s"
      paths = Paths
        { root = concat [ protocol, "://", domain ]
        , novel = novel
        , chapter = concat [ novel, "/chapter-%d" ] }

  getRootPath :: BoxNovelEnv -> Text
  getRootPath = root . paths

  getNovelPath :: BoxNovelEnv -> Text
  getNovelPath = novel . paths

  getChapterPath :: BoxNovelEnv -> Text
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
