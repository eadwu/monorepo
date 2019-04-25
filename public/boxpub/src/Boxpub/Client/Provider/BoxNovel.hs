module Boxpub.Client.Provider.BoxNovel
( Paths(..), BoxNovelEnv(..)
, mkEnv
, getRootPath, getNovelPath, getChapterPath
, novelTitle, novelAuthor, coverImage, chapterList, chapterName, chapterContents ) where
  import Data.Text as T
  import Text.HTML.Scalpel

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
        { root = T.concat [ protocol, "://", domain ]
        , novel = novel
        , chapter = T.concat [ novel, "/chapter-%d" ] }

  getRootPath :: BoxNovelEnv -> Text
  getRootPath = root . paths

  getNovelPath :: BoxNovelEnv -> Text
  getNovelPath = novel . paths

  getChapterPath :: BoxNovelEnv -> Text
  getChapterPath = chapter . paths

  novelTitle :: Scraper Text Text
  novelTitle = text $ "div" @: [ hasClass "post-title" ]

  novelAuthor :: Scraper Text Text
  novelAuthor = text $ "div" @: [ hasClass "author-content" ]

  coverImage :: Scraper Text Text
  coverImage = chroot ("div" @: [ hasClass "summary_image" ]) $ attr "src" "img"

  chapterList :: Scraper Text [Text]
  chapterList = chroot ("ul" @: [ hasClass "version-chap" ]) $ attrs "href" "a"

  chapterName :: Scraper Text Text
  chapterName = text $ "li" @: [ hasClass "active" ]

  chapterContents :: Scraper Text Text
  chapterContents = innerHTML $ "div" @: [ hasClass "text-left" ]
