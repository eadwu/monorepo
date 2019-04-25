module Boxpub.Client.Provider.BoxNovel
( novelPath
, novelTitle, novelAuthor, coverImage, chapterList, chapterName, chapterContents ) where
  import Data.Text as T
  import Text.HTML.Scalpel

  protocol :: Text
  protocol = "https"

  domain :: Text
  domain = "boxnovel.com"

  novelPath :: Text
  novelPath = T.concat [ protocol, "://", domain, "/novel/%s" ]

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
