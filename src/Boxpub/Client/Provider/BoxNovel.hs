module Boxpub.Client.Provider.BoxNovel
( config ) where
  import Data.Text as T
  import Text.HTML.Scalpel
  import Boxpub.Client.ProviderType ( ProviderConfig(..) )
  import qualified Boxpub.Client.ProviderType as B

  novelBaseURL :: Text
  novelBaseURL = "https://boxnovel.com/novel/%s"

  novelTitle :: Scraper Text Text
  novelTitle = text $ "div" @: [ hasClass "post-title" ]

  novelAuthor :: Scraper Text Text
  novelAuthor = text $ "div" @: [ hasClass "author-content" ]

  coverImage :: Scraper Text Text
  coverImage = chroot ("div" @: [ hasClass "summary_image" ]) $ attr "src" "img"

  chapterList :: Scraper Text [Text]
  chapterList = chroots ("ul" @: [ hasClass "version-chap" ] // "li" @: [ hasClass "wp-manga-chapter" ]) $ attr "href" "a"

  chapterName :: Scraper Text Text
  chapterName = text $ "li" @: [ hasClass "active" ]

  chapterContents :: Scraper Text Text
  chapterContents = innerHTML $ "div" @: [ hasClass "text-left" ]

  config :: ProviderConfig
  config = ProviderConfig
    { novelPath = novelBaseURL
    , fetchMetadata = B.defaultFetchMetadata novelTitle coverImage novelAuthor
    , fetchChapter = B.defaultFetchChapter chapterName chapterContents
    , fetchChapterList = \url -> do
      cL <- B.defaultFetchChapterList chapterList url
      return $ Prelude.reverse cL }
