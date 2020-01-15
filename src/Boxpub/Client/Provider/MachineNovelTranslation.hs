module Boxpub.Client.Provider.MachineNovelTranslation
( config ) where
  import Data.Text as T
  import Text.HTML.Scalpel
  import Boxpub.Client.ProviderType ( ProviderConfig(..) )
  import qualified Boxpub.Client.ProviderType as B

  novelBaseURL :: Text
  novelBaseURL = "https://machinenoveltranslation.com/%s"

  novelTitle :: Scraper Text Text
  novelTitle = chroot ("div" @: [ hasClass "desc" ]) $ text "h4"

  novelAuthor :: Scraper Text Text
  novelAuthor = text $ "div" @: [ hasClass "NA_" ]

  coverImage :: Scraper Text Text
  coverImage = chroot ("div" @: [ hasClass "about-author" ]) $ attr "src" "img"

  chapterList :: Scraper Text [Text]
  chapterList = chroots ("ul" @: [ hasClass "navigate-page" ] // "li") $ attr "href" "a"

  chapterName :: Scraper Text Text
  chapterName = chroot ("div" @: [ hasClass "about-author" ] // "div" @: [ hasClass "row" ]) $ text "h5"

  chapterContents :: Scraper Text Text
  chapterContents = innerHTML $ "div" @: [ hasClass "desc" ]

  config :: ProviderConfig
  config = ProviderConfig
    { novelPath = novelBaseURL
    , fetchMetadata = B.defaultFetchMetadata novelTitle coverImage novelAuthor
    , fetchChapter = B.defaultFetchChapter chapterName chapterContents
    , fetchChapterList = B.defaultFetchChapterList chapterList }
