module Boxpub.Client.Provider.MachineNovelTranslation
( config ) where
  import Data.Text as T
  import Data.Maybe ( fromJust )
  import Text.HTML.Scalpel
  import Boxpub.Client.ProviderType ( Metadata(..), ProviderConfig(..) )
  import qualified Boxpub.Client.ProviderType as B

  novelBaseURL :: Text
  novelBaseURL = "https://machinenoveltranslation.com/%s"

  novelTitle :: Scraper Text Text
  novelTitle = chroot ("div" @: [ hasClass "desc" ]) $ text "h5"

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
    , fetchMetadata = \url -> do
      title <- B.req url novelTitle
      cover <- B.req url coverImage
      return Metadata
        { title = strip $ fromJust title
        , cover = strip $ fromJust cover
        , author = "UNKNOWN" }
    , fetchChapter = B.defaultFetchChapter chapterName chapterContents
    , fetchChapterList = B.defaultFetchChapterList chapterList }
