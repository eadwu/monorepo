module Boxpub.Client.Provider.NovelPlanet
( config ) where
  import Data.Text as T
  import Data.Maybe ( fromJust )
  import Text.HTML.Scalpel
  import Boxpub.Client.ProviderType ( Metadata(..), ProviderConfig(..) )
  import qualified Boxpub.Client.ProviderType as B

  novelBaseURL :: Text
  novelBaseURL = "https://novelplanet.com/Novel/%s"

  novelTitle :: Scraper Text Text
  novelTitle = text $ "a" @: [ hasClass "title" ]

  coverImage :: Scraper Text Text
  coverImage = chroot ("div" @: [ hasClass "post-previewInDetails" ]) $ attr "src" "img"

  chapterList :: Scraper Text [Text]
  chapterList = chroots ("div" @: [ hasClass "rowChapter" ]) $ attr "href" "a"

  chapterName :: Scraper Text Text
  chapterName = chroot ("div" @: [ hasClass "container" ]) $ text "h4"

  chapterContents :: Scraper Text Text
  chapterContents = innerHTML $ "div" @: [ hasClass "divReadContent" ]

  config :: ProviderConfig
  config = ProviderConfig
    { novelPath = novelBaseURL
    , fetchMetadata = \url -> do
      title <- B.req url novelTitle
      cover <- B.req url coverImage
      return Metadata
        { title = strip $ fromJust title
        , cover = T.concat [ "https://novelplanet.com", strip $ fromJust cover ]
        , author = "UNKNOWN" }
    , fetchChapter = B.defaultFetchChapter chapterName chapterContents
    , fetchChapterList = \url -> do
      cL <- B.defaultFetchChapterList chapterList url
      return $ Prelude.map (\path -> T.concat [ "https://novelplanet.com", path ]) cL }
