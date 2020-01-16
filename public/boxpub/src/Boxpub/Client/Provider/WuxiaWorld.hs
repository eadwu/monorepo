module Boxpub.Client.Provider.WuxiaWorld
( config ) where
  import Data.Text as T
  import Text.HTML.Scalpel
  import Boxpub.Client.ProviderType ( ProviderConfig(..) )
  import qualified Boxpub.Client.ProviderType as B

  novelBaseURL :: Text
  novelBaseURL = "https://www.wuxiaworld.com/novel/%s"

  novelTitle :: Scraper Text Text
  novelTitle = text "h4"

  novelAuthor :: Scraper Text Text
  novelAuthor = text $ ("div" @: [ hasClass "media-body" ]) // "dd"

  coverImage :: Scraper Text Text
  coverImage = attr "data-cfsrc" ("img" @: [ hasClass "media-object" ])

  chapterList :: Scraper Text [Text]
  chapterList = chroots ("ul" @: [ hasClass "list-chapters" ] // "li" @: [ hasClass "chapter-item" ]) $ attr "href" "a"

  chapterName :: Scraper Text Text
  chapterName = chroot ("div" @: [ hasClass "p-15" ]) $ text "h4"

  chapterContents :: Scraper Text Text
  chapterContents = chroot ("div" @: [ hasClass "p-15" ]) $ innerHTML $ "div" @: [ hasClass "fr-view" ]

  config :: ProviderConfig
  config = ProviderConfig
    { novelPath = novelBaseURL
    , fetchMetadata = B.defaultFetchMetadata novelTitle coverImage novelAuthor
    , fetchChapter = B.defaultFetchChapter chapterName chapterContents
    , fetchChapterList = \url -> do
      cL <- B.defaultFetchChapterList chapterList url
      return $ Prelude.map (\path -> T.concat [ "https://www.wuxiaworld.com", path ]) cL }
