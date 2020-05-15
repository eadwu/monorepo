module Boxpub.Client.Provider
( mkEnv ) where
  import Data.Text as T
  import Boxpub.Client.Parser as B ( BoxpubOptions(..) )
  import Boxpub.Client.ProviderType as B
  import Data.Maybe ( fromJust, fromMaybe )
  import Text.Printf ( printf )
  import qualified Boxpub.Client.Provider.BoxNovel as BoxNovel
  import qualified Boxpub.Client.Provider.MachineNovelTranslation as MTL
  import qualified Boxpub.Client.Provider.NovelPlanet as NovelPlanet
  import qualified Boxpub.Client.Provider.WuxiaWorld as WuxiaWorld

  getProvider :: BoxpubOptions -> ProviderConfig
  getProvider args = case source of
    "wuxiaworld" -> WuxiaWorld.config
    -- NOTE: Cloudflare anti-bot check
    -- "novelplanet" -> NovelPlanet.config
    "machinenoveltranslation" -> MTL.config
    -- NOTE: Defaults to BoxNovel
    _ -> BoxNovel.config
    where
      source = T.toLower $ fromMaybe "" $ B.source args

  mkEnv :: BoxpubOptions -> IO ProviderEnv
  mkEnv args = do
    metadata <- (fetchMetadata provider) novelURL
    chapterList <- (fetchChapterList provider) novelURL
    return $ ProviderEnv novelURL provider metadata chapterList
    where
      provider = getProvider args
      novel = fromJust $ B.novel args
      novelURL = printf (unpack $ novelPath provider) novel
