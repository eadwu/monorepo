module Boxpub.EPUB
( main ) where
  import Control.Concurrent
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Data.Maybe ( fromJust )
  import Text.Printf ( printf )
  import Boxpub.Client.Provider as P ( Provider, Chapter(..), Metadata(..), ProviderEnv(..), mkEnv, fetchChapter )
  import qualified Boxpub.Client.Provider.BoxNovel as PB ( mkEnv )

  main :: Env -> IO ()
  main env = do
    bnEnv <- PB.mkEnv
    pEnv <- P.mkEnv env bnEnv
    putStrLn filename
    test <- getNumCapabilities
    print test
    where
      filename = printf "%s.epub" (fromJust $ (novel . options) env)
