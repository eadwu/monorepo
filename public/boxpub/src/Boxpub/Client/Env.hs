module Boxpub.Client.Env
( Env(..)
, mkEnv, boxpubVersion ) where
  import Data.Version ( showVersion )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import qualified Paths_boxpub as Boxpub

  data Env = Env
    { options :: BoxpubOptions }

  mkEnv :: BoxpubOptions -> IO Env
  mkEnv options = return Env
    { options = options }

  boxpubVersion :: String
  boxpubVersion = "boxpub " ++ showVersion Boxpub.version
