module Boxpub.Client.Env
( Env(..)
, mkEnv
, boxpubVersion ) where
  import Data.Version ( showVersion )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Network.HTTP.Client ( Manager )
  import Network.HTTP.Client.TLS ( newTlsManagerWith, tlsManagerSettings )
  import qualified Paths_boxpub as Boxpub

  data Env = Env
    { options :: BoxpubOptions
    , manager :: Manager }

  mkEnv :: BoxpubOptions -> IO Env
  mkEnv options = do
    manager <- newTlsManagerWith tlsManagerSettings
    return Env
      { options = options
      , manager = manager }

  boxpubVersion :: String
  boxpubVersion = "boxpub " ++ showVersion Boxpub.version
