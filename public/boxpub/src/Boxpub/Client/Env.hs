module Boxpub.Client.Env
( Env(..)
, mkEnv, boxpubVersion ) where
  import Data.Text as T ( Text, pack, concat )
  import Data.Version ( showVersion )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import qualified Paths_boxpub as Boxpub

  data Env = Env
    { options :: BoxpubOptions }

  mkEnv :: BoxpubOptions -> IO Env
  mkEnv options = return Env
    { options = options }

  boxpubVersion :: Text
  boxpubVersion = T.concat
    [ "boxpub "
    , pack $ showVersion Boxpub.version ]
