module Boxpub.Client.Env
( Env(..)
, mkEnv, boxpubVersion ) where
  import Prelude hiding ( concat )
  import Data.Version ( showVersion )
  import Data.Text ( Text, pack, concat )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import qualified Paths_boxpub as Boxpub

  data Env = Env
    { options :: BoxpubOptions }

  mkEnv :: BoxpubOptions -> IO Env
  mkEnv options = return Env
    { options = options }

  boxpubVersion :: Text
  boxpubVersion = concat
    [ "boxpub "
    , pack $ showVersion Boxpub.version ]
