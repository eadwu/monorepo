module Boxpub.Client
( main ) where
  import Data.Text as T
  import Data.Text.IO as T
  import Boxpub.Client.Parser ( BoxpubOptions(..), getOptions )
  import Data.Maybe ( isNothing )
  import Data.Version ( showVersion )
  import System.Exit ( die )
  import qualified Boxpub.EPUB as E
  import qualified Paths_boxpub as Boxpub

  boxpubVersion :: Text
  boxpubVersion = T.concat
    [ "boxpub "
    , T.pack $ showVersion Boxpub.version ]

  main :: IO ()
  main = do
    -- Get options as a "readable" data structure
    rawOptions <- getOptions
    if version rawOptions
      -- If `-v` or `--version` is given output the version and exit
      then T.putStrLn boxpubVersion
      -- Otherwise make sure the mandatory parameter is given
      else if isNothing $ novel rawOptions
        -- Exit with an error and nonzero exit code
        then die "Invalid argument for <NOVEL> receieved"
        -- Proceed with the intended operation of the program
        else E.main rawOptions
