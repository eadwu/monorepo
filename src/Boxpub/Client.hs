module Boxpub.Client
( main ) where
  import Data.Text.IO as T
  import Boxpub.Client.Env ( mkEnv, boxpubVersion )
  import Boxpub.Client.Parser ( BoxpubOptions(..), getOptions )
  import Data.Maybe ( isNothing )
  import System.Exit ( die )
  import qualified Boxpub.EPUB as E

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
        else do
          env <- mkEnv rawOptions
          E.main env
