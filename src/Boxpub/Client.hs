module Boxpub.Client
( main ) where
  import Boxpub.Client.Env ( mkEnv, boxpubVersion )
  import Boxpub.Client.Parser ( BoxpubOptions(..), getOptions )
  import Data.Maybe ( isNothing )
  import Data.Text.IO as T ( putStrLn )
  import System.Exit ( die )
  import qualified Boxpub.Client.Provider as BP

  main :: IO ()
  main = do
    rawOptions <- getOptions
    if version rawOptions
      then T.putStrLn boxpubVersion
      else if isNothing $ novel rawOptions
        then die "Invalid argument for <NOVEL> receieved"
        else do
          env <- mkEnv rawOptions
          putStrLn "Hello World"
