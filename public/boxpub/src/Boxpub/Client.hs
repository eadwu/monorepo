module Boxpub.Client
( main ) where
  import Boxpub.Client.Env ( mkEnv, boxpubVersion )
  import Boxpub.Client.Parser ( BoxpubOptions(..), getOptions )
  import Data.Maybe ( isNothing )
  import System.Exit ( die )

  main :: IO ()
  main = do
    rawOptions <- getOptions
    if version rawOptions
      then putStrLn boxpubVersion
      else if isNothing $ novel rawOptions
        then die "Invalid argument for <NOVEL> receieved"
        else do
          env <- mkEnv rawOptions
          putStrLn "Hello World"
