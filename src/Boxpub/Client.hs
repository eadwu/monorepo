module Boxpub.Client
( main ) where
  import Boxpub.Client.Env ( mkEnv, boxpubVersion )
  import Boxpub.Client.FileSystem ( printString )
  import Boxpub.Client.Parser ( BoxpubOptions(..), getOptions )
  import Foreign.C.String ( newCString )
  import Data.Maybe ( isJust, fromJust )
  import System.Exit ( die )

  main :: IO ()
  main = do
    rawOptions <- getOptions
    if version rawOptions
      then putStrLn boxpubVersion
      else if not $ isJust $ novel rawOptions
        then die "Invalid argument for <NOVEL> receieved"
        else do
          env <- mkEnv rawOptions
          str <- newCString "Hello World\0"
          printString str
