module Boxpub.Client
( main ) where
  import Boxpub.Client.Env ( mkEnv, boxpubVersion )
  import Boxpub.Client.FileSystem ( printString )
  import Boxpub.Client.Parser ( BoxpubOptions(..), getOptions )
  import Foreign.C.String ( newCString )

  main :: IO ()
  main = do
    rawOptions <- getOptions
    case (version rawOptions) of
      True -> putStrLn boxpubVersion
      _ -> do
        env <- mkEnv rawOptions
        str <- newCString "Hello World\0"
        printString str
