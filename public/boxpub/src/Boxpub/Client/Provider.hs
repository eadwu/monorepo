module Boxpub.Client.Provider
( main ) where
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.FileSystem ( printString )
  import Foreign.C.String ( newCString )

  main :: Env -> IO ()
  main env = do
    str <- newCString "Hello World\0"
    printString str
