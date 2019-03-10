module Boxpub.Client.Handler ( exec ) where
  import Boxpub.Client.Options
  import Boxpub.Client.FileSystem
  import Foreign.C.String ( newCString )

  exec :: Options -> IO ()
  exec (Options version quiet verbose start end outputDirectory novel) = do
    str <- if version
      then newCString "Bye\0"
      else newCString "Hello World\0"
    printString str
