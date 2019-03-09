module Boxpub.Handler (exec) where
  import Boxpub.Options
  import Boxpub.FileSystem
  import Foreign.C.String (newCString)

  exec :: Options -> IO ()
  exec (Options version quiet verbose start end outputDirectory novel) = do
    str <- if version
      then newCString "Bye\0"
      else newCString "Hello World\0"
    printString str
