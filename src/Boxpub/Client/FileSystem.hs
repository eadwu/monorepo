{-# LANGUAGE ForeignFunctionInterface #-}
module Boxpub.Client.FileSystem ( printString ) where
  import Foreign.C.String

  foreign import ccall unsafe "print_string" printString :: CString -> IO ()
