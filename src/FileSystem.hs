{-# LANGUAGE ForeignFunctionInterface #-}
module FileSystem where
  import Foreign.C.String

  foreign import ccall unsafe "print_string" printString :: CString -> IO ()
