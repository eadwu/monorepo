module Main
( main ) where
  import GHC.IO.Encoding ( utf8, setLocaleEncoding )
  import GHC.IO.Handle ( hSetEncoding )
  import GHC.IO.Handle.FD ( stderr, stdout )
  import qualified Boxpub.Client as BC

  main :: IO ()
  main = do
    -- NOTE: Hopefully resolves commitAndReleaseBuffer
    setLocaleEncoding utf8
    -- While this seems to only occur during filesystem I/O operations might as
    -- well include the standard handles, stdin is excluded since it isn't used
    hSetEncoding stderr utf8
    hSetEncoding stdout utf8
    BC.main
