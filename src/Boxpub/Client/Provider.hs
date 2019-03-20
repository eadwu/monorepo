module Boxpub.Client.Provider
( main ) where
  import Boxpub.Client.Env ( Env(..) )

  main :: Env -> IO ()
  main env = putStrLn "Hello World"
