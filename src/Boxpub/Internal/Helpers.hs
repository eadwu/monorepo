module Boxpub.Internal.Helpers
( printf' ) where
  -- Neat use of overloads and recursion
  -- https://stackoverflow.com/questions/34113824/string-substitution-operators-with-lists-in-haskell
  printf' :: String -> [String] -> String
  printf' ('%':'s':d) (a:b) = a ++ printf' d b
  printf' (c:d) b = c : printf' d b
  printf' [] _ = []
