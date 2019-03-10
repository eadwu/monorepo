module Main where
  import Boxpub.Options
  import Boxpub.Handler

  import Options.Applicative

  settings :: ParserPrefs
  settings = prefs showHelpOnEmpty

  parser :: ParserInfo Options
  parser = info (helper <*> opts)
    fullDesc

  main :: IO ()
  main = exec =<< customExecParser settings parser
