module Boxpub.Client.Parser
( BoxpubOptions(..)
, getOptions, boxpubOptionsParser ) where
  import Options.Applicative
  import Data.Semigroup ( (<>) )

  data BoxpubOptions = BoxpubOptions
    { version :: Bool
    , start :: Int
    , end :: Int
    , outputDirectory :: Maybe FilePath
    , novel :: Maybe String }

  settings :: ParserPrefs
  settings = prefs showHelpOnEmpty

  description :: InfoMod a
  description = fullDesc

  infoH :: Parser a -> InfoMod a -> ParserInfo a
  infoH a = info (helper <*> a)

  parser :: ParserInfo BoxpubOptions
  parser = infoH boxpubOptionsParser description

  getOptions :: IO BoxpubOptions
  getOptions = customExecParser settings parser

  boxpubOptionsParser :: Parser BoxpubOptions
  boxpubOptionsParser = BoxpubOptions
    <$> switch
      ( long "version"
     <> short 'V'
     <> help "display the version" )
    <*> option auto
      ( long "start"
     <> value (-1)
     <> metavar "START"
     <> help "the first chapter [number] to include in the ebook"
     <> showDefault )
    <*> option auto
      ( long "end"
     <> value (-1)
     <> metavar "END"
     <> help "the last chapter [number] to include in the ebook"
     <> showDefault )
    <*> optional ( strOption
      ( long "output-directory"
     <> metavar "DIR"
     <> help "write files to DIR" ) )
    <*> optional ( argument str
      ( metavar "NOVEL" ) )
