module Boxpub.Client.Parser
( BoxpubOptions(..)
, getOptions, boxpubOptionsParser ) where
  import Data.Text
  import Options.Applicative
  import Data.Semigroup ( (<>) )

  data BoxpubOptions = BoxpubOptions
    { version :: Bool
    , start :: Maybe Int
    , end :: Maybe Int
    , outputDirectory :: Maybe FilePath
    , novel :: Maybe Text }

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
     <> help "Display the version" )
    <*> optional ( option auto
      ( long "start"
     <> metavar "START"
     <> help "The first chapter [index] to include in the ebook"
     <> showDefault ) )
    <*> optional ( option auto
      ( long "end"
     <> metavar "END"
     <> help "The last chapter [index] to include in the ebook"
     <> showDefault ) )
    <*> optional ( strOption
      ( long "output-directory"
     <> metavar "DIR"
     <> help "Write files to DIR" ) )
    <*> optional ( argument str
      ( metavar "NOVEL" ) )
