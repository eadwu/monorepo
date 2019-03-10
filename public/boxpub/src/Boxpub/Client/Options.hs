module Boxpub.Client.Options ( Options(..), opts ) where
  import Options.Applicative
  import Data.Semigroup ( (<>) )

  data Options = Options
    { version :: Bool
    , quiet :: Bool
    , verbose :: Bool
    , start :: Int
    , end :: Int
    , outputDirectory :: Maybe FilePath
    , novel :: Maybe String }

  opts :: Parser Options
  opts = Options
    <$> switch
      ( long "version"
     <> short 'V'
     <> help "display the version" )
    <*> switch
      ( long "quiet"
     <> short 'q'
     <> help "be quiet (no [unecessary] output)" )
    <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "be verbose (this is the default)" )
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
