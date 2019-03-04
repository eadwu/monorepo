import Options
import FileSystem
import Options.Applicative
import Foreign.C.String (newCString)

settings :: ParserPrefs
settings = prefs showHelpOnEmpty

parser :: ParserInfo Options
parser = info (helper <*> opts)
  fullDesc

functor :: Options -> IO ()
functor (Options version quiet verbose start end outputDirectory novel) = do
  str <- newCString "Hello World\0"
    printString str

main :: IO ()
main = functor =<< customExecParser settings parser
