module Boxpub.EPUB
( main
, getReaderOptions, getWriterOptions ) where
  import Prelude hiding ( concat )
  import Paths_boxpub ( getDataDir )
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Boxpub.EPUB.Filters ( getFilters )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import Data.Default ( def )
  import Data.Maybe ( fromJust, fromMaybe )
  import Data.Text ( Text, append, concat, unpack )
  import System.Directory ( makeAbsolute, getCurrentDirectory )
  import System.FilePath ( (</>) )
  import System.IO ( hFlush, stdout )
  import Text.Pandoc.Class ( runIO )
  import Text.Pandoc.Extensions ( Extensions, Extension(..), extensionsFromList )
  import Text.Pandoc.Error ( handleError )
  import Text.Pandoc.Filter ( applyFilters )
  import Text.Pandoc.Options ( ReaderOptions(..), WriterOptions(..) )
  import Text.Pandoc.Readers ( readHtml )
  import Text.Pandoc.Templates ( getDefaultTemplate )
  import Text.Pandoc.Writers ( writeEPUB3 )
  import Text.Printf ( printf )
  import Boxpub.Client.Provider as P ( Provider, Chapter(..), Metadata(..), ProviderEnv(..), mkEnv, fetchChapter )
  import qualified Boxpub.Client.Provider.BoxNovel as PB ( mkEnv )
  import qualified Boxpub.EPUB.Metadata as M ( generate )
  import qualified Data.ByteString.Lazy.Char8 as C8 ( writeFile )

  -- extensions don't really seem to work or at least does nothing much here
  getExtensions :: Extensions
  getExtensions = extensionsFromList
    [ Ext_native_divs
    , Ext_native_spans
    , Ext_epub_html_exts
    , Ext_header_attributes
    , Ext_implicit_header_references ]

  getReaderOptions :: ReaderOptions
  getReaderOptions = def
    { readerExtensions = getExtensions
    , readerStripComments = True }

  getWriterOptions :: Maybe String -> FilePath -> Metadata -> WriterOptions
  getWriterOptions template dataDir metadata = def
    { writerTemplate = template
    , writerVariables =
      [ ("css", dataDir </> "stylesheet.css")
      , ("pagetitle", unpack $ title metadata) ]
    , writerTableOfContents = True
    , writerSectionDivs = True
    , writerEpubMetadata = Just $ M.generate (title metadata) (author metadata)
    , writerEpubFonts =
      [ dataDir </> "Lato" </> "Lato-Bold.ttf"
      , dataDir </> "Lato" </> "Lato-BoldItalic.ttf"
      , dataDir </> "Lato" </> "Lato-Italic.ttf"
      , dataDir </> "Lato" </> "Lato-Regular.ttf" ]
    , writerEpubChapterLevel = 1
    , writerTOCDepth = 1 }

  generateXHTMLBody :: Env -> Provider -> Int -> Text -> IO Text
  generateXHTMLBody env pEnv n initial
    | n <= final = do
      chapter <- fetchChapter env pEnv n
      -- ANSI codes aren't supported pre-Windows 10 so good old carriage return
      printf "\r[%d/%d/%d built] building %s.epub: downloading chapter %d" (1 :: Int) n final name n
      hFlush stdout
      -- workaround for table of contents
      -- encapsulates chapter with a <div> and prepends a <h1>
      generateXHTMLBody env pEnv (n + 1) (append initial (concat
        [ "<h1>"
        , P.name chapter
        , "</h1>"
        , "<div>"
        , content chapter
        , "</div>" ]))
    | otherwise = do
      putStrLn ""
      return initial
    where
      opts = options env
      final = end opts
      name = fromJust $ novel opts

  main :: Env -> IO ()
  main env = do
    bnEnv <- PB.mkEnv
    pEnv <- P.mkEnv env bnEnv
    filters <- getFilters
    dataDir <- getDataDir
    xhtmlBody <- generateXHTMLBody env bnEnv ((start . options) env) ""
    result <- runIO $ do
      raw <- readHtml getReaderOptions xhtmlBody
      src <- applyFilters getReaderOptions filters [ "html" ] raw
      template <- Just <$> getDefaultTemplate "epub"
      writeEPUB3 (getWriterOptions template dataDir (metadata pEnv)) src
    epub <- handleError result
    -- default to cwd if --output-directory is undefined
    cwd <- getCurrentDirectory
    prefix <- makeAbsolute $ fromMaybe cwd ((outputDirectory . options) env)
    mkdirp prefix
    C8.writeFile (prefix </> filename) epub
    where
      filename = printf "%s.epub" (fromJust $ (novel . options) env)
