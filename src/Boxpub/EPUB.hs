module Boxpub.EPUB
( main
, getReaderOptions, getWriterOptions ) where
  import Prelude hiding ( concat, readFile, appendFile )
  import Paths_boxpub ( getDataDir )
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Boxpub.EPUB.Filters ( getFilters )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import Data.Default ( def )
  import Data.Maybe ( fromJust, fromMaybe )
  import Data.Text ( Text, append, concat, unpack )
  import Data.Text.IO ( readFile, appendFile )
  import System.Directory ( makeAbsolute, getCurrentDirectory )
  import System.FilePath ( (<.>), (</>) )
  import System.IO ( hFlush, stdout )
  import System.IO.Temp ( withSystemTempDirectory )
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
      [ dataDir </> "Arvo" </> "Arvo-Bold.ttf"
      , dataDir </> "Arvo" </> "Arvo-BoldItalic.ttf"
      , dataDir </> "Arvo" </> "Arvo-Italic.ttf"
      , dataDir </> "Arvo" </> "Arvo-Regular.ttf" ]
    , writerEpubChapterLevel = 1
    , writerTOCDepth = 1 }

  getContentFile :: Env -> Provider -> Int -> FilePath -> IO FilePath
  getContentFile env pEnv n destination
    | n <= final = do
      chapter <- fetchChapter env pEnv n
      -- ANSI codes aren't supported pre-Windows 10 so good old carriage return
      printf "\r[0/%d/%d built] building %s.epub: downloading chapter %d" (n - first) (final - first) name n
      hFlush stdout
      -- workaround for table of contents
      -- encapsulates chapter with a <div> and prepends a <h1>
      appendFile destination (concat
        [ "<h1>"
        , P.name chapter
        , "</h1>"
        , "<div>"
        , content chapter
        , "</div>" ])
      getContentFile env pEnv (n + 1) destination
    | otherwise = do
      putStrLn ""
      return destination
    where
      opts = options env
      first = start opts
      final = end opts
      name = fromJust $ novel opts

  main :: Env -> IO ()
  main env = do
    bnEnv <- PB.mkEnv
    pEnv <- P.mkEnv env bnEnv
    filters <- getFilters
    dataDir <- getDataDir
    -- default to cwd if --output-directory is undefined
    cwd <- getCurrentDirectory
    prefix <- makeAbsolute $ fromMaybe cwd ((outputDirectory . options) env)
    mkdirp prefix
    withSystemTempDirectory "boxpub" $ \tmp -> do
      file <- getContentFile env bnEnv ((start . options) env) (tmp </> "content" <.> "html")
      fileContents <- readFile file
      pandocResult <- runIO $ do
        raw <- readHtml getReaderOptions fileContents
        src <- applyFilters getReaderOptions filters [ "html" ] raw
        template <- Just <$> getDefaultTemplate "epub"
        writeEPUB3 (getWriterOptions template dataDir (metadata pEnv)) src
      epub <- handleError pandocResult
      C8.writeFile (prefix </> filename) epub
    where
      filename = printf "%s.epub" (fromJust $ (novel . options) env)
