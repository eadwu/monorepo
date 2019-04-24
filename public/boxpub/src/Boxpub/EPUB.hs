module Boxpub.EPUB
( main
, getReaderOptions, getWriterOptions ) where
  import Paths_boxpub ( getDataDir )
  import Boxpub.Client.Env ( Env(..) )
  import Boxpub.Client.Parser ( BoxpubOptions(..) )
  import Boxpub.EPUB.Filters ( getFilters )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import Control.Concurrent ( getNumCapabilities )
  import Control.Concurrent.MVar ( MVar(..), newMVar, readMVar, modifyMVar_)
  import Control.Concurrent.Spawn ( pool, parMapIO_ )
  import Control.Monad.IO.Class ( liftIO )
  import Data.ByteString.Lazy as BL ( writeFile )
  import Data.Default ( def )
  import Data.Maybe ( fromJust, fromMaybe )
  import Data.Text as T ( Text, foldl', append, concat, unpack )
  import Data.Text.IO as T ( readFile, writeFile )
  import System.Directory ( makeAbsolute, getCurrentDirectory )
  import System.FilePath ( (<.>), (</>) )
  import System.IO ( hFlush, stdout )
  import System.IO.Temp ( withSystemTempDirectory )
  import Text.Pandoc.Class ( runIO, fetchMediaResource )
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

  getWriterOptions :: Maybe String -> FilePath -> Metadata -> FilePath -> WriterOptions
  getWriterOptions template dataDir metadata coverImage = def
    { writerTemplate = template
    , writerVariables =
      [ ("css", dataDir </> "stylesheet.css")
      , ("pagetitle", unpack $ title metadata)
      , ("epub-cover-image", coverImage) ]
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

  getContent :: Env -> Provider -> FilePath -> MVar Int -> Int -> IO ()
  getContent env pEnv outDir mVar n = do
    maxConcurrentThreads <- getNumCapabilities
    -- Display initial status output
    nDownloaded <- readMVar mVar
    statusDisplay maxConcurrentThreads nDownloaded ("ing" :: Text)
    -- Fetch the chapter
    chapter <- fetchChapter env pEnv n
    -- workaround for table of contents
    -- encapsulates chapter with a <div> and prepends a <h1>
    T.writeFile (outDir </> filename) (T.concat
      [ "<h1>"
      , P.name chapter
      , "</h1>"
      , "<div>"
      , content chapter
      , "</div>" ])
    -- Update the counter and respective MVar fetched
    modifyMVar_ mVar (\old -> return (old + 1))
    newNDownloaded <- readMVar mVar
    -- Update status output
    statusDisplay maxConcurrentThreads newNDownloaded ("ed" :: Text)
    where
      opts = options env
      first = start opts
      final = end opts
      name = fromJust $ novel opts
      filename = printf "%s-%d.html" name n
      statusDisplay nConcur nDone gramFix = liftIO $ do
        -- ANSI codes aren't supported pre-Windows 10, though the carriage return should work fine...
        printf "\x1b[2K\r[%d/%d/%d built] building %s.epub: download%s chapter %d" nConcur nDone (final - first + 1) name gramFix n
        hFlush stdout

  mergeFiles :: FilePath -> [FilePath] -> IO (Text)
  mergeFiles dir [] = return ""
  mergeFiles dir (x:xs) = do
    -- Fetch the current file contents
    fileContents <- T.readFile (dir </> x)
    -- Fetch the rest of the file recursively
    restOfFile <- mergeFiles dir xs
    return $ T.concat
      [ fileContents
      , restOfFile ]

  main :: Env -> IO ()
  main env = do
    bnEnv <- PB.mkEnv
    pEnv <- P.mkEnv env bnEnv
    filters <- getFilters
    dataDir <- getDataDir
    -- Determine the output directory
    -- NOTE: default to cwd if --output-directory is undefined
    cwd <- getCurrentDirectory
    prefix <- makeAbsolute $ fromMaybe cwd ((outputDirectory . options) env)
    mkdirp prefix
    withSystemTempDirectory "boxpub" $ \tmp -> do
      -- Generate the actual chapter in "chunks"
      maxConcurrentThreads <- getNumCapabilities
      limiter <- pool maxConcurrentThreads
      counter <- newMVar 0
      parMapIO_ (limiter . getContent env bnEnv tmp counter) chapterRange
      putStrLn "" -- "Flush" to next line
      -- Merge the files and store in memory
      fileContents <- mergeFiles tmp (map (\n -> template n) chapterRange)
      -- Perform conversion
      pandocResult <- runIO $ do
        -- Fetch the cover image, ignoring the MimeType
        (fp, _, bs) <- fetchMediaResource $ unpack $ (cover . metadata) pEnv
        liftIO $ BL.writeFile (tmp </> fp) bs
        -- Sanitize contents before generate epub
        raw <- readHtml getReaderOptions fileContents
        src <- applyFilters getReaderOptions filters [ "html" ] raw
        template <- Just <$> getDefaultTemplate "epub"
        writeEPUB3 (getWriterOptions template dataDir (metadata pEnv) (tmp </> fp)) src
      -- Handle errors and write to output directory
      epub <- handleError pandocResult
      C8.writeFile (prefix </> filename) epub
      where
        opts = options env
        name = fromJust $ novel opts
        filename = printf "%s.epub" name
        template = printf "%s-%d.html" name
        chapterRange = [(start opts)..(end opts)]
