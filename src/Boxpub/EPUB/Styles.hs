module Boxpub.EPUB.Styles
( generateStyles ) where
  import Paths_boxpub ( getDataFileName )
  import Boxpub.EPUB.Constants ( styleSubDirectory )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import Control.Monad ( forM_ )
  import System.Directory ( copyFile )
  import System.FilePath ( (</>) )

  stylesheets :: [String]
  stylesheets = map (++ ".css")
    [ "stylesheet" ]

  generateStyles :: FilePath -> IO ()
  generateStyles root = do
    mkdirp stylesDir
    forM_ stylesheets $ \f -> do
      dataFilePath <- getDataFileName f
      copyFile dataFilePath (stylesDir </> f)
    where
      stylesDir = root </> styleSubDirectory
