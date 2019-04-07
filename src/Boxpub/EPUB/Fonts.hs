module Boxpub.EPUB.Fonts
( generateFonts, generateManifest ) where
  import Paths_boxpub ( getDataFileName )
  import Boxpub.EPUB.Constants ( fontSubDirectory )
  import Boxpub.Internal.FileSystem ( mkdirp )
  import Control.Monad ( forM_ )
  import System.Directory ( copyFile )
  import System.FilePath ( (</>), dropExtension, isExtensionOf )
  import Text.Printf ( printf )

  fonts :: [String]
  fonts = [ "OFL.txt" ] ++ map (\v -> "Arvo-" ++ v ++ ".ttf")
    [ "Bold"
    , "BoldItalic"
    , "Italic"
    , "Regular" ]

  -- filename, filepath
  manifestItem :: String -> FilePath -> String
  manifestItem a b = printf "<item id=\"%s\" href=\"%s\" media-type=\"application/x-font-truetype\" />" a b

  generateFonts :: FilePath -> IO ()
  generateFonts root = do
    mkdirp fontsDir
    forM_ fonts $ \f -> do
      dataFilePath <- getDataFileName f
      copyFile dataFilePath (fontsDir </> f)
    where
      fontsDir = root </> fontSubDirectory

  generateManifest :: String
  generateManifest = concatMap (\v -> manifestItem (dropExtension v) (fontSubDirectory </> v)) (filter (isExtensionOf "ttf") fonts)
