{-# LANGUAGE OverloadedStrings #-}
module Boxpub.Client.Provider.BoxNovel
( Paths(..)
, BoxNovelEnv(..)
, mkEnv
, getRootPath, getNovelPath, getChapterPath
, novelTitle, coverImage, chapterContents ) where
  import Text.HTML.Scalpel ( Scraper, (@:), attr, text, chroot, hasClass, innerHTML )

  data Paths = Paths
    { root :: String
    , novel :: String
    , chapter :: String }

  data BoxNovelEnv = BoxNovelEnv
    { paths :: Paths }

  protocol :: String
  protocol = "https"

  domain :: String
  domain = "boxnovel.com"

  mkEnv :: IO BoxNovelEnv
  mkEnv = return BoxNovelEnv
    { paths = paths }
    where
      novel = "/novel/%s"
      paths = Paths
        { root = protocol ++ "://" ++ domain
        , novel = novel
        , chapter = novel ++ "/chapter-%s" }

  getRootPath :: BoxNovelEnv -> String
  getRootPath = root . paths

  getNovelPath :: BoxNovelEnv -> String
  getNovelPath = novel . paths

  getChapterPath :: BoxNovelEnv -> String
  getChapterPath = chapter . paths

  novelTitle :: Scraper String String
  novelTitle = text "title"

  coverImage :: Scraper String String
  coverImage = chroot ("div" @: [ hasClass "summary_image" ]) $ attr "src" "img"

  chapterContents :: Scraper String String
  chapterContents = innerHTML $ "div" @: [ hasClass "text-left" ]
