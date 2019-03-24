module Boxpub.EPUB.Filters
( getFilters ) where
  import Paths_boxpub ( getDataFileName )
  import System.FilePath ( (</>) )
  import Text.Pandoc.Filter ( Filter(..) )

  getFilters :: IO [ Filter ]
  getFilters = do
    attrFilter <- getDataFileName "strip-attrs.lua"
    return
      [ LuaFilter $ attrFilter ]
