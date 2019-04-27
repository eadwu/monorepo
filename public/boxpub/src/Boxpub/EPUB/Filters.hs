module Boxpub.EPUB.Filters
( getFilters ) where
  import Paths_boxpub ( getDataFileName )
  import Text.Pandoc.Filter ( Filter(..) )

  -- | Retrieves the filters used for sanitizing the output HTML.
  getFilters :: IO [ Filter ]
  getFilters = do
    attrFilter <- getDataFileName "sanitize.lua"
    return
      [ LuaFilter attrFilter ]
