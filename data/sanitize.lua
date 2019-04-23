local List = require 'pandoc.List'

function sanitizeBoxNovel (elem)
  if elem.classes then
    if elem.classes:includes("code-block", 0) then
      return pandoc.Null()
    end
  end

  return elem
end

function sanitize (elem)
  local elem = sanitizeBoxNovel(elem)

  if elem.attr then
    elem.attr = pandoc.Attr()
  end

  return elem
end

return {
  { Block = sanitize
  , Inline = sanitize }
}
