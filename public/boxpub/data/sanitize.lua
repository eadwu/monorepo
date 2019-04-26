local List = require 'pandoc.List'

function sanitizeBoxNovel (elem)
  if elem.classes and elem.classes:includes("code-block", 0) then
    return pandoc.Null()
  end

  return elem
end

function sanitize (elem)
  local elem = sanitizeBoxNovel(elem)

  if elem.attr then
    elem.attr = pandoc.Attr()
  end

  if elem.tag and elem.tag == "Image" then
    elem = pandoc.Null()
  end

  if elem.text and elem.text == "INVALID_CHAPTER" then
    elem = pandoc.Null()
  end

  return elem
end

return {
  { Block = sanitize
  , Inline = sanitize }
}
