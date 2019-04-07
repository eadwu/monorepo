module Boxpub.EPUB.Content
() where
  metadata :: String
  metadata = "<metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:identifier id="epub-id-1">urn:uuid:2a862dc0-530b-4a09-ab13-9d2c25844c7e</dc:identifier>
    <dc:title id="epub-title-1">Monster Paradise</dc:title>
    <dc:date id="epub-date">2019</dc:date>
    <dc:language>en-US</dc:language>
    <dc:creator id="epub-creator-1">Nuclear Warhead Cooked in Wine,酒煮核弹头</dc:creator>
    <dc:rights>Copyright ©2019&lt;/dc:right&gt;</dc:rights>
    <meta property="dcterms:modified">2019-03-24T19:06:07Z</meta>
  </metadata>"

  manifest :: String
  manifest = "<manifest>
    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml" />
    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav" />
    <item id="style" href="styles/stylesheet1.css" media-type="text/css" />
    <item id="title_page_xhtml" href="text/title_page.xhtml" media-type="application/xhtml+xml" />
    <item id="ch001_xhtml" href="text/ch001.xhtml" media-type="application/xhtml+xml" />
    <item id="ch002_xhtml" href="text/ch002.xhtml" media-type="application/xhtml+xml" />
    <item id="ch003_xhtml" href="text/ch003.xhtml" media-type="application/xhtml+xml" />
    <item id="ch004_xhtml" href="text/ch004.xhtml" media-type="application/xhtml+xml" />
    <item id="ch005_xhtml" href="text/ch005.xhtml" media-type="application/xhtml+xml" />
    <item id="ch006_xhtml" href="text/ch006.xhtml" media-type="application/xhtml+xml" />
    <item id="ch007_xhtml" href="text/ch007.xhtml" media-type="application/xhtml+xml" />
    <item id="ch008_xhtml" href="text/ch008.xhtml" media-type="application/xhtml+xml" />
    <item id="ch009_xhtml" href="text/ch009.xhtml" media-type="application/xhtml+xml" />
    <item id="ch010_xhtml" href="text/ch010.xhtml" media-type="application/xhtml+xml" />
    <item id="Lato-Bold_ttf" href="fonts/Lato-Bold.ttf" media-type="application/x-font-truetype" />
    <item id="Lato-BoldItalic_ttf" href="fonts/Lato-BoldItalic.ttf" media-type="application/x-font-truetype" />
    <item id="Lato-Italic_ttf" href="fonts/Lato-Italic.ttf" media-type="application/x-font-truetype" />
    <item id="Lato-Regular_ttf" href="fonts/Lato-Regular.ttf" media-type="application/x-font-truetype" />
  </manifest>"

  spine :: String
  spine = "<spine toc="ncx">
    <itemref idref="title_page_xhtml" linear="no" />
    <itemref idref="nav" />
    <itemref idref="ch001_xhtml" />
    <itemref idref="ch002_xhtml" />
    <itemref idref="ch003_xhtml" />
    <itemref idref="ch004_xhtml" />
    <itemref idref="ch005_xhtml" />
    <itemref idref="ch006_xhtml" />
    <itemref idref="ch007_xhtml" />
    <itemref idref="ch008_xhtml" />
    <itemref idref="ch009_xhtml" />
    <itemref idref="ch010_xhtml" />
  </spine>"

  -- metadata, manifest, spine
  content = "<?xml version="1.0" encoding="UTF-8"?>
  <package version="3.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="epub-id-1" prefix="ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/">
    %s
    %s
    %s
    <guide>
      <reference type="toc" title="Monster Paradise" href="nav.xhtml" />
    </guide>
  </package>"
