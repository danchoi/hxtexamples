module Main where
import Text.XML.HXT.Core

main = do
  s <- getContents
  r <- runX (
      -- configSysVars [ withTrace 1 ] >>>
      readString [withParseHTML yes, withValidate no] s
      >>> processChildren (this `when` isElem)
      >>> p
      -- >>> putXmlTree "-"
      >>> writeDocumentToString [withIndent yes, withOutputHTML, withOutputEncoding utf8] 
      )
  print r
  return ()

p = deep (isElem >>> hasName "body") 