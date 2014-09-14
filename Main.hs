module Main where
import Text.XML.HXT.Core
import Data.List (intercalate)
import Data.List (isPrefixOf)

main = do
  s <- getContents
  r <- runX (
      -- configSysVars [ withTrace 1 ] >>>
      readString [withParseHTML yes, withValidate no] s
      >>> p 
      >>> processTopDown (
          (processAttrl (changeAttrValue prependHttp))
          `when`
          hasBadHref
          )
      >>> writeDocumentToString [withIndent yes, withOutputHTML, withOutputEncoding utf8] 
      )
  putStrLn $ intercalate "\n" r
  return ()

hasBadHref = isElem >>> hasName "a" >>> hasAttr "href"
             >>> getAttrValue "href" >>> isA missingProtocol

p = (deep (isElem >>> hasName "body"))
    `orElse`
    this

-- Fixes web clipped relative URLS like
-- //c.o0bc.com/rf/image_399w/Boston/Images/399newjfk2.jpg


missingProtocol :: String -> Bool
missingProtocol = isPrefixOf "//" 

prependHttp :: String -> String
prependHttp = (++) "http:" 



