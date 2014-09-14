module Main where
import Text.XML.HXT.Core
import Data.List (intercalate)
import Data.List (isPrefixOf)
import qualified Text.Regex.PCRE.Light as R
import qualified Data.ByteString.Char8 as B

htmlRegex :: R.Regex
htmlRegex = R.compile (B.pack "</(html|body|p|ul|a|h1|h2|h3) *>|<br */?>") []

main = do
  s <- getContents
  -- R.match returns Just or Nothing
  -- then decide whether to process as HTML or pass through plain text
  print $ R.match htmlRegex (B.pack s) []
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



