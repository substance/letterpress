import Pandoc.Filters.ParseHtml (parseHtml)
import System.Environment (getArgs)
import Text.Pandoc
  ( Pandoc(..), WriterOptions(..), defaultWriterOptions
  , writeLaTeX, writeMarkdown, writeHtmlString
  )
import Text.JSON.Generic (decodeJSON, encodeJSON)

main :: IO ()
main = do
  [format] <- getArgs
  interact $ convert format writerOptions . parseHtml . decodeJSON

writerOptions :: WriterOptions
writerOptions = defaultWriterOptions

convert :: String -> WriterOptions -> Pandoc -> String
convert "latex"    = \_ -> encodeJSON
convert "markdown" = writeMarkdown
convert "html"     = writeHtmlString
convert format = error $ "unknown format: " ++ format
