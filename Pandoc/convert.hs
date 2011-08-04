import Pandoc.Filters.ParseHtml (parseHtml)
import System.Environment (getArgs)
import Text.Pandoc
  ( Pandoc(..), WriterOptions(..)
  , defaultWriterOptions, writers
  )
import Text.JSON.Generic (decodeJSON)
import System.FilePath ((</>))

main :: IO ()
main = do
  [format,templatesDir] <- getArgs
  let (Just writer) = lookup format writers
  let templateFile = templatesDir </> "default." ++ format
  template <- readFile templateFile
  let writerOptions = defaultWriterOptions
                        { writerStandalone = True
                        , writerTemplate = template
                        }
  interact $ writer writerOptions . parseHtml . decodeJSON
