import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import Text.Pandoc (WriterOptions(..), defaultWriterOptions, writers)
import Text.JSON.Generic (decodeJSON)

import Pandoc.Filters.ParseHtml (parseHtml)

main :: IO ()
main = do
  [format,templatesDir] <- getArgs
  let (Just writer) = lookup format writers
  let templateFile = templatesDir </> "default." ++ format
  template <- maybeReadFile templateFile
  let writerOptions = defaultWriterOptions
                        { writerStandalone = True
                        , writerTemplate = template
                        }
  interact $ writer writerOptions . parseHtml . decodeJSON

maybeReadFile :: FilePath -> IO String
maybeReadFile path = do
  exists <- doesFileExist path
  if exists then readFile path else return ""
