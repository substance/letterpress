import Control.Monad (liftM)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B

import Text.Pandoc (WriterOptions(..), Pandoc(..), defaultWriterOptions, writers)
import Text.Pandoc.Writers.EPUB (writeEPUB)
import Text.Pandoc.Writers.ODT (writeODT)
import Text.Pandoc.Writers.RTF (rtfEmbedImage)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (HTMLSlideVariant(..))
import Text.Pandoc.Generic (bottomUpM)
import Text.Pandoc.Templates (getDefaultTemplate)
import Text.JSON.Generic (decodeJSON)

import Pandoc.Filters.ParseHtml (parseHtml)
import Pandoc.Filters.Compact (compact)

binaryWriters :: [(String, WriterOptions -> Pandoc -> IO B.ByteString)]
binaryWriters =
  [ ("epub", writeEPUB Nothing [])
  , ("odt",  writeODT Nothing)
  ]

stringWriters :: [(String, WriterOptions -> Pandoc -> String)]
stringWriters = writers

main :: IO ()
main = do
  [format,outputFile,rootDir] <- getArgs
  let templatesDir = rootDir </> "templates"
  doc <- liftM (compact . parseHtml . decodeJSON) getContents
  template <- getTemplate templatesDir format
  let variables = case format of
        "s5" -> [("s5-url", "/s5/default")]
        _ -> []
  let writerOptions = defaultWriterOptions
                        { writerStandalone = True
                        , writerTemplate = template
                        , writerVariables = variables
                        , writerSlideVariant = case format of
                                                 "s5"     -> S5Slides
                                                 _        -> NoSlides
                        }
  doc' <- (if format == "rtf" then bottomUpM rtfEmbedImage else return) doc
  case lookup format binaryWriters of
    (Just binaryWriter) -> do
      -- binary formats are written to outputFile
      binaryWriter writerOptions doc' >>= B.writeFile outputFile
    Nothing -> do
      -- other formats are written to stdout
      let (Just writer) = lookup format stringWriters
      putStr $ writer writerOptions doc'

getTemplate :: FilePath -> String -> IO String
getTemplate user = liftM (either (const "") id) . getDefaultTemplate (Just user)
