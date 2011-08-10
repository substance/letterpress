module Pandoc.Filters.ParseHtml
  ( parseHtml
  ) where

import Text.Pandoc
import Text.ParserCombinators.Parsec
import Text.HTML.TagSoup
import Data.List (concat)
import Control.Monad (liftM)

-- | local Pandoc that exports `inline`
import Pandoc.Readers.HTML (block, inline)

repeatParser p = liftM concat (manyTill p eof)

parseHtml :: Pandoc -> Pandoc
parseHtml = parseBlocks . parseInlines

parseBlocks :: Pandoc -> Pandoc
parseBlocks = bottomUp plainToPara . bottomUp (concatMap parseBlock)

parseBlock :: Block -> [Block]
parseBlock (RawBlock "unparsed-html" "") = []
parseBlock (RawBlock "unparsed-html" html) = parseHtmlWith (repeatParser block) html
parseBlock x = [x]

plainToPara :: Block -> Block
plainToPara (Plain inlines) = Para inlines
plainToPara x = x

parseInlines :: Pandoc -> Pandoc
parseInlines = bottomUp (concatMap parseInline)

parseInline :: Inline -> [Inline]
parseInline (RawInline "unparsed-html" "") = []
parseInline (RawInline "unparsed-html" html) = parseHtmlWith (repeatParser inline) html
parseInline x = [x]

parseHtmlWith parser html = case runParser parser defaultParserState ("source: " ++ html) tags of
                              Left err -> error $ html ++ show err
                              Right result -> result
  where tags = canonicalizeTags $ parseTagsOptions options html
        options = parseOptions { optTagPosition = True }
