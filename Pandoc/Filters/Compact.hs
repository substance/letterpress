module Pandoc.Filters.Compact
  ( compact
  ) where

import Text.Pandoc (bottomUp, Pandoc(..), Block(..), Inline(..))

-- | Removes empty paragraphs
compact :: Pandoc -> Pandoc
compact = bottomUp trimPara

trimPara :: Block -> Block
trimPara (Para inlines) = Para $ trim inlines
trimPara a = a

trim :: [Inline] -> [Inline]
trim = trimRight . trimLeft

trimLeft :: [Inline] -> [Inline]
trimLeft [] = []
trimLeft (a:as) = if isSpace a then trimLeft as else a:as

trimRight :: [Inline] -> [Inline]
trimRight [] = []
trimRight (a:as) = let nexts = trimRight as
                   in case nexts of
                        [] -> if isSpace a then [] else [a]
                        bs -> a:bs

isSpace :: Inline -> Bool
isSpace Space = True
isSpace LineBreak = True
isSpace _ = False
