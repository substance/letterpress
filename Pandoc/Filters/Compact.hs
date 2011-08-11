module Pandoc.Filters.Compact
  ( compact
  ) where

import Text.Pandoc (bottomUp, Pandoc(..), Block(..), Inline(..))

-- | Removes empty paragraphs
compact :: Pandoc -> Pandoc
compact = bottomUp (filter $ not . isEmpty)

isEmpty :: Block -> Bool
isEmpty (Para inlines) = null $ filter (not . isSpace) inlines
isEmpty _ = False

isSpace :: Inline -> Bool
isSpace Space = True
isSpace LineBreak = True
isSpace _ = False
