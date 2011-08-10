convert: Pandoc/Filters/ParseHtml.hs Pandoc/Readers/HTML.hs Pandoc/convert.hs
	ghc --make Pandoc/convert.hs -o convert
