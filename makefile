hakyll-shortcode: FORCE
	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build
	runhaskell Setup.hs install

test: FORCE
	cabal test

FORCE:
