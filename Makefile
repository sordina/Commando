all:
	@echo "Only used for development workflow"

devel:
	commando -c echo -j | grep --line-buffered -v dist | uniqhash | conscript make doc

doc:
	cabal haddock
	chromereload Commando
