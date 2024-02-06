################################################################################

GHCI_OPTS:=\
	--ghci-options '-fdefer-typed-holes -funclutter-valid-hole-fits' \
	--work-dir '.stack-work-ghci' \
	--test \
	--bench \
	--main-is ":test"

################################################################################

hoogle: ## Launch a hoogle server
	stack hoogle --rebuild --server

ghci:		## Launch ghci with some default settings
	stack ghci \
		--package=pretty-simple \
		--ghci-options=-interactive-print=Text.Pretty.Simple.pPrint \
		$(GHCI_OPTS)

test-min:  ## Build the library and run the tests using lowest possible dependency versions
	stack test \
		--ghc-options=-Werror \
		--stack-yaml=./stack/stack.min.yaml \
		--work-dir ".stack-work-min"

stylish:
	find . -name '.stack-work' -prune -o -name 'dist-newstyle' -prune -o -name '*.hs' -exec stylish-haskell -i '{}' \;

release:  ## Creates a release package
	stack clean --stack-yaml=./stack/stack.min.yaml	      --work-dir ".stack-work-min"
	stack test  --stack-yaml=./stack/stack.min.yaml       --work-dir ".stack-work-min"
	stack clean --stack-yaml=./stack/stack.lts-21.25.yaml --work-dir ".stack-work-lts-21.25"
	stack test  --stack-yaml=./stack/stack.lts-21.25.yaml --work-dir ".stack-work-lts-21.25"
	stack clean
	stack test  --ghc-options=-Werror
	make hlint
	stack sdist

hlint: ## Runs hlint on the project
	hlint .

docs:  ## Builds haddock documentation and watch files for changes
	stack haddock --no-haddock-deps --file-watch

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help

.PHONY: test-api
