# Based on https://github.com/roman/Haskell-capataz/blob/master/Makefile

################################################################################

STACK:=stack $(STACK_ARGS)

################################################################################

ghcid:  ## Launch ghcid
	ghcid \
		--command "stack ghci" \
			--restart package.yaml
.PHONY: ghcid

ghcid-test:  ## Launch ghcid and automatically run tests
	ghcid \
		--command "stack ghci \
			--test" \
		--test main \
		--restart package.yaml
.PHONY: ghcid-test

flatb: ## Generate java flatbuffers
	cd ./test/integration/ && \
	flatc \
		--java \
		-o ./test-api/src/main/java/ \
		schema.fbs

test-api: ## Generate java flatbuffers and launch test-api
	make flatb && \
		cd ./test/integration/test-api/ && \
		sbt run

docs:  ## Builds haddock documentation and watch files for changes
	$(STACK) haddock --file-watch
.PHONY: docs

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
