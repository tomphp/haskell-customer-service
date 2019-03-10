.PHONY=ghcid

ghcid:
	ghcid --command "stack ghci haskell-customer-service:lib haskell-customer-service:test:haskell-customer-service-test --ghc-options=-Wall --ghci-options=-fobject-code" -T=':!stack test'
