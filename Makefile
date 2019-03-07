.PHONY=ghcid

ghcid:
	ghcid --command "stack ghci haskell-customer-service:lib haskell-customer-service:test:haskell-customer-service-test --ghci-options=-fobject-code" -T=':!stack test'
