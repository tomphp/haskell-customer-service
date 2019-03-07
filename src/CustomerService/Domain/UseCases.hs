module CustomerService.Domain.UseCases where

import CustomerService.Domain.Customer
import CustomerService.Domain.NewCustomer
import CustomerService.Domain.CustomerRepository

getCustomerById :: CustomerRepository m => Text -> m (Either Text Customer)
getCustomerById id = do
  customer <- fetchById id

  case customer of
    Just c -> return $ Right c
    Nothing -> return $ Left "Customer Not Found"

createCustomer :: CustomerRepository m => NewCustomer -> m Text
createCustomer = save
