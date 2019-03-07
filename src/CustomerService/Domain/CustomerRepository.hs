module CustomerService.Domain.CustomerRepository where

import CustomerService.Domain.Customer
import CustomerService.Domain.NewCustomer

class (Monad m) => CustomerRepository m where
  fetchById :: Text -> m (Maybe Customer)
  save :: NewCustomer -> m Text

