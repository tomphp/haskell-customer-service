module CustomerService.Domain.Customer where

data Customer = Customer { customerId :: Text
                         , firstName :: Text
                         , surname :: Text
                         } deriving (Eq, Show)
