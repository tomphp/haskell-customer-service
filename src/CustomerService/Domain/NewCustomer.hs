module CustomerService.Domain.NewCustomer where
  
data NewCustomer = NewCustomer { firstName :: Text
                               , surname :: Text
                               } deriving (Eq, Show)
