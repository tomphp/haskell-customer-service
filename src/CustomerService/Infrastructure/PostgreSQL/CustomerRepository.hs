module CustomerService.Infrastructure.PostgreSQL.CustomerRepository
  ( fetchById
  , save
  ) where

import Database.PostgreSQL.Simple (Connection, Only(Only, fromOnly), Query, query)

import CustomerService.Domain.Customer
import CustomerService.Domain.NewCustomer

fetchById :: Connection -> Text -> IO (Maybe Customer)
fetchById db theId = do
  customers <- query db fetchByIdQuery (Only theId)  :: IO [(Int, Text, Text)]
  case customers of
    [(cid, fn, sn)] -> return $ Just $ Customer (tshow cid) fn sn
    _               -> return Nothing

save :: Connection -> NewCustomer -> IO Text
save db (NewCustomer fn sn) = do
  cid <- query db insertQuery (fn, sn) :: IO [Only Int]
  case cid of
    [theId] -> return $ tshow $ fromOnly theId
    _       -> error "Damn!"

fetchByIdQuery :: Query
fetchByIdQuery = "select customerId, firstName, surname from customers where customerId=?" 

insertQuery :: Query
insertQuery = "insert into customers (firstName, surname) VALUES (?, ?) returning customerId" 
