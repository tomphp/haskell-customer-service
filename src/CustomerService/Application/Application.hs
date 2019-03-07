{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CustomerService.Application.Application (Env(Env), AppT(unApp)) where

import Database.PostgreSQL.Simple (Connection, Only(Only, fromOnly), Query, query)

import           CustomerService.Domain.Customer
import           CustomerService.Domain.NewCustomer
import           CustomerService.Domain.CustomerRepository
import           CustomerService.Application.Web
import qualified CustomerService.Domain.UseCases            as UseCases

data Env = Env { conn :: Connection }

newtype AppT a = AppT { unApp :: ReaderT Env IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader Env
           )

fetchByIdQuery :: Query
fetchByIdQuery = "select customerId, firstName, surname from customers where customerId=?" 

insertQuery :: Query
insertQuery = "insert into customers (firstName, surname) VALUES (?, ?) returning customerId" 

instance CustomerRepository AppT where
  fetchById theId = do
    db <- conn <$> ask
    customers <- liftIO (query db fetchByIdQuery (Only theId)  :: IO [(Int, Text, Text)])
    case customers of
      [(cid, fn, sn)] -> return $ Just $ Customer (tshow cid) fn sn
      _               -> return Nothing

  save (NewCustomer fn sn) = do
    db <- conn <$> ask
    cid <- liftIO (query db insertQuery (fn, sn) :: IO [Only Int])
    case cid of
      [theId] -> return $ tshow $ fromOnly theId
      _       -> error "Damn!"

instance UseCases AppT where
  getCustomerById = UseCases.getCustomerById
  createCustomer = UseCases.createCustomer
