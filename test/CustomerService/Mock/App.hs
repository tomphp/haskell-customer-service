{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CustomerService.Mock.App where

import CustomerService.Application.Web (UseCases(..))
import CustomerService.Domain.Customer (Customer(Customer))
import CustomerService.Domain.CustomerRepository
import CustomerService.Domain.UseCases as UC

import qualified CustomerService.Domain.Customer as Customer
import qualified CustomerService.Domain.NewCustomer as NewCustomer

data MockEnv = MockEnv { customers :: !(IORef [Customer]) }

newtype MockedApp a = MockedApp { runMockedApp :: ReaderT MockEnv IO a }
  deriving (Applicative , Functor , Monad , MonadIO , MonadReader MockEnv)

run :: MockEnv -> MockedApp a -> IO a
run env app = runReaderT (runMockedApp app) env

newMockEnv :: [Customer] -> IO MockEnv
newMockEnv customers = do
  ref <- newIORef customers 
  return $ MockEnv ref

instance CustomerRepository MockedApp where
  fetchById id = do
    MockEnv{customers=ref} <- ask
    cs <- readIORef ref
    return $ find ((==id) . Customer.customerId) cs

  save newCustomer = do
    MockEnv{customers=ref} <- ask
    modifyIORef' ref (\customers ->
      let customerId = tshow (length customers)
          customer = Customer customerId (NewCustomer.firstName newCustomer) (NewCustomer.surname newCustomer)
       in customer:customers)
    customers <- readIORef ref
    return $ tshow $ (length customers) - 1

instance UseCases MockedApp where
  getCustomerById = UC.getCustomerById
  createCustomer = UC.createCustomer
