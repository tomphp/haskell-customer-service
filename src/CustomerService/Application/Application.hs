{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CustomerService.Application.Application (Env(Env), AppT(unApp)) where

import Database.PostgreSQL.Simple (Connection)

import           CustomerService.Domain.CustomerRepository
import           CustomerService.Application.Web
import qualified CustomerService.Domain.UseCases                              as UseCases
import qualified CustomerService.Infrastructure.PostgreSQL.CustomerRepository as CR

data Env = Env { conn :: Connection }

newtype AppT a = AppT { unApp :: ReaderT Env IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader Env
           )

instance CustomerRepository AppT where
  fetchById theId = do
    db <- conn <$> ask
    liftIO (CR.fetchById db theId)

  save customer = do
    db <- conn <$> ask
    liftIO (CR.save db customer)

instance UseCases AppT where
  getCustomerById = UseCases.getCustomerById
  createCustomer = UseCases.createCustomer
