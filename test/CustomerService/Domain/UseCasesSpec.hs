{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CustomerService.Domain.UseCasesSpec where

import Test.Hspec

import qualified CustomerService.Mock.App as App

import           CustomerService.Domain.Customer           (Customer(Customer))
import           CustomerService.Domain.NewCustomer        (NewCustomer(NewCustomer))
import qualified CustomerService.Domain.CustomerRepository as Repo
import qualified CustomerService.Domain.UseCases           as UC

spec :: Spec
spec = before (App.newMockEnv []) $
  describe "CustomerService.Domain.UseCases" $ do
    describe "getCustomerById" $ do
      it "returns just the customer when found" $ \env -> do
        (result, cid) <- App.run env $ do
          cid <- Repo.save $ NewCustomer "Small" "Fish"
          result <- UC.getCustomerById cid
          return (result, cid)

        result `shouldBe` (Right $ Customer cid "Small" "Fish")

      it "returns nothing when not found" $ \env -> do
        result <- App.run env $ UC.getCustomerById "5678"

        result `shouldBe` Left "Customer Not Found"

    describe "createCustomer" $
      it "creates a new customer" $ \env -> do
        (result, cid) <- App.run env $ do
          cid <- UC.createCustomer $ NewCustomer "Medium" "Otter"
          result <- Repo.fetchById cid
          return (result, cid)

        result `shouldBe` (Just $ Customer cid "Medium" "Otter")
