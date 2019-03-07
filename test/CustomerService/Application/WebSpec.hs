{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module CustomerService.Application.WebSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import CustomerService.Domain.Customer
import CustomerService.Domain.UseCases
import qualified CustomerService.Application.Web as Web

newtype MockUseCases a = MockUseCases { runMockUseCases :: IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

instance Web.UseCases MockUseCases where
  getCustomerById "1234" = return $ Right $ Customer "1234" "Big" "Fish"
  getCustomerById _      = return $ Left "Customer not found"

  createCustomer _ = return "1001"

spec :: Spec
spec = with (Web.main runMockUseCases) $
  describe "CustomerService.Application.Web" $ do
    describe "GET /customers/:id" $ do
      it "successfully finds a customer" $ do
        let body = [json|{
                     customerId: "1234",
                     firstName: "Big",
                     surname: "Fish"
                   }|] 

        get "/customers/1234" `shouldRespondWith` body { matchStatus = 200 } 

      it "returns 404 if customer is not found" $ do
        let body = [json|{ message: "Customer not found" }|]

        get "/customers/5678" `shouldRespondWith` body { matchStatus = 404 }

    describe "POST /customers" $ do
      it "successfully creats a customer" $ do
        post "/customers/" [json|{firstName: "Jez", surname: "Humble"}|]
          `shouldRespondWith` [json|{customerId: "1001"}|] { matchStatus = 201 }
