{-# LANGUAGE QuasiQuotes #-}

module CustomerService.Acceptance.GetCustomerSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Network.Wai (Application)

import qualified CustomerService.Application.Web as Web
import           CustomerService.Domain.Customer (Customer(Customer))

import qualified CustomerService.Mock.App as MockedApp

{--
Feature: Get the customer based on id
  As a service I want to get the customer details so that I can check whether they exist.
--}

scotty :: IO Application
scotty = do
  env <- MockedApp.newMockEnv [Customer "12345" "Nicole" "Forsgren"]
  Web.main (MockedApp.run env)

spec :: Spec
spec = with scotty $
  describe "Feature: Get the customer based on ID" $ do
    it "Scenario: Fetch an existing customer" $
      -- Given customer "Nicole Forsgren" with ID "12345" exists
      -- When I fetch customer "12345"
      -- Then I should see customer "Nicole Forsgren"
      get "/customers/12345" `shouldRespondWith` [json|
        {
          customerId: "12345",
          firstName: "Nicole",
          surname: "Forsgren"
        }
      |] { matchStatus = 200
         , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
         }

    it "Scenario: Fetching customer which doesn't exist" $
      -- Given there is no customer with ID "99999"
      -- When I fetch customer "99999"
      -- Then I should get a not found response
      get "/customers/99999" `shouldRespondWith` [json|
        { message: "Customer Not Found" }
      |] { matchStatus = 404
         , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
         }
