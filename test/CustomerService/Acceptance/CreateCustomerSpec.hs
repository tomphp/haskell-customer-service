{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module CustomerService.Acceptance.CreateCustomerSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import           Data.Aeson       ((.:))
import           Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson as Aeson

import Network.Wai (Application)
import Network.Wai.Test (SResponse(..))

import qualified CustomerService.Application.Web as Web

import qualified CustomerService.Mock.App as MockedApp

{--
Feature: Get the customer based on id
  As a service I want to get the customer details so that I can check whether they exist.
--}

scotty :: IO Application
scotty = do
  env <- MockedApp.newMockEnv []
  Web.main $ MockedApp.run env

spec :: Spec
spec = before scotty $
  describe "Feature: Create customer" $ do
    it "Scenario: A new customer is created" $ do
      -- When I create customer "Gene Kim"
      response <- post "/customers/" [json|{firstName: "Gene", surname: "Kim"}|]

      let body = simpleBody response 
      print body
      let parser = (Aeson.withObject "response" $ \o -> o .: "customerId") :: Aeson.Value -> Parser Text
      let cid = parseMaybe parser =<< Aeson.decode body

      let getUrl = "/customers/" <> encodeUtf8 (fromMaybe "null" cid)
      print getUrl

      -- Then I should be able to fetch customer details for "Gene Kim"
      get ("/customers/" <> encodeUtf8 (fromMaybe "null" cid)) `shouldRespondWith` [json|
        {
          customerId: "0",
          firstName: "Gene",
          surname: "Kim"
        }
      |] { matchStatus = 200
         , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
         }

    it "Scenario: Bad payload is sent" $ do
      post "/customers/" [json|{}|] `shouldRespondWith` [json|
        {
          "message": "\"jsonData - no parse: Error in $: key \\\"firstName\\\" not present. Data was:{}\""
        }
      |] { matchStatus = 400
         , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
         }
