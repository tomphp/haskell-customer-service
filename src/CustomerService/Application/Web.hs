{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module CustomerService.Application.Web (main, UseCases(..)) where

import Data.Aeson (FromJSON, Value(..), object, (.=))
import Network.Wai (Application, Response)
import Network.HTTP.Types.Status as Status
import Web.Scotty.Trans

import GHC.Generics

import CustomerService.Domain.Customer (Customer)
import CustomerService.Domain.NewCustomer (NewCustomer(NewCustomer))
import qualified CustomerService.Domain.Customer as C


main :: (UseCases m, MonadIO m) => (m Response -> IO Response) -> IO Application
main runner = scottyAppT runner routes

class (Monad m) => UseCases m where
  getCustomerById :: Text -> m (Either Text Customer)
  createCustomer :: NewCustomer -> m Text

data CustomerDetails = CustomerDetails { firstName :: Text
                                       , surname :: Text
                                       } deriving (Generic)
instance FromJSON CustomerDetails

routes :: (UseCases m, MonadIO m) =>  ScottyT LText m ()
routes = do
  post "/customers/" $ do
    body <- jsonData `rescue` \e -> do
                                      status Status.badRequest400
                                      json $ object [ "message" .= String (tshow e)]
                                      finish
    let customer = NewCustomer (firstName body) (surname body)
    customerId <- lift $ createCustomer customer

    status Status.created201
    json $ object ["customerId" .= String customerId]

  get "/customers/:id" $ do
    id <- param "id"
    result <- lift $ getCustomerById id

    case result of
      Right customer -> do
        status Status.ok200
        json $ object [ "customerId" .= (String $ C.customerId customer)
                      , "firstName" .= (String $ C.firstName customer)
                      , "surname" .= (String $ C.surname customer)
                      ]
      Left err -> do
        status Status.notFound404
        json $ object [ "message" .= String err ]

