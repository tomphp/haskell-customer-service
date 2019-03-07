module Main where

import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai                          (Response)

import           Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PSQL
import           Database.PostgreSQL.Simple.Migration ( MigrationResult(..)
                                                      , MigrationCommand(..)
                                                      , runMigrations
                                                      )
import System.Environment
import Fmt

import qualified CustomerService.Application.Web         as Web
import           CustomerService.Application.Application (Env(..), AppT, unApp)


main :: IO ()
main = do
  dbHost <- fromMaybe "localhost" <$> lookupEnv "DB_HOST"
  dbPort <- fromMaybe "5432" <$> lookupEnv "DB_PORT"
  dbUser <- fromMaybe "user" <$> lookupEnv "DB_USERNAME"
  dbPass <- fromMaybe "pass" <$> lookupEnv "DB_PASSWORD"
  dbName <- fromMaybe "db" <$> lookupEnv "DB_NAME"

  port <- lookupEnv "PORT"
  let port' = fromMaybe 3000 (port >>= readMay)

  let uri = ("postgresql://"+|dbUser|+":"+|dbPass|+"@"+|dbHost|+":"+|dbPort|+"/"+|dbName|+"") :: ByteString

  withPostgreSQL uri $ \conn -> do
    putStrLn "Migrating the database..."
    migrate conn

    putStrLn ("Starting on port "+|port|+"...")
    app <- logStdout <$> Web.main (runner (Env conn))
    Warp.run port' app


runner :: Env -> AppT Response -> IO Response
runner env app = flip runReaderT env $ unApp app

withPostgreSQL :: ByteString -> (Connection -> IO ()) -> IO ()
withPostgreSQL uri = 
  bracket (putStrLn "Migrating the database..." >> PSQL.connectPostgreSQL uri)
          PSQL.close

migrate :: Connection -> IO ()
migrate conn = do
  result <- PSQL.withTransaction conn (runMigrations True conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
  where cmds = [ MigrationInitialization
               , MigrationDirectory "migrations/"
               ]
