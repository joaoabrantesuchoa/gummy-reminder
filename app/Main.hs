{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple
import Data.List (intercalate)
import Text.Read (Lexeme(String))

pg :: ConnectInfo 
pg = defaultConnectInfo {
  connectHost = "localhost",
  connectDatabase = "gummy-reminder-dev",
  connectUser = "postgres",
  connectPassword = "0152"
}

getCardQuestion:: Connection -> Int -> IO [Only String]
getCardQuestion conn c_id = query conn "SELECT question FROM cards WHERE id = ?" $ Only c_id

main :: IO ()
main = do
  conn <- connect pg
  card <- getCardQuestion conn 1
  putStrLn (fromOnly(head card))