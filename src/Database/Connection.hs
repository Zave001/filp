{-# LANGUAGE OverloadedStrings #-}

module Database.Connection where

import Database.PostgreSQL.Simple

connectLocal :: IO Connection
connectLocal = connectPostgreSQL "host=localhost port=5432 dbname=musicalhub user=postgres password=543997"