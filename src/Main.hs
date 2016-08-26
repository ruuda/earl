-- Earl -- A flexible redirect server
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Network.HTTP.Types (ok200, notFound404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseLBS, pathInfo)

import qualified Network.Wai.Handler.Warp as Warp

-- Define the urls.
router :: Application
router request = case pathInfo request of
  "repo" : args : [] -> serveRepo args request
  []                 -> serveIndex request
  _otherPath         -> serveNotFound request

-- Serves the GitHub repository redirect.
serveRepo :: Text -> Application
serveRepo request args f =
  f $ responseLBS ok200 [(hContentType, "text/plain")] "should redirect to repository"

-- Serves the GitHub repository redirect.
serveIndex :: Application
serveIndex request f =
  f $ responseLBS ok200 [(hContentType, "text/plain")] "hi"

-- Fallback if no route matched.
serveNotFound :: Application
serveNotFound request f =
  f $ responseLBS notFound404 [(hContentType, "text/plain")] "not found"

-- Runs the webserver at the specified port.
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Serving on port " ++ (show port)
  Warp.run port router

main :: IO ()
main = runServer 8000
