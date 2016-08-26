-- Earl -- A flexible redirect server
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Network.HTTP.Types (ok200, movedPermanently301, notFound404)
import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.Wai (Application, responseLBS, pathInfo)

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Network.Wai.Handler.Warp as Warp

-- Strings in Haskell are madness. For url pieces, we get a strict Text. For
-- the response body, we must provide a lazy ByteString, but for the headers a
-- strict ByteString. To alleviate the pain a bit, work with stict Text
-- internally everywhere, and use the two functions below to convert to the
-- required type.

encodeUtf8Lazy :: Text -> Data.ByteString.Lazy.ByteString
encodeUtf8Lazy = Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict

encodeUtf8Strict :: Text -> Data.ByteString.ByteString
encodeUtf8Strict = Data.Text.Encoding.encodeUtf8

serveRedirect :: Text -> Application
serveRedirect newUrl request f =
  let
    body = Text.append "-> " newUrl
    headers =
      [ (hLocation, encodeUtf8Strict newUrl)
      , (hContentType, "text/plain")
      ]
  in
    f $ responseLBS movedPermanently301 headers (encodeUtf8Lazy body)

-- Define the urls.
router :: Application
router request = case pathInfo request of
  "repo" : args : [] -> serveRepo args request
  []                 -> serveIndex request
  _otherPath         -> serveNotFound request

-- Serves the GitHub repository redirect.
serveRepo :: Text -> Application
serveRepo repo =
  serveRedirect $ Text.append "https://github.com/ruuda/" repo

-- Serves main page.
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
