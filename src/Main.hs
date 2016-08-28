-- Earl -- A flexible redirect server
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens (view)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (toList)
import Network.HTTP.Types (ok200, movedPermanently301, notFound404)
import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.Wai (Application, Response, responseLBS, pathInfo)

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified GitHub.Endpoints.Repos as Github
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as WreqTypes

-- Strings in Haskell are madness. For url pieces, we get a strict Text. For
-- the response body, we must provide a lazy ByteString, but for the headers a
-- strict ByteString. To alleviate the pain a bit, work with stict Text
-- internally everywhere, and use the two functions below to convert to the
-- required type.

encodeUtf8Lazy :: Text -> Data.ByteString.Lazy.ByteString
encodeUtf8Lazy = Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict

encodeUtf8Strict :: Text -> Data.ByteString.ByteString
encodeUtf8Strict = Data.Text.Encoding.encodeUtf8

-- Generates a response that redirects to the given url.
responseRedirect :: Text -> Response
responseRedirect newUrl =
  let
    body = Text.append "-> " newUrl
    headers =
      [ (hLocation, encodeUtf8Strict newUrl)
      , (hContentType, "text/plain")
      ]
  in
    responseLBS movedPermanently301 headers (encodeUtf8Lazy body)

serveRedirect :: Text -> Application
serveRedirect newUrl _request respond = respond $ responseRedirect newUrl

-- Returns a list of all GitHub repositories for the user "ruuda". (Note:
-- without authentication, this is rate-limited heavily.) Returns urls like
-- https://github.com/ruuda/repo-name.
getGithubRepos :: IO [Text]
getGithubRepos = do
  response <- Github.userRepos "ruuda" Github.RepoPublicityAll
  case response of
    Left _error -> return []
    Right repos -> return $ fmap Github.repoHtmlUrl $ toList repos

-- The normal Wreq.get function throws when the response is a 404, which is not
-- what we want. The solution is to use custom options, with the 'checkStatus'
-- function set to one that does not throw for non-200 statuses.
noThrowOptions :: Wreq.Options
noThrowOptions = Wreq.defaults { WreqTypes.checkStatus = Just ignoreStatus }
  where
    ignoreStatus _ _ _ = Nothing

-- Given a list of urls, returns the sublist of urls that serve 200 ok.
probeUrlsExist :: [Text] -> IO [Text]
probeUrlsExist urls =
  let
    probeUrl url = do
      response <- Wreq.getWith noThrowOptions $ Text.unpack url
      if (view Wreq.responseStatus response) == ok200 then
        return $ Just url
      else
        return Nothing
  in
    -- TODO: Return first repository that matches, don't wait for the others
    -- because it is too slow. Also, sort list of repositories by activity date
    -- to increase the probability of a hit?
    catMaybes <$> Async.mapConcurrently probeUrl urls

-- Define the urls.
router :: Application
router request = case pathInfo request of
  "commit" : arg : [] -> serveCommit arg request
  "repo" : arg : [] -> serveRepo arg request
  [] -> serveIndex request
  _ -> serveNotFound request

-- Serves the GitHub repository redirect.
serveRepo :: Text -> Application
serveRepo repo =
  serveRedirect $ Text.append "https://github.com/ruuda/" repo

-- Probes all repositories for the given commit, then redirects to the page
-- for the commit if it was found, or serves a "not found" page.
serveCommit :: Text -> Application
serveCommit commit _request respond = do
  repoUrls <- getGithubRepos
  -- TODO: This should probe the API, in order for private repos to be included.
  let commitUrls = fmap (\ url -> Text.concat [url, "/commit/", commit]) repoUrls
  goodUrls <- probeUrlsExist commitUrls
  case goodUrls of
    [] -> respond $ responseLBS notFound404 [(hContentType, "text/plain")] "commit not found"
    commitUrl : _ -> respond $ responseRedirect commitUrl

-- Serves main page.
serveIndex :: Application
serveIndex request respond =
  respond $ responseLBS ok200 [(hContentType, "text/plain")] "hi"

-- Fallback if no route matched.
serveNotFound :: Application
serveNotFound request respond =
  respond $ responseLBS notFound404 [(hContentType, "text/plain")] "not found"

-- Runs the webserver at the specified port.
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Serving on port " ++ (show port)
  Warp.run port router

main :: IO ()
main = runServer 8000
