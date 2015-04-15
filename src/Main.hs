{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (toStrict)
import Data.Text (Text, pack)

import Data.Monoid
import Web.Spock.Safe
import Lucid

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Monad.IO.Class
import System.Random (StdGen, newStdGen, randomRs)


baseUrl = "http://localhost:8080/"

renderHtmlStrict :: Html a -> ActionT IO a
renderHtmlStrict = html . toStrict . renderText

main :: IO ()
main = do
  conn <- connectSqlite3 "urls.db"
  runSpock 8080 $ spockT id $
    do get root $
           -- Display input form to grab Url to shorten
           renderHtmlStrict $ urlform_
       post root $ do
           -- Get url -> Insert shortened url and url -> Display shortened url
           url <- param' "url"
           liftIO $ insertUrls url conn
           shortenedUrl <- liftIO $ retrieveShortenedUrl url conn
           html $ baseUrl <> shortenedUrl
       get ("" <//> var) $ \shortenedUrl -> do
           -- Retrieve original url given shortendUrl -> Redirect
           url <- liftIO $ retrieveUrl shortenedUrl conn
           case url of
             "" -> html $ "404"
             url -> redirect $ url
       get ("hello" <//> var) $ \name ->
           text ("Hello " <> name <> "!")

insertUrls url conn = do
  shortenedUrl <- randomText 4
  run conn "INSERT INTO urls(url, shortenedUrl) VALUES (?,?)" [toSql url, toSql shortenedUrl]
  commit conn

retrieveShortenedUrl :: Text -> Connection -> IO Text
retrieveShortenedUrl url conn = do
  r <- quickQuery' conn "SELECT shortenedUrl FROM urls WHERE url=?" [toSql url]
  return $ fromSql (r !! 0 !! 0)

retrieveUrl :: Text -> Connection -> IO Text
retrieveUrl url conn = do
  r <- quickQuery' conn "SELECT url FROM urls WHERE shortenedUrl=?" [toSql url]
  return $ case r of
       [] ->  ""
       r  ->  fromSql (r !! 0 !! 0)

randomText :: Int -> IO Text
randomText n = do
  gen <- newStdGen
  let randomChars = map (charset !!) (randomRs (0,61) gen)
  return $ pack $ take n randomChars
  where charset = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

urlform_ :: Html ()
urlform_ = form_ [method_ "post"] $
               input_ [type_ "text", name_ "url"] <>
               input_ [type_ "submit", value_ "Submit"]

{-
Look at
  Monoid
  Applicative
    pure :: a -> f a
    <*> :: f (a -> b) -> f a -> f b
    <*> - map a function inside a functor over a value inside a functor
    f <$> x = fmap f x - just an infix form of fmap
    (+) <$> Just 3 <*> Just 5 = Just 8
  Alternative

<div>
  <span>Hey y'all</span>
  <div class="innerDiv">
    <h1>Give me your moneyz</h1>
    <form method="post">
      <input type="text" name="dollarz"/>
    </form>
  </div>
</div>

mconcat
-}
