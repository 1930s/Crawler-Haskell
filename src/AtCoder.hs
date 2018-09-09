{-# LANGUAGE OverloadedStrings #-}

module AtCoder where

import Text.HTML.Scalpel (Scraper, URL, chroots, attrs, texts, scrapeURL, (//))
import Text.Printf (printf)
import Control.Applicative ((<|>), empty)
import Control.Monad (mfilter)

type Details = [String]
type Username = String
type Contest = String

data Submission
    = AcceptedSubmission Details URL
    | FailedSubmission Details URL

submissions :: Scraper String [Submission]
submissions = chroots "tr" (acceptedSubmission <|> failedSubmission)

acceptedSubmission :: Scraper String Submission
acceptedSubmission = do
    status <- fmap init . filterN 10 $ texts "td"
    submissionURL <- fmap last. attrs "href" $ "td" // "a"
    return $ AcceptedSubmission status submissionURL

failedSubmission :: Scraper String Submission
failedSubmission = do
    status <- fmap init . filterN 8 $ texts "td"
    submissionURL <- fmap last . attrs "href" $ "td" // "a"
    return $ FailedSubmission status submissionURL

filterN :: Int -> Scraper str [a] -> Scraper str [a]
filterN n = mfilter ((==n) . length)

instance Show Submission where
    show (AcceptedSubmission details url) 
      = unwords $ zipWith (++) acceptedTableElements (details ++ [url])
        where 
            acceptedTableElements = ["CreatedTime", "Title:", "UserName:", "Language:", "Score:", "SourceLength:", "Status:", "ExecTime:", "MemoryUsage:", "Details:"] 

    show (FailedSubmission details url) 
      = unwords $ zipWith (++) failedTableElements (details ++ [url])
        where
            failedTableElements = ["CreatedTime", "Title:", "UserName:", "Language:", "Score:", "SourceLength:", "Status:", "Details:"] 

url :: Contest -> Username -> URL
url = printf "https://%s.contest.atcoder.jp/submissions/all?user_screen_name=%s"

crawlAtCoder :: Contest -> Username -> IO ()
crawlAtCoder contest username = print =<< scrapeURL (url contest username) submissions
