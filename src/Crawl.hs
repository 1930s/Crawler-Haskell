{-# LANGUAGE OverloadedStrings #-}

module Crawl where

import Text.HTML.Scalpel (Scraper, URL, chroots, attr, hasClass, scrapeURL, (@:), (//))
import Text.Printf (printf)
import Control.Applicative ((<|>))
import Text.StringLike (StringLike)

type Price = String
type ASIN = String
type CurrencyCode = String

data Item 
    = SubmittedProduct ASIN CurrencyCode Price
    | UnsubmittedItem String 

item :: Scraper String [Item] 
item = chroots "body" (submittedProduct <|> unsubmittedItem)

submittedProduct :: Scraper String Item 
submittedProduct = do
    asin         <- attr "data-asin" $ "div" @: [hasClass "feature"] // "div"
    currencyCode <- attr "data-asin-currency-code" $ "div" @: [hasClass "feature"] // "div"
    price        <- attr "data-asin-price" $ "div" @: [hasClass "feature"] // "div"
    return $ SubmittedProduct asin currencyCode price

-- Later implementation
unsubmittedItem :: Scraper String Item
unsubmittedItem = return $ UnsubmittedItem "Error Occurred" 

scrapeURLs :: (Ord str, StringLike str) => Scraper str a -> [URL] -> IO [Maybe a]
scrapeURLs item = mapM (`scrapeURL` item)

instance Show Item where
    show (SubmittedProduct asin currencyCode price) 
      = printf "ASIN:%s %s:%s" asin currencyCode price
    show (UnsubmittedItem errorMessage) 
      = errorMessage

urls :: [String]
urls = [
        "http://amzn.asia/d/i3pExSS", 
        "http://amzn.asia/d/i1DWFWd",
        "http://a.co/d/hgPaKkN",
        "http://a.co/d/3yMEx9r"
       ]

scraping :: IO ()
scraping = mapM_ print =<< scrapeURLs item urls
