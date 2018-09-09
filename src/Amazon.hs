{-# LANGUAGE OverloadedStrings #-}

module Amazon where

import Text.HTML.Scalpel (Scraper, URL, chroots, attr, hasClass, scrapeURL, (@:), (//))
import Text.Printf (printf)
import Text.StringLike (StringLike)

type Price = String
type ASIN = String
type CurrencyCode = String

data Item = SubmittedProduct ASIN CurrencyCode Price

item :: Scraper String [Item] 
item = chroots ("div" @: [hasClass "a-container"]) submittedProduct

submittedProduct :: Scraper String Item 
submittedProduct = do
    asin <- attr "data-asin" $ "div" @: [hasClass "feature"] // "div"
    currencyCode <- attr "data-asin-currency-code" $ "div" @: [hasClass "feature"] // "div"
    price <- attr "data-asin-price" $ "div" @: [hasClass "feature"] // "div"
    return $ SubmittedProduct asin currencyCode price

instance Show Item where
    show (SubmittedProduct asin currencyCode price) 
      = printf "ASIN:%s %s:%s" asin currencyCode price

crawlAmazon :: URL -> IO ()
crawlAmazon url = print =<< scrapeURL url item
