{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Lib
    (someFunc
    ) where

import Network.Wreq
import Control.Lens
import "lens-aeson" Data.Aeson.Lens (key, _String)

main = someFunc

someFunc :: IO ()
someFunc = do
    r <- get "http://httpbin.org/get"
    print $ r ^. responseHeaders
    print $ r ^? responseBody . key "url" . _String
