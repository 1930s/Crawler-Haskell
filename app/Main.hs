module Main where

import Amazon
import AtCoder

main :: IO ()
main = do
    amazon
    atcoder

amazon :: IO ()
amazon = do
    putStrLn "amazon URL"
    amazonURL <- getLine
    crawlAmazon amazonURL

atcoder :: IO ()
atcoder = do
    putStrLn "contest"
    contest <- getLine
    putStrLn "username"
    username <- getLine
    crawlAtCoder contest username
