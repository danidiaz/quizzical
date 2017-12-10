{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)
import Text.Megaparsec

import Tabelle.Plaintext

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" 
    [
        testCase "basic" basic 
    ]

basic :: Assertion
basic = do
    let tabletext =
               mconcat
            .  intersperse "\n"
            $  ["     foo"
               ," bar aaa"
               ]
        result = parseMaybe (parser2D (ident,ident) ident) tabletext 
    print $ result 
    assertEqual "booo" 'a' 'a'


