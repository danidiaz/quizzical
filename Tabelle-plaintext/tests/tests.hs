{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Data.List
import Data.Text (Text)
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
    , testCase "basicSpaceAtEnd" basicSpaceAtEnd 
    ]

tabletext :: Text
tabletext =
       mconcat
    .  (\ts -> intersperse "\n" ts ++ ["\n"])
    $  ["    d1a  d1b"
       ,"d2x ax   bx "
       ,"d2y ay   by"
       ]

basicexpected :: [((Text,Text),Text)]
basicexpected = 
    [ (("d1a","d2x"),"ax")
    , (("d1b","d2x"),"bx")
    , (("d1a","d2y"),"ay")
    , (("d1b","d2y"),"by")
    ] 

tabletext2 :: Text
tabletext2 = tabletext <> " "

basic :: Assertion
basic = do
    let result = parseMaybe (parser2D (ident,ident) ident) tabletext 
        expected = 
            [ (("d1a","d2x"),"ax")
            , (("d1b","d2x"),"bx")
            , (("d1a","d2y"),"ay")
            , (("d1b","d2y"),"by")
            ] 
    case result of
        Just actual -> assertEqual "parse results" expected actual

basicSpaceAtEnd :: Assertion
basicSpaceAtEnd = do
    let result = parseMaybe (parser2D (ident,ident) ident) tabletext2
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

