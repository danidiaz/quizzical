{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Data.List
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)
import Text.Megaparsec
import Text.Megaparsec.Char

import Tabelle.Plaintext

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" 
    [
      testCase "basic" basic 
    , testCase "basicSpaceAtEnd" basicSpaceAtEnd 
    , testCase "basicDoubled" basicDoubled
    , testCase "basicDoubled2" basicDoubled2
    ]

tabletext :: Text
tabletext =
       mconcat
    .  (\ts -> intersperse "\n" ts ++ ["\n"])
    $  ["    d1a  d1b"
       ,"d2x ax   bx"
       ,"d2y ay   by "
       ,"d2z az   bz"
       ]

basicexpected :: [((Text,Text),Text)]
basicexpected = 
    [ (("d1a","d2x"),"ax")
    , (("d1b","d2x"),"bx")
    , (("d1a","d2y"),"ay")
    , (("d1b","d2y"),"by")
    , (("d1a","d2z"),"az")
    , (("d1b","d2z"),"bz")
    ] 

tabletextSpaceAtEnd :: Text
tabletextSpaceAtEnd = tabletext <> " "

tabletextDoubled :: Text
tabletextDoubled = tabletext <> "\n" <> tabletext

tabletextDoubled2 :: Text
tabletextDoubled2 = tabletext <> tabletext

basic :: Assertion
basic = do
    let result = parseMaybe (parser2D (ident,ident) ident) tabletext 
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

basicSpaceAtEnd :: Assertion
basicSpaceAtEnd = do
    let result = parseMaybe (parser2D (ident,ident) ident <* space) tabletextSpaceAtEnd
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

basicDoubled :: Assertion
basicDoubled = do
    let p = parser2D (ident,ident) ident 
        result = parseMaybe (p *> eol *> p) tabletextDoubled
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

basicDoubled2 :: Assertion
basicDoubled2 = do
    let p = parser2D (ident,ident) ident 
        result = parseMaybe (p *> p) tabletextDoubled2
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

