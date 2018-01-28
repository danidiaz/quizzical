{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Data.List
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)
import Text.Megaparsec
import Text.Megaparsec.Char

import Tabelle.Klartext

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
    $  ["    d2a  d2b"
       ,"d1x xa   xb"
       ,"d1y ya   yb"
       ,"d1z za   zb"
       ]

basicexpected :: [((Text,Text),Text)]
basicexpected = 
    [ (("d1x","d2a"),"xa")
    , (("d1x","d2b"),"xb")
    , (("d1y","d2a"),"ya")
    , (("d1y","d2b"),"yb")
    , (("d1z","d2a"),"za")
    , (("d1z","d2b"),"zb")
    ] 

tabletextSpaceAtEnd :: Text
tabletextSpaceAtEnd = tabletext <> " "

tabletextDoubled :: Text
tabletextDoubled = tabletext <> "\n" <> tabletext

tabletextDoubled2 :: Text
tabletextDoubled2 = tabletext <> tabletext

basic :: Assertion
basic = do
    let result = parseMaybe (table2D (dim,dim) cell) tabletext 
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

basicSpaceAtEnd :: Assertion
basicSpaceAtEnd = do
    let result = parseMaybe (table2D (dim,dim) cell <* space) tabletextSpaceAtEnd
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

basicDoubled :: Assertion
basicDoubled = do
    let p = table2D (dim,dim) cell 
        result = parseMaybe (p *> eol *> p) tabletextDoubled
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

basicDoubled2 :: Assertion
basicDoubled2 = do
    let p = table2D (dim,dim) cell 
        result = parseMaybe (p *> p) tabletextDoubled2
    case result of
        Just actual -> assertEqual "parse results" basicexpected actual

