{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Monoid
import Data.List
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)
import Text.Megaparsec
import Text.Megaparsec.Char

import Tabelle
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
    $  ["   A    B"
       ,"X  xa   xb"
       ,"Y  ya   yb"
       ,"Z  za   zb"
       ]

data D1 = X | Y | Z deriving (Eq,Ord,Enum,Bounded,Show,Read)

data D2 = A | B deriving (Eq,Ord,Enum,Bounded,Show,Read)

basicexpected :: Tabelle '[D1,D2] Text
basicexpected = 
    let list = [ ((X,A),"xa")
               , ((X,B),"xb")
               , ((Y,A),"ya")
               , ((Y,B),"yb")
               , ((Z,A),"za")
               , ((Z,B),"zb")
               ] 
     in case fromList list of
        Right x -> x

tabletextSpaceAtEnd :: Text
tabletextSpaceAtEnd = tabletext <> " "

tabletextDoubled :: Text
tabletextDoubled = tabletext <> "\n" <> tabletext

tabletextDoubled2 :: Text
tabletextDoubled2 = tabletext <> tabletext

basic :: Assertion
basic = do
    let result = parseMaybe (table2D (dim',dim') cell) tabletext 
    case result of
        Just x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpected actual

basicSpaceAtEnd :: Assertion
basicSpaceAtEnd = do
    let result = parseMaybe (table2D (dim',dim') cell <* space) tabletextSpaceAtEnd
    case result of
        Just x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpected actual

basicDoubled :: Assertion
basicDoubled = do
    let p = table2D (dim',dim') cell 
        result = parseMaybe (p *> eol *> p) tabletextDoubled
    case result of
        Just x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpected actual

basicDoubled2 :: Assertion
basicDoubled2 = do
    let p = table2D (dim',dim') cell 
        result = parseMaybe (p *> p) tabletextDoubled2
    case result of
        Just x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpected actual

