{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Monoid
import Data.List
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool,assertFailure)
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
      testCase "basicD1" basicD1 
    , testCase "basic" basic 
    , testCase "basicSpaceAtEnd" basicSpaceAtEnd 
    , testCase "basicDoubled" basicDoubled
    , testCase "basicDoubled2" basicDoubled2
    ]

data D1 = X | Y | Z deriving (Eq,Ord,Enum,Bounded,Show,Read)

tabletextD1 :: Text
tabletextD1 =
       mconcat
    .  (\ts -> intersperse "\n" ts ++ ["\n"])
    $  ["(X  x"
       ," Y  y"
       ," Z  z)"
       ]

basicexpectedD1 :: Tabelle '[D1] Text
basicexpectedD1 = 
    let list = [ (I X :* Nil,"x")
               , (I Y :* Nil,"y")
               , (I Z :* Nil,"z")
               ] 
     in case fromList list of
        Right x -> x

basicD1 :: Assertion
basicD1 = do
    let result = parse (tableD1 (dimRead @D1) cell) "" tabletextD1
    case result of
        Right x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpectedD1 actual
            Left e -> assertFailure (show e)
        Left e -> assertFailure (parseErrorPretty e)

---
---
---

tabletext :: Text
tabletext =
       mconcat
    .  (\ts -> intersperse "\n" ts ++ ["\n"])
    $  ["   A    B"
       ,"X  xa   xb"
       ,"Y  ya   yb"
       ,"Z  za   zb"
       ]


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
     in case fromList' list of
        Right x -> x

tabletextSpaceAtEnd :: Text
tabletextSpaceAtEnd = tabletext <> " "

tabletextDoubled :: Text
tabletextDoubled = tabletext <> "\n" <> tabletext

tabletextDoubled2 :: Text
tabletextDoubled2 = tabletext <> tabletext

basic :: Assertion
basic = do
    let result = parseMaybe (table2D (dimRead,dimRead) cell) tabletext 
    case result of
        Just x -> case fromList' x of
            Right actual -> assertEqual "parse results" basicexpected actual

basicSpaceAtEnd :: Assertion
basicSpaceAtEnd = do
    let result = parseMaybe (table2D (dimRead,dimRead) cell <* space) tabletextSpaceAtEnd
    case result of
        Just x -> case fromList' x of
            Right actual -> assertEqual "parse results" basicexpected actual

basicDoubled :: Assertion
basicDoubled = do
    let p = table2D (dimRead,dimRead) cell 
        result = parseMaybe (p *> eol *> p) tabletextDoubled
    case result of
        Just x -> case fromList' x of
            Right actual -> assertEqual "parse results" basicexpected actual

basicDoubled2 :: Assertion
basicDoubled2 = do
    let p = table2D (dimRead,dimRead) cell 
        result = parseMaybe (p *> p) tabletextDoubled2
    case result of
        Just x -> case fromList' x of
            Right actual -> assertEqual "parse results" basicexpected actual

