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
    , testCase "basicD1Quoted" basicD1Quoted 
    , testCase "basicD2" basicD2 
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
    let result = parse (parser (dimRead @D1 :* Nil) cell) "" tabletextD1
    case result of
        Right x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpectedD1 actual
            Left e -> assertFailure (show e)
        Left e -> assertFailure (parseErrorPretty e)

---
---
---

data D2 = A | B deriving (Eq,Ord,Enum,Bounded,Show,Read)

tabletextD2 :: Text
tabletextD2 =
       mconcat
    .  (\ts -> intersperse "\n" ts ++ ["\n"])
    $  ["(X (A  xa"
       ,"    B  xb)"
       ," Y (A  ya"
       ,"    B  yb)"
       ," Z (A  za"
       ,"    B  zb))"
       ]

basicexpectedD2 :: Tabelle '[D1,D2] Text
basicexpectedD2 = 
    let list = [ ((X,A),"xa")
               , ((X,B),"xb")
               , ((Y,A),"ya")
               , ((Y,B),"yb")
               , ((Z,A),"za")
               , ((Z,B),"zb")
               ] 
     in case fromList' list of
        Right x -> x

basicD2 :: Assertion
basicD2 = do
    let result = parse (parser (dimRead @D1 :* dimRead @D2 :* Nil) cell) "" tabletextD2
    case result of
        Right x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpectedD2 actual
            Left e -> assertFailure (show e)
        Left e -> assertFailure (parseErrorPretty e)

---
---
---

tabletextD1Quoted :: Text
tabletextD1Quoted =
       mconcat
    .  (\ts -> intersperse "\n" ts ++ ["\n"])
    $  ["(X  \"u v\""
       ," Y  \"\""
       ," Z  z)"
       ]

basicexpectedD1Quoted :: Tabelle '[D1] Text
basicexpectedD1Quoted = 
    let list = [ (I X :* Nil,"u v")
               , (I Y :* Nil,"")
               , (I Z :* Nil,"z")
               ] 
     in case fromList list of
        Right x -> x

basicD1Quoted :: Assertion
basicD1Quoted = do
    let result = parse (parser (dimRead @D1 :* Nil) cell) "" tabletextD1Quoted
    case result of
        Right x -> case fromList x of
            Right actual -> assertEqual "parse results" basicexpectedD1Quoted actual
            Left e -> assertFailure (show e)
        Left e -> assertFailure (parseErrorPretty e)

