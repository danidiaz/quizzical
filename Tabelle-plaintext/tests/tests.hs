module Main where

import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

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
	assertEqual "booo" 'a' 'a'

