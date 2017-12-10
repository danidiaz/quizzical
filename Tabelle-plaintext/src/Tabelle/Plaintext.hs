{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module Tabelle.Plaintext (parser2D,ident) where

import Data.Char
import Data.Void
import Data.Functor
import Data.Text (Text)
import Control.Applicative
import Generics.SOP (All)

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html
-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec-Char.html
-- http://hackage.haskell.org/package/parser-combinators-0.2.0/docs/Control-Applicative-Combinators.html

parser2D :: (Show d1,Ord d1,Show d2,Ord d2,Show r) => (Parser d1,Parser d2) -> Parser r -> Parser [((d1,d2),r)]
parser2D (d1,d2) rP = do
    cols <- blank1 *> sepEndBy1 (dbg "d1" d1) blank1 <* eol
    rows <- sepEndBy (dbg "rowps" $ rowP cols) eol
    return $ mconcat $ zipWith (\c (r,v) -> ((c,r),v)) cols <$> rows
    where 
    rowP cols = do
        header <- dbg "d2" d2 <* blank1
        row <- sepEndBy1 (dbg "rP" rP) blank1  
        return $ map ((,) header) row
     
ident :: Parser Text
ident = takeWhile1P Nothing isAlphaNum

blank1 :: Parser ()
blank1 = void $ takeWhile1P (Just "white space") $ (==) ' '

