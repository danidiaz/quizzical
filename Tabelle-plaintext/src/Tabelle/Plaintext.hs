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
import Data.Text (Text)
import Control.Applicative
import Generics.SOP (All)

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html
-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec-Char.html
-- http://hackage.haskell.org/package/parser-combinators-0.2.0/docs/Control-Applicative-Combinators.html

parser2D :: (Ord d1,Ord d2) => (Parser d1,Parser d2) -> Parser r -> Parser [((d1,d2),r)]
parser2D (d1,d2) rP = do
    cols <- space1 *> sepEndBy1 d1 space1 <* eol
    rows <- sepEndBy (rowP cols) eol
    return $ mconcat $ zipWith (\c (r,v) -> ((c,r),v)) cols <$> rows
    where 
    rowP cols = do
        header <- d2
        row <- sepEndBy1 rP space1  
        return $ map ((,) header) row
     
ident :: Parser Text
ident = takeWhile1P Nothing isAlphaNum


