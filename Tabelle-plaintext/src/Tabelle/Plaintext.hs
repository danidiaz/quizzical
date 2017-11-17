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
module Tabelle.Plaintext where

import Data.Void
import Data.Text (Text)
import Control.Applicative
import Generics.SOP (All)

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parser2D :: (Ord d1,Ord d2) => (Parser d1,Parser d2) -> Parser r -> Parser [((d1,d2),r)]
parser2D (d1,d2) r = do
    cols <- space1 *> liftA2 (\cs c -> cs ++ [c]) (some (d2 <* space1)) (d2 <* space <* eol)   
    return []
     
--parse2DFile :: (Dimensions xs, All Read xs, xs ~ [x1,x2]) => String -> Tabelle xs String
--parse2DFile = undefined
