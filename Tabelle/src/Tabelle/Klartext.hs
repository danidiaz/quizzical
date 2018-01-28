{-# LANGUAGE ApplicativeDo #-}
module Tabelle.Klartext (table2D,dim,dim',cell) where

import Data.Char
import Data.Void
import Data.Functor
import Data.Text (Text,unpack)
import Control.Applicative
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html
-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec-Char.html
-- http://hackage.haskell.org/package/parser-combinators-0.2.0/docs/Control-Applicative-Combinators.html

table2D :: (Show d1,Ord d1,Show d2,Ord d2,Show r) 
        => (Parser d1,Parser d2) 
        -> Parser r 
        -> Parser [((d1,d2),r)]
table2D (d1,d2) rP = do
    cols <- blank1 *> sepEndBy1 d1 blank1 <* eol
    rows <- sepEndBy1 (rowP cols) eol
    return $ mconcat $ zipWith (\c (r,v) -> ((c,r),v)) cols <$> rows
    where 
    rowP cols = do
        header <- d2 <* blank1
        row <- sepEndBy1 rP blank1  
        return $ map ((,) header) row
     
dim :: Parser Text
dim = takeWhile1P Nothing (\c -> isAlphaNum c || c == '\'')

dim' :: Read a => Parser a
dim' = do
    d <- dim
    case readMaybe (unpack d) of
        Nothing -> empty
        Just a -> return a

cell :: Parser Text
cell =  takeWhile1P Nothing (\c -> isAlphaNum c || c == '-' || c == '_' || c == '.')

blank1 :: Parser ()
blank1 = void $ takeWhile1P (Just "white space") $ (==) ' '

