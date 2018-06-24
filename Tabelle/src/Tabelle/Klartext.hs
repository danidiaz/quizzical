{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module Tabelle.Klartext (tableD1,table2D,dim,dimRead,cell) where

import           Data.Char
import           Data.Void
import           Data.Functor
import           Data.Text (Text,unpack)
import           Control.Applicative
import           Text.Read
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Generics.SOP (NP(..),I(..))

type Parser = Parsec Void Text 

-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html
-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec-Char.html
-- http://hackage.haskell.org/package/parser-combinators-0.2.0/docs/Control-Applicative-Combinators.html

tableD1 :: Parser k -> Parser v -> Parser [(NP I '[k], v)]  
tableD1 kp vp = 
    do space
       char '('
       space
       kvs <- sepEndBy1 pair space1
       char ')'
       pure kvs
  where
    mkEntry k v = 
       (I k :* Nil,v)
    pair =
       do k <- kp
          space1
          v <- vp
          pure (mkEntry k v)

table2D :: (Show d1,Ord d1,Show d2,Ord d2,Show r) 
        => (Parser d1,Parser d2) 
        -> Parser r 
        -> Parser [((d1,d2),r)]
table2D (d1,d2) rP = do
    cols <- blank1 *> sepEndBy1 d2 blank1 <* eol
    rows <- sepEndBy1 (rowP cols) eol
    return $ mconcat $ zipWith (\c (r,v) -> ((r,c),v)) cols <$> rows
    where 
    rowP cols = do
        header <- d1 <* blank1
        row <- sepEndBy1 rP blank1  
        return $ map ((,) header) row
     
dim :: Parser Text
dim = takeWhile1P Nothing (\c -> isAlphaNum c || c == '\'')

dimRead :: Read a => Parser a
dimRead = do
    d <- dim
    case readMaybe (unpack d) of
        Nothing -> empty
        Just a -> return a

cell :: Parser Text
cell =  takeWhile1P Nothing (\c -> isAlphaNum c || c == '-' || c == '_' || c == '.')

blank :: Parser ()
blank = void (takeWhileP (Just "white space") ((==) ' '))

blank1 :: Parser ()
blank1 = void (takeWhile1P (Just "white space") ((==) ' '))

-- TODO
-- set notation
-- 1d tables
-- changes in nomenclature (-D)
-- integrate with Tabelle ?
--
