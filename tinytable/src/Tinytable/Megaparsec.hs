{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Tinytable.Megaparsec (
              Parser
            , Parsable(..)
            -- * Useful parsers
            , readDimParser
            , textCellParser
            -- * Megaparsec re-exports
            , Text.Megaparsec.parse
            , Text.Megaparsec.parseMaybe
            , Text.Megaparsec.parseTest
            , Text.Megaparsec.parseErrorPretty
       ) where

import           Data.Char
import           Data.Void
import           Data.Functor
import           Data.Text (Text,unpack)
import           Control.Applicative
import           Text.Read
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Generics.SOP (NP(..),I(..))

--import           Tinytable
--import           Tinytable.Internal

type Parser = Parsec Void Text 

class Parsable (xs :: [*]) where
    parser :: NP Parser xs -> Parser v -> Parser [(NP I xs, v)]
    --parser :: NP Parser xs -> Parser r -> Parser (Tinytable xs r)

instance Parsable '[x] where
    parser (kp :* Nil) vp = tableD1 kp vp

instance Parsable (y ': xs) => Parsable (x ': y ': xs) where
    parser (kp :* npxs) vp = tableDN kp (parser npxs vp)

-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html
-- http://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec-Char.html
-- http://hackage.haskell.org/package/parser-combinators-0.2.0/docs/Control-Applicative-Combinators.html

tableDN :: Parser k -> Parser [(NP I xs, v)] -> Parser [(NP I (k ': xs), v)]  
tableDN kp xsvp = 
    do space
       char '('
       space
       kvs <- sepEndBy1 pair space1
       char ')'
       pure (mconcat kvs)
  where
    mkEntry k (xs,v) = 
       (I k :* xs,v)
    pair =
       do k <- kp
          space1
          xsv <- xsvp
          pure (mkEntry k <$> xsv)

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

readDimParser :: Read a => Parser a
readDimParser = do
    d <- takeWhile1P Nothing (\c -> isAlphaNum c || c == '\'')
    case readMaybe (unpack d) of
        Nothing -> empty
        Just a -> return a

textCellParser :: Parser Text
textCellParser =  
    let unquoted c =  isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '\''
        quoted c = unquoted c || isSpace c 
     in try (char '"' *> takeWhileP Nothing quoted <* char '"') 
        <|> 
        takeWhile1P Nothing unquoted

-- TODO
-- - set notation
-- - aliases for fields
-- auto-currying ?
-- pattern synonyms
-- should the parser be thingly integrated with Tinytable? 
