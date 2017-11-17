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
module Tabelle.Internal where

import Data.Maybe (fromJust)
import Data.Bifunctor (first)
import Data.Proxy
import Data.Distributive
import Data.Functor.Rep
import Control.Applicative
import Generics.SOP (Compose,I,All,And,NP,IsProductType,SOP(SOP),NS(Z),unSOP,unZ,from,to)
import Generics.SOP.NP (sequence_NP, cpure_NP)
import qualified Data.Map.Strict as M

newtype Tabelle (xs :: [*]) r = Tabelle { getTabelle :: M.Map (NP I xs) r } 
                              deriving (Functor,Foldable,Traversable)

type Dimensions xs = (All (Enum `And` Bounded) xs,All (Compose Eq I) xs,All (Compose Ord I) xs)

instance (All (Generics.SOP.Compose Show I) xs, Show r) => Show (Tabelle xs r) where
    show (Tabelle t) = "fromRight undefined (fromList " ++ show (M.toList t) ++ ")"

instance Dimensions xs => Distributive (Tabelle xs) where
    distribute (fmap getTabelle -> f) = Tabelle $ 
        M.fromList $ fmap (\k -> (k,fromJust . M.lookup k <$> f)) enumerate_NP

instance Dimensions xs => Representable (Tabelle xs) where
    type Rep (Tabelle xs) = NP I xs 
    tabulate f = Tabelle . M.fromList $ fmap (\np -> (np, f np)) enumerate_NP 
    index (Tabelle t) np = fromJust $ M.lookup np t

instance Dimensions xs => Applicative (Tabelle xs) where
    pure a = tabulate (pure a)
    Tabelle f <*> Tabelle a = Tabelle (M.intersectionWith id f a) 

instance (Dimensions xs, Monoid r) => Monoid (Tabelle xs r) where
    mempty = tabulate (const mempty)
    a `mappend` b = liftA2 mappend a b

fromList :: forall xs a. Dimensions xs => [(NP I xs, a)] -> Either (NP I xs) (Tabelle xs a)
fromList entries =
    let mapita = M.fromList entries
        findKey k = case M.lookup k mapita of
            Just v  -> Right (k,v)
            Nothing -> Left  k 
    in  Tabelle . M.fromList <$> traverse findKey enumerate_NP 

fromList' :: forall r xs a. (IsProductType r xs, Dimensions xs) => [(r, a)] -> Either r (Tabelle xs a)
fromList' entries = 
      first (to . SOP . Z)
    . fromList  
    . fmap (first (unZ . unSOP . from))
    $ entries

toList :: forall xs a . Dimensions xs => Tabelle xs a -> [(NP I xs, a)]  
toList (Tabelle t) = M.toList t 

toList' :: forall r xs a . (IsProductType r xs, Dimensions xs) => Tabelle xs a -> [(r, a)]  
toList' (Tabelle t) = fmap (first (to . SOP . Z)) $ M.toList t 

enumerate_NP :: forall xs. (All (Enum `And` Bounded) xs) => [NP I xs]
enumerate_NP = sequence_NP @xs 
             $ cpure_NP (Proxy @(Enum `And` Bounded)) (enumFromTo minBound maxBound)
