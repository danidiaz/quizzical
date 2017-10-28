{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
module Tablita.Internal where

import Data.Maybe (fromJust)
import Data.Bifunctor (first)
import Data.Proxy
import Data.Distributive
import Data.Functor.Rep
import Control.Applicative
import Generics.SOP (Compose,I,All,And,NP,IsProductType,SOP(SOP),NS(Z),unSOP,unZ,from,to)
import Generics.SOP.NP (sequence_NP, cpure_NP)
import qualified Data.Map.Strict as M

newtype Tablita (xs :: [*]) r = Tablita { getTablita :: M.Map (NP I xs) r } 
                              deriving (Functor,Foldable,Traversable)

type Dimensions xs = (All (Enum `And` Bounded) xs,All (Compose Eq I) xs,All (Compose Ord I) xs)

instance (All (Generics.SOP.Compose Show I) xs, Show r) => Show (Tablita xs r) where
    show (Tablita t) = "fromRight undefined (fromList " ++ show (M.toList t) ++ ")"

instance Dimensions xs => Distributive (Tablita xs) where
    distribute (fmap getTablita -> f) = Tablita $ 
        M.fromList $ fmap (\k -> (k,fromJust . M.lookup k <$> f)) enumerate_NP

instance Dimensions xs => Representable (Tablita xs) where
    type Rep (Tablita xs) = NP I xs 
    tabulate f = Tablita . M.fromList $ fmap (\np -> (np, f np)) enumerate_NP 
    index (Tablita t) np = fromJust $ M.lookup np t

instance Dimensions xs => Applicative (Tablita xs) where
    pure a = tabulate (pure a)
    Tablita f <*> Tablita a = Tablita (M.intersectionWith id f a) 

instance (Dimensions xs, Monoid r) => Monoid (Tablita xs r) where
    mempty = tabulate (const mempty)
    a `mappend` b = liftA2 mappend a b

fromList :: forall xs a. Dimensions xs => [(NP I xs, a)] -> Either (NP I xs) (Tablita xs a)
fromList entries =
    let mapita = M.fromList entries
        findKey k = case M.lookup k mapita of
            Just v  -> Right (k,v)
            Nothing -> Left  k 
    in  Tablita . M.fromList <$> traverse findKey enumerate_NP 

fromList' :: forall r xs a. (IsProductType r xs, Dimensions xs) => [(r, a)] -> Either r (Tablita xs a)
fromList' entries = 
      first (to . SOP . Z)
    . fromList  
    . fmap (first (unZ . unSOP . from))
    $ entries

toList :: forall xs a . Dimensions xs => Tablita xs a -> [(NP I xs, a)]  
toList (Tablita t) = M.toList t 

toList' :: forall r xs a . (IsProductType r xs, Dimensions xs) => Tablita xs a -> [(r, a)]  
toList' (Tablita t) = fmap (first (to . SOP . Z)) $ M.toList t 

enumerate_NP :: forall xs. (All (Enum `And` Bounded) xs) => [NP I xs]
enumerate_NP = sequence_NP @xs 
             $ cpure_NP (Proxy @(Enum `And` Bounded)) (enumFromTo minBound maxBound)
