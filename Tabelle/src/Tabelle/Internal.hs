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

import           Data.Maybe (fromJust)
import           Data.Bifunctor (first)
import           Data.Proxy
import           Data.Distributive
import           Data.Functor.Rep
import           Control.Applicative
import           Generics.SOP (Compose,I,All,And,NP,IsProductType,SOP(SOP),NS(Z),unSOP,unZ,from,to)
import           Generics.SOP.NP (sequence_NP, cpure_NP)
import           Generics.SOP.Dict                          
import qualified Data.Map.Strict as M

-- https://stackoverflow.com/questions/50777865/super-classes-with-all-from-generics-sop
newtype Tabelle (xs :: [*]) r = 
    Tabelle { getTabelle :: M.Map (NP I xs) r } 
    deriving (Functor,Foldable,Traversable)

type Dimension = Enum `And` Bounded `And` Compose (Show `And` Eq `And` Ord) I

instance (All (Generics.SOP.Compose Show I) xs, Show r) => Show (Tabelle xs r) where
    show (Tabelle t) = "fromRight undefined (fromList " ++ show (M.toList t) ++ ")"

instance (All Dimension xs,Eq v) => Eq (Tabelle xs v) where
    tabelle1 == tabelle2 = and (liftA2 (==) tabelle1 tabelle2)

instance All Dimension xs => Distributive (Tabelle xs) where
    distribute (fmap getTabelle -> f) = 
        let dict :: Dict (All Dimension) xs
            dict = Dict
            dictTrans0 :: forall z . Dict Dimension z -> Dict (Compose (Show `And` Eq `And` Ord) I) z 
            dictTrans0 d = withDict d Dict
            dictTrans1 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Eq I) z
            dictTrans1 d = withDict d Dict
            dictTrans2 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Ord I) z
            dictTrans2 d = withDict d Dict
            dictEq :: Dict (All (Compose Eq I)) xs
            dictEq = mapAll (dictTrans1 . dictTrans0) dict
            dictOrd :: Dict (All (Compose Ord I)) xs
            dictOrd = mapAll (dictTrans2 . dictTrans0) dict
         in withDict dictEq (withDict dictOrd (Tabelle (M.fromList (fmap (\k -> (k,fromJust . M.lookup k <$> f)) enumerate_NP))))

instance All Dimension xs => Representable (Tabelle xs) where
    type Rep (Tabelle xs) = NP I xs 
    tabulate f = 
        let dict :: Dict (All Dimension) xs
            dict = Dict
            dictTrans0 :: forall z . Dict Dimension z -> Dict (Compose (Show `And` Eq `And` Ord) I) z 
            dictTrans0 d = withDict d Dict
            dictTrans1 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Eq I) z
            dictTrans1 d = withDict d Dict
            dictTrans2 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Ord I) z
            dictTrans2 d = withDict d Dict
            dictEq :: Dict (All (Compose Eq I)) xs
            dictEq = mapAll (dictTrans1 . dictTrans0) dict
            dictOrd :: Dict (All (Compose Ord I)) xs
            dictOrd = mapAll (dictTrans2 . dictTrans0) dict
         in withDict dictEq (withDict dictOrd (Tabelle (M.fromList (fmap (\np -> (np, f np)) enumerate_NP))))
    index (Tabelle t) np = 
        let dict :: Dict (All Dimension) xs
            dict = Dict
            dictTrans0 :: forall z . Dict Dimension z -> Dict (Compose (Show `And` Eq `And` Ord) I) z 
            dictTrans0 d = withDict d Dict
            dictTrans1 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Eq I) z
            dictTrans1 d = withDict d Dict
            dictTrans2 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Ord I) z
            dictTrans2 d = withDict d Dict
            dictEq :: Dict (All (Compose Eq I)) xs
            dictEq = mapAll (dictTrans1 . dictTrans0) dict
            dictOrd :: Dict (All (Compose Ord I)) xs
            dictOrd = mapAll (dictTrans2 . dictTrans0) dict
         in withDict dictEq (withDict dictOrd (fromJust (M.lookup np t)))

instance All Dimension xs => Applicative (Tabelle xs) where
    pure a = tabulate (pure a)
    Tabelle f <*> Tabelle a = 
        let dict :: Dict (All Dimension) xs
            dict = Dict
            dictTrans0 :: forall z . Dict Dimension z -> Dict (Compose (Show `And` Eq `And` Ord) I) z 
            dictTrans0 d = withDict d Dict
            dictTrans1 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Eq I) z
            dictTrans1 d = withDict d Dict
            dictTrans2 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Ord I) z
            dictTrans2 d = withDict d Dict
            dictEq :: Dict (All (Compose Eq I)) xs
            dictEq = mapAll (dictTrans1 . dictTrans0) dict
            dictOrd :: Dict (All (Compose Ord I)) xs
            dictOrd = mapAll (dictTrans2 . dictTrans0) dict
         in withDict dictEq (withDict dictOrd (Tabelle (M.intersectionWith id f a)))

instance (All Dimension xs, Semigroup r) => Semigroup (Tabelle xs r) where
    a <> b = liftA2 (<>) a b

instance (All Dimension xs, Monoid r) => Monoid (Tabelle xs r) where
    mempty = tabulate (const mempty)
    a `mappend` b = liftA2 mappend a b

fromList :: forall xs a. All Dimension xs => [(NP I xs, a)] -> Either (NP I xs) (Tabelle xs a)
fromList entries =
    let dict :: Dict (All Dimension) xs
        dict = Dict
        dictTrans0 :: forall z . Dict Dimension z -> Dict (Compose (Show `And` Eq `And` Ord) I) z 
        dictTrans0 d = withDict d Dict
        dictTrans1 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Eq I) z
        dictTrans1 d = withDict d Dict
        dictTrans2 :: forall z . Dict (Compose (Show `And` Eq `And` Ord) I) z -> Dict (Compose Ord I) z
        dictTrans2 d = withDict d Dict
        dictEq :: Dict (All (Compose Eq I)) xs
        dictEq = mapAll (dictTrans1 . dictTrans0) dict
        dictOrd :: Dict (All (Compose Ord I)) xs
        dictOrd = mapAll (dictTrans2 . dictTrans0) dict
     in withDict dictEq (withDict dictOrd (
            let mapita = M.fromList entries
                findKey k = case M.lookup k mapita of
                    Just v  -> Right (k,v)
                    Nothing -> Left  k 
            in  Tabelle . M.fromList <$> traverse findKey enumerate_NP))

fromList' :: forall r xs a. (IsProductType r xs, All Dimension xs) => [(r, a)] -> Either r (Tabelle xs a)
fromList' entries = 
      first (to . SOP . Z)
    . fromList  
    . fmap (first (unZ . unSOP . from))
    $ entries

toList :: forall xs a . All Dimension xs => Tabelle xs a -> [(NP I xs, a)]  
toList (Tabelle t) = M.toList t 

toList' :: forall r xs a . (IsProductType r xs, All Dimension xs) => Tabelle xs a -> [(r, a)]  
toList' (Tabelle t) = fmap (first (to . SOP . Z)) $ M.toList t 

enumerate_NP :: forall xs. (All Dimension xs) => [NP I xs]
enumerate_NP = 
      sequence_NP @xs 
    $ cpure_NP (Proxy @Dimension) (enumFromTo minBound maxBound)

