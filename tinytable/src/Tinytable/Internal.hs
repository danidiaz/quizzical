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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Tinytable.Internal where

import           Data.Maybe (fromJust)
import           Data.Bifunctor (first)
import           Data.Kind
import           Data.Proxy
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Control.Applicative
import           Generics.SOP (SListI2,hcoerce,Generic,Compose,I,All,And,NP,IsProductType,SOP(SOP),NS(Z),unSOP,unZ,from,to,I(..),K(..),IsEnumType,Code,projections,injections,mapKK,Injection,apFn,type (-.->)(..),hpure)
import           Generics.SOP.NP (ap_NP,liftA_NP,sequence_NP, cpure_NP, NP((:*),Nil))
import           Generics.SOP.NS
import           Generics.SOP.Dict
import           Generics.SOP.Type.Metadata
import qualified Data.Map.Strict as M

import qualified GHC.Generics as GHC

newtype Tinytable (xs :: [Type]) r = Tinytable { getTinytable :: M.Map (NP I xs) r } 
                                     deriving (Functor,Foldable,Traversable)

type AllC c xs = All (Generics.SOP.Compose c I) xs

type Dimensions xs = (All Enum xs, All Bounded xs, AllC Eq xs, AllC Ord xs, AllC Show xs)

instance (Dimensions xs, Show r) => Show (Tinytable xs r) where
    show (Tinytable t) = "fromRight undefined (fromList " ++ show (M.toList t) ++ ")"

instance (Dimensions xs,Eq v) => Eq (Tinytable xs v) where
    tabelle1 == tabelle2 = and (liftA2 (==) tabelle1 tabelle2)

instance Dimensions xs => Distributive (Tinytable xs) where
    distribute (fmap getTinytable -> f) = Tinytable $ 
        M.fromList $ fmap (\k -> (k,fromJust . M.lookup k <$> f)) enumerate_NP

instance Dimensions xs => Representable (Tinytable xs) where
    type Rep (Tinytable xs) = NP I xs 
    tabulate f = Tinytable . M.fromList $ fmap (\np -> (np, f np)) enumerate_NP 
    index (Tinytable t) np = fromJust $ M.lookup np t

instance Dimensions xs => Applicative (Tinytable xs) where
    pure a = tabulate (pure a)
    Tinytable f <*> Tinytable a = Tinytable (M.intersectionWith id f a) 

instance (Dimensions xs, Semigroup r) => Semigroup (Tinytable xs r) where
    a <> b = liftA2 (<>) a b

instance (Dimensions xs, Monoid r) => Monoid (Tinytable xs r) where
    mempty = tabulate (const mempty)
    a `mappend` b = liftA2 mappend a b
   
indexR :: (IsProductType r xs, Dimensions xs) => Tinytable xs v -> r -> v 
indexR tt = index tt .  unZ . unSOP . from 

tabulateR :: (IsProductType r xs, Dimensions xs) => (r -> v) -> Tinytable xs v
tabulateR f = tabulate (\np -> f (to (SOP (Z (np)))))

fromList :: forall xs a. Dimensions xs => [(NP I xs, a)] -> Either (NP I xs) (Tinytable xs a)
fromList entries =
    let mapita = M.fromList entries
        findKey k = case M.lookup k mapita of
            Just v  -> Right (k,v)
            Nothing -> Left  k 
    in  Tinytable . M.fromList <$> traverse findKey enumerate_NP 

fromListR :: forall r xs a. (IsProductType r xs, Dimensions xs) => [(r, a)] -> Either r (Tinytable xs a)
fromListR entries = 
      first (to . SOP . Z)
    . fromList  
    . fmap (first (unZ . unSOP . from))
    $ entries

toList :: forall xs a . Dimensions xs => Tinytable xs a -> [(NP I xs, a)]  
toList (Tinytable t) = M.toList t 

toListR :: forall r xs a . (IsProductType r xs, Dimensions xs) => Tinytable xs a -> [(r, a)]  
toListR (Tinytable t) = fmap (first (to . SOP . Z)) $ M.toList t 

enumerate_NP :: forall xs. Dimensions xs => [NP I xs]
enumerate_NP = 
    -- sooo, only use `And` when passing proxies around, and avoid it otherwise?
    let dictZipped = zipAll (Dict :: Dict (All Enum) xs) 
                            (Dict :: Dict (All Bounded) xs)
        proxy = Proxy @(Enum `And` Bounded)
     in withDict dictZipped (sequence_NP @xs (cpure_NP proxy (enumFromTo minBound maxBound)))

type family Expand (xs :: [Type]) (v :: Type) :: Type where
    Expand '[a] v = a -> v
    Expand (a:b:cs) v = a -> Expand (b:cs) v

class Curry (xs :: [Type]) where
    curry_NP :: (NP I xs -> v) -> Expand xs v

instance Curry '[a] where
    curry_NP f a = f (I a :* Nil)

instance Curry (b:cs) => Curry (a:b:cs) where
    curry_NP f a = curry_NP (\np -> f (I a :* np))

class Uncurry (xs :: [Type]) where
    uncurry_NP ::  Expand xs v -> NP I xs -> v

instance Uncurry '[a] where
    uncurry_NP f (I a :* Nil) = f a

instance Uncurry (b:cs) => Uncurry (a:b:cs) where
    uncurry_NP f (I a :* np) = uncurry_NP (f a) np

indexC :: (Dimensions xs, Curry xs) => Tinytable xs v -> Expand xs v
indexC tt = curry_NP (index tt)

tabulateC :: (Dimensions xs, Uncurry xs) => Expand xs v -> Tinytable xs v
tabulateC = tabulate . uncurry_NP 

--
type AliasesFor ns = NP (K Text) ns

type family ConstructorNamesOf (r :: DatatypeInfo) :: [Symbol] where
    ConstructorNamesOf (ADT moduleName datatypeName constructors) = MapGetConstructorName constructors
    
type family MapGetConstructorName (r :: [ConstructorInfo]) :: [Symbol] where
    MapGetConstructorName '[] = '[]
    MapGetConstructorName (c : cs) = GetConstructorName c : MapGetConstructorName cs

type family GetConstructorName (r :: ConstructorInfo) :: Symbol where
    GetConstructorName (Constructor n) = n

-- What do I need? A way of producing a NP of   
-- of a um type. Constructing a n-ary product of all the values of a simple sum type.
-- The key: to (S S S())

--values :: forall r c. (Generic r, Code r ~ c, Foo c, SListI2 c) => NP (K r) c
--values = liftA_NP (mapKK (to . SOP))  (liftA_NP (\(Fn inj) -> inj _) injections)
-- values = liftA_NP (mapKK (to . SOP)) (ap_NP injections (hpure Nil))
--values = liftA_NP (mapKK (to . SOP)) foobar

values :: forall r c. (Generic r, Code r ~ c, POSN c) => NP (K r) c
values = liftA_NP (mapKK (to . SOP)) posn

-- products of sums of nil
class POSN xss where
    posn :: NP (K (NS (NP I) xss)) xss   
    
instance POSN '[] where
    posn = Nil

instance (SListI2 xss, POSN xss) => POSN ('[] ': xss) where
    posn = let previous = posn @xss
            in K (Z Nil) :* liftA_NP (mapKK S) previous

--changy :: forall xss . SListI2 xss => NP (K (NS (NP I) xss)) xss -> NP (K (NS (NP I) ('[] : xss))) xss 
--changy = liftA_NP (mapKK S)

--soopa :: forall xss. SListI2 xss => NS (NP I) xss -> NS (NP I) ('[] : xss)
--soopa ns = S ns

data Foo = Bar | Baz deriving (Show,GHC.Generic)

instance Generic Foo
