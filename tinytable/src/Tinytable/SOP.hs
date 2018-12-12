{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Tinytable.SOP where

import           Control.Applicative
import           Data.Kind
import           Data.Proxy
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Generics.SOP (AllZip(..),LiftedCoercible(..),HasDatatypeInfo(..),DatatypeInfoOf(..),SListI2,hcoerce,Generic,Compose,All,And,NP,IsProductType,SOP(SOP),NS(Z),unSOP,unZ,from,to,I(..),K(..),IsEnumType,Code,projections,injections,mapKK,mapKKK,Injection,apFn,type (-.->)(..),hpure)
import           Generics.SOP.NP (liftA2_NP,collapse_NP,map_NP,ap_NP,liftA_NP,sequence_NP, cpure_NP, NP((:*),Nil))
import           Generics.SOP.NS
import           Generics.SOP.Dict
import           Generics.SOP.Type.Metadata
import qualified GHC.Generics as GHC

type AllC c xs = All (Generics.SOP.Compose c I) xs

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

type AliasesFor (ns :: [Symbol]) = NP (K Text) ns

type family ConstructorNamesOf (r :: Type) :: [Symbol] where
    ConstructorNamesOf r = MapGetConstructorName (GetConstructors (DatatypeInfoOf r))

type family GetConstructors (r :: DatatypeInfo) :: [ConstructorInfo] where
    GetConstructors (ADT moduleName datatypeName constructors) = constructors
    
type family MapGetConstructorName (r :: [ConstructorInfo]) :: [Symbol] where
    MapGetConstructorName '[] = '[]
    MapGetConstructorName (c : cs) = GetConstructorName c : MapGetConstructorName cs

type family GetConstructorName (r :: ConstructorInfo) :: Symbol where
    GetConstructorName (Constructor n) = n

enumValues :: forall r . IsEnumType r => NP (K r) (Code r)
enumValues =
  map_NP (mapKK (to . SOP))
         (apInjs'_NP (cpure_NP (Proxy @((~) '[])) Nil))

alternatives :: forall f r xss ns. 
                (Alternative f,
                 IsEnumType r, 
                 Code r ~ xss,
                 HasDatatypeInfo r,
                 ConstructorNamesOf r ~ ns,
                 All KnownSymbol ns,
                 AllZip (LiftedCoercible (K (f ())) (K (f ()))) ns xss) 
             => NP (K (f ())) ns
             -> f r
alternatives as = 
    let aliases = hcoerce as :: NP (K (f ())) xss
        mapped = liftA2_NP (mapKKK (\p x -> p *> pure x)) aliases (enumValues @r) :: NP (K (f r)) xss
     in asum (collapse_NP mapped)

data Foo = Bar | Baz deriving (Show,GHC.Generic)

instance Generic Foo
instance HasDatatypeInfo Foo
