{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Tablita where 

import qualified Generics.SOP as G
import Data.Distributive
import Data.Functor.Rep
import qualified Data.Map.Strict as M

newtype Tablita (xs::[*]) r = Tablita (M.Map (G.NP G.I xs) r) 
                              deriving (Functor,Foldable,Traversable)

instance (G.All (G.Compose Show G.I) xs,Show r) => Show (Tablita xs r) where
    show (Tablita m) = show m 

instance (G.All (G.Compose (Ord `G.And` Enum `G.And` Bounded) G.I) xs) => Distributive (Tablita xs) where
    distribute = undefined

instance (G.All (G.Compose (Ord `G.And` Enum `G.And` Bounded) G.I) xs) => Representable (Tablita xs) where
    tabulate = undefined
    index = undefined
