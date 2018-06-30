-- | This module defines a multi-dimensional table type for dimensions that
--   have a finite number of values.
-- 
--   The motivating use case was to represent verb conjugations.
--
--   Be careful with the number and size of the dimensions or you will risk a
--   combinatorial explosion!
module Tabelle (
      -- * The Tabelle data type
      Tabelle
    , Dimensions
    , AllC
    -- * from and to NP 
    , fromList
    , toList
    -- * from and to records
    , fromList'
    , toList'
    -- * generics-sop re-rexports
    , NP(..)
    , I(..)
    ) where
import           Generics.SOP (NP(..),I(..))

import           Tabelle.Internal


