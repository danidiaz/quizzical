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
    , AllC
    , Dimensions
    , fromList
    , fromList'
    , toList
    , toList'
    -- * from generics-sop
    , NP(..)
    , I(..)
    ) where
import           Generics.SOP (NP(..),I(..))

import           Tabelle.Internal


