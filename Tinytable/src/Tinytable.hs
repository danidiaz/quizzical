-- | This module defines a multi-dimensional table type for dimensions that
--   have a finite number of values.
-- 
--   Be careful with the number and size of the dimensions or you will risk a
--   combinatorial explosion!
module Tinytable (
      -- * The Tinytable data type
      Tinytable
    , Dimensions
    , AllC
    -- * from and to NP 
    , fromList
    , toList
    -- * from and to records
    , fromList'
    , toList'
    -- * Utility classes and functions
    , Curry(..)
    -- * generics-sop re-rexports
    , NP(..)
    , I(..)
    ) where
import           Generics.SOP (NP(..),I(..))

import           Tinytable.Internal


