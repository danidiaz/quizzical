-- | This module defines a multi-dimensional table type for dimensions that
--   have a finite number of values.
-- 
--   Be careful with the number and size of the dimensions or you will risk a
--   combinatorial explosion!
module Tinytable (
      -- * the Tinytable data type
      Tinytable
    , Dimensions
    -- * from and to lists of entries
    , fromList
    , toList
    -- * working with nominal records as keys
    , indexR
    , tabulateR
    , fromListR
    , toListR
    -- * curried/uncurried functions
    , indexC
    , tabulateC
    -- * serialization helpers
    , Foo
    -- * generics-sop re-rexports
    , NP(..)
    , I(..)
    , K(..)
    , All
    , Compose
    -- * generics-sop extras
    , AllC
    , Expand
    , Curry
    , Uncurry
    , AliasesFor
    , enumValues
    , alternatives
    ) where
import           Generics.SOP (NP(..),I(..),All,Compose)

import           Tinytable.Internal


