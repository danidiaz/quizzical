-- | This module defines a multi-dimensional table type for dimensions that
--   have a finite number of values.
-- 
--   Be careful with the number and size of the dimensions or you will risk a
--   combinatorial explosion!
module Tinytable (
      -- * the Tinytable data type
      Tinytable
    , Dimensions
    , AllC
    -- * from and to NP 
    , fromList
    , toList
    -- * working with nominal records
    , indexR
    , tabulateR
    , fromListR
    , toListR
    -- * curried/uncurried functions
    , Expand
    , indexC
    , tabulateC
    -- * serialization helpers
    , AliasesFor
    , ConstructorNamesOf 
    , values
    , Foo
    -- * generics-sop re-rexports
    , NP(..)
    , I(..)
    , All
    , Compose
    ) where
import           Generics.SOP (NP(..),I(..),All,Compose)

import           Tinytable.Internal


