-- | This module defines a multi-dimensional table type for dimensions that
--   have a finite number of values.
-- 
--   The motivating use case was to represent verb conjugations.
--
--   Be careful with the number and size of the dimensions or you will risk a
--   combinatorial explosion!
module Tablita (
      -- * The Tablita data type
      Tablita
    , Dimensions
    , fromList
    , fromList'
    , toList
    , toList'
    ) where

import Tablita.Internal


