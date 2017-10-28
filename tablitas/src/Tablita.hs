-- | This module defines a multi-dimensional table type for dimensions that
--   have a finite number of values.
-- 
--   The motivating use case was to represent verb conjugations.
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


