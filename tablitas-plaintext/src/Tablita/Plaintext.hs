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
module Tablita.Plaintext where

import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Generics.SOP (All)

import Tablita

type Parser = Parsec Void Text

parse2DFile :: (Dimensions xs, All Read xs, xs ~ [x1,x2]) => String -> Tablita xs String
parse2DFile = undefined

