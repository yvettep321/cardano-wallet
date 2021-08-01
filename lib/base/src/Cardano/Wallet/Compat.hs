-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Adds back missing functions.

module Cardano.Wallet.Compat
    ( (^?)
    ) where

import Control.Applicative
    ( Const (..) )
import Data.Monoid
    ( First (..) )
import Data.Profunctor.Unsafe
    ( ( #. ) )
import Prelude
    ( Maybe (..) )

-- | Get the first item of a traversal, if it exists.
infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)
