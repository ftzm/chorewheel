{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Fam where

import Data.Kind
import Data.Functor.Identity
import Optics.TH
import Optics.Getter (view)


type Flex :: Type -> Type
type family Flex a where
  Flex (Identity x) = x
  Flex (Maybe x) = Maybe x

a :: Flex (Identity Int)
a = 10

-- b :: Flex (Maybe Int)
-- b = Just 10

data Oi = Oi { _test :: String }

makeLenses ''Oi

data R' f = R'
  { first :: Flex (f String)
  , second :: Flex (f Int)
  }

makeFieldLabels ''R'

type R = R' Identity


r1 :: R' Maybe
r1 = R' Nothing (Just 1)

r2 :: R' Identity
r2 = R' "oi" 1

r3 :: R
r3 = R' "oi" 1

o1 :: Oi
o1 = Oi "oi"

o1f :: String
o1f = view test o1
-- >>> o1f
-- "oi"

-- r1f :: Maybe Int
-- r1f = view #second r1
--
-- r3f :: String
-- r3f = view #first r3


-- >>> r3f
-- "oi"
