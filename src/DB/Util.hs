module DB.Util where

import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector as V
import qualified Hasql.Encoders as E
import Contravariant.Extras.Contrazip

loadNESetUnsafe :: Ord a => [a] -> NESet.NESet a
loadNESetUnsafe = NESet.fromList . fromList

loadNESetUnsafeV :: Ord a => V.Vector a -> NESet.NESet a
loadNESetUnsafeV = NESet.fromList . fromList . V.toList

vectorParam :: E.Value b -> E.Params (V.Vector b)
vectorParam =
  E.param
    . E.nonNullable
    . E.array
    . E.dimension V.foldl'
    . E.element
    . E.nonNullable

vectorParamN :: E.NullableOrNot E.Value b -> E.Params (V.Vector b)
vectorParamN =
  E.param
    . E.nonNullable
    . E.array
    . E.dimension V.foldl'
    . E.element

vectorEncoder2
  :: E.Value a
  -> E.Value b
  -> E.Params (V.Vector (a, b))
vectorEncoder2 v1 v2 =
  contramap V.unzip $ contrazip2 (vectorParam v1) (vectorParam v2)

vectorEncoder3
  :: E.Value a
  -> E.Value b
  -> E.Value c
  -> E.Params (V.Vector (a, b, c))
vectorEncoder3 v1 v2 v3 =
  contramap V.unzip3
  $ contrazip3 (vectorParam v1) (vectorParam v2) (vectorParam v3)

vectorEncoder4
  :: E.Value a
  -> E.Value b
  -> E.Value c
  -> E.Value d
  -> E.Params (V.Vector (a, b, c, d))
vectorEncoder4 v1 v2 v3 v4 =
  contramap V.unzip4
  $ contrazip4 (vectorParam v1) (vectorParam v2) (vectorParam v3) (vectorParam v4)

vectorEncoder4N
  :: E.NullableOrNot E.Value a
  -> E.NullableOrNot E.Value b
  -> E.NullableOrNot E.Value c
  -> E.NullableOrNot E.Value d
  -> E.Params (V.Vector (a, b, c, d))
vectorEncoder4N v1 v2 v3 v4 =
  contramap V.unzip4
  $ contrazip4 (vectorParamN v1) (vectorParamN v2) (vectorParamN v3) (vectorParamN v4)
