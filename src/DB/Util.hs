module DB.Util where

import Contravariant.Extras.Contrazip
import Control.Monad.Catch
import Data.Set.NonEmpty qualified as NESet
import Data.Text (isInfixOf)
import Data.Vector qualified as V
import Hasql.Encoders qualified as E
import Hasql.Pool
import Hasql.Session

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

vectorEncoder2 ::
  E.Value a ->
  E.Value b ->
  E.Params (V.Vector (a, b))
vectorEncoder2 v1 v2 =
  contramap V.unzip $ contrazip2 (vectorParam v1) (vectorParam v2)

vectorEncoder3 ::
  E.Value a ->
  E.Value b ->
  E.Value c ->
  E.Params (V.Vector (a, b, c))
vectorEncoder3 v1 v2 v3 =
  contramap V.unzip3 $
    contrazip3 (vectorParam v1) (vectorParam v2) (vectorParam v3)

vectorEncoder4 ::
  E.Value a ->
  E.Value b ->
  E.Value c ->
  E.Value d ->
  E.Params (V.Vector (a, b, c, d))
vectorEncoder4 v1 v2 v3 v4 =
  contramap V.unzip4 $
    contrazip4 (vectorParam v1) (vectorParam v2) (vectorParam v3) (vectorParam v4)

vectorEncoder4N ::
  E.NullableOrNot E.Value a ->
  E.NullableOrNot E.Value b ->
  E.NullableOrNot E.Value c ->
  E.NullableOrNot E.Value d ->
  E.Params (V.Vector (a, b, c, d))
vectorEncoder4N v1 v2 v3 v4 =
  contramap V.unzip4 $
    contrazip4 (vectorParamN v1) (vectorParamN v2) (vectorParamN v3) (vectorParamN v4)

mapSqlError :: (Exception e, MonadCatch m) => [(Text, e)] -> m a -> m a
mapSqlError pairs act =
  catches
    act
    [ Handler $ \e -> case e of
        SessionUsageError
          (QueryError _ _ (ResultError (ServerError _ errStr _ _ _))) ->
            throwIfSubstr e errStr
        otherError -> throwM otherError
    , Handler $ \e -> case e of
        QueryError _ _ (ResultError (ServerError _ errStr _ _ _)) ->
          throwIfSubstr e errStr
        otherError -> throwM otherError
    ]
 where
  throwIfSubstr e errStr =
    case [e' | (s, e') <- pairs, s `isInfixOf` decodeUtf8 errStr] of
      match : _ -> throwM match
      [] -> throwM e
