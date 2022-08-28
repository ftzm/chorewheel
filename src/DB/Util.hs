module DB.Util where

import qualified Data.Set.NonEmpty as NESet

loadNESetUnsafe :: Ord a => [a] -> NESet.NESet a
loadNESetUnsafe = NESet.fromList . fromList
