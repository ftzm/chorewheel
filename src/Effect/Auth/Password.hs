module Effect.Auth.Password where

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Hasql.Session as HS
import Data.Text.Encoding
import Data.Bool
import Control.Monad.IO.Class

import Models
import DB.Password

validateBasicAuth :: Username -> Password -> HS.Session (Maybe UserId)
validateBasicAuth u p = do
  result <-  HS.statement u passwordInfoByUsername
  pure $ result >>= \(i, dbPasswordHash) ->
    bool Nothing (Just i) $ validatePassword
     (unPassword p) (encodeUtf8 $ unPasswordHash dbPasswordHash)

createPassword :: UserId -> Password -> HS.Session ()
createPassword u (Password p) = do
  h <- liftIO $ PasswordHash . decodeUtf8 <$> hashPassword 10 p
  HS.statement (u, h) insertPassword
