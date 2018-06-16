module Utakata.LocalStorage (loadStorage, saveStorage) where 

import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Foreign (F, ForeignError(..), fail)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Prelude (bind, pure, (>>=))

storageKey :: String
storageKey = "Utakata.Storage"

loadStorage :: forall a. Decode a => Effect (F a)
loadStorage = do 
    str :: Maybe String <- window >>= localStorage >>= getItem storageKey
    pure case str of 
        Nothing -> fail (ForeignError "No option data in the local storage.")
        Just str' -> decodeJSON str'

saveStorage :: forall a. Encode a => a -> Effect Unit
saveStorage a = window >>= localStorage >>= setItem storageKey (encodeJSON a)
