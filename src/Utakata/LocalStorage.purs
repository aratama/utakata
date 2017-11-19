module Utakata.LocalStorage (loadStorage, saveStorage) where 

import Control.Monad.Eff (Eff, kind Effect)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (getItem, setItem)
import Data.Foreign (F, ForeignError(..), fail)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Prelude (bind, pure, (>>=))

storageKey :: String
storageKey = "Utakata.Storage"

loadStorage :: forall a eff. Decode a => Eff (dom :: DOM | eff) (F a)
loadStorage = do 
    str :: Maybe String <- window >>= localStorage >>= getItem storageKey
    pure case str of 
        Nothing -> fail (ForeignError "No option data in the local storage.")
        Just str' -> decodeJSON str'

saveStorage :: forall a eff. Encode a => a -> Eff (dom :: DOM | eff) Unit
saveStorage a = window >>= localStorage >>= setItem storageKey (encodeJSON a)
