module LocalStorage (STORAGE, loadStorage, saveStorage, loadStorage', saveStorage') where 

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Except.Trans (ExceptT)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecodeJSON, genericEncodeJSON)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit)
import Prelude ((<$>))

foreign import data STORAGE :: Symbol -> Type -> Effect

foreign import loadStorage_ :: forall key a eff. String -> Eff (storage :: STORAGE key a | eff) String

foreign import saveStorage_ :: forall key a eff. String -> String -> Eff (storage :: STORAGE key a | eff) Unit

loadStorage :: forall key a eff. IsSymbol key => Decode a => Eff (storage :: STORAGE key a | eff) (ExceptT (NonEmptyList ForeignError) Identity a)
loadStorage = decodeJSON <$> loadStorage_ (reflectSymbol (SProxy :: SProxy key)) 

saveStorage :: forall key a eff. IsSymbol key => Encode a => a -> Eff (storage :: STORAGE key a | eff) Unit
saveStorage a = saveStorage_ (reflectSymbol (SProxy :: SProxy key)) (encodeJSON a)


loadStorage' :: forall key a rep eff. IsSymbol key => Generic a rep => GenericDecode rep => Eff (storage :: STORAGE key a | eff) (ExceptT (NonEmptyList ForeignError) Identity a)
loadStorage' = genericDecodeJSON defaultOptions { unwrapSingleConstructors = true } <$> loadStorage_ (reflectSymbol (SProxy :: SProxy key)) 


saveStorage' :: forall key a rep eff. IsSymbol key => Generic a rep => GenericEncode rep => a -> Eff (storage :: STORAGE key a | eff) Unit
saveStorage' a = saveStorage_ (reflectSymbol (SProxy :: SProxy key)) (genericEncodeJSON defaultOptions { unwrapSingleConstructors = true } a)

