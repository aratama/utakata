module Cuica.LocalStorage (STORAGE, loadStorage, saveStorage) where 

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Except.Trans (ExceptT)
import Data.Foreign (Foreign, ForeignError)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit)
import Prelude ((<$>))

foreign import data STORAGE :: Symbol -> Type -> Effect

foreign import loadStorage_ :: forall key a eff. String -> Eff (storage :: STORAGE key a | eff) Foreign

foreign import saveStorage_ :: forall key a eff. String -> Foreign -> Eff (storage :: STORAGE key a | eff) Unit

loadStorage :: forall key a eff. IsSymbol key => Decode a => Eff (storage :: STORAGE key a | eff) (ExceptT (NonEmptyList ForeignError) Identity a)
loadStorage = decode <$> loadStorage_ (reflectSymbol (SProxy :: SProxy key)) 

saveStorage :: forall key a eff. IsSymbol key => Encode a => a -> Eff (storage :: STORAGE key a | eff) Unit
saveStorage a = saveStorage_ (reflectSymbol (SProxy :: SProxy key)) (encode a)

