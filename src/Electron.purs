module Electron (close, minimize, showOpenDialog, home, openDevTools) where 

import AffUtil (makeAffWithNonCanceler)
import Control.Apply ((<$>))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Exception (Error)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Node.Path (FilePath)

foreign import close :: Effect Unit

foreign import minimize :: Effect Unit

foreign import showOpenDialogEff :: (Error -> Effect Unit) -> (Nullable FilePath -> Effect Unit) -> Effect Unit 
    
showOpenDialog :: forall m. MonadAff m => m (Maybe FilePath)
showOpenDialog = toMaybe <$> liftAff (makeAffWithNonCanceler showOpenDialogEff)

foreign import openDevTools :: Effect Unit

foreign import home :: FilePath






