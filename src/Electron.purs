module Electron (close, minimize, showOpenDialog, home, openDevTools) where 

import Control.Apply ((<$>))
import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Node.Path (FilePath)

foreign import close :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import minimize :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import showOpenDialogEff :: forall eff. (Error -> Eff (dom :: DOM | eff) Unit) -> (Nullable FilePath -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit 

showOpenDialog :: forall m eff. MonadAff (dom :: DOM | eff) m => m (Maybe FilePath)
showOpenDialog = toMaybe <$> liftAff (makeAff showOpenDialogEff)

foreign import openDevTools :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import home :: FilePath
