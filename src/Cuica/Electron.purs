module Cuica.Electron where 


import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

foreign import close :: forall eff. Eff (dom :: DOM | eff) Unit

