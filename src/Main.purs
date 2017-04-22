module Main where

import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Cuica.Audio (createAudioContext)
import Data.Unit (Unit)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Query (action)
import Halogen.VDom.Driver (runUI)
import Type (Effects, Query(OpenDirectory))
import UI (ui)

main :: forall eff. Eff (Effects eff) Unit
main = runHalogenAff do
    body <- awaitBody
    context <- liftEff createAudioContext
    runUI ui context body
