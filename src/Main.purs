module Main where

import Control.Bind (bind, discard)
import Control.Monad (pure, void)
import Control.Monad.Aff (delay, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Cuica.Audio (createAudioContext)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Unit (Unit)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Query (action)
import Halogen.VDom.Driver (runUI)
import Prelude (unit, ($))
import Type (Effects, Query(OpenDirectory, Update))
import UI (ui)

main :: forall eff. Eff (Effects eff) Unit
main = runHalogenAff do
    body <- awaitBody
    context <- liftEff createAudioContext
    io <- runUI ui context body  
    win <- liftEff window
    _ <- tailRecM (\_ -> do
        makeAff \_ resolve -> void do 
            requestAnimationFrame (do 
                runHalogenAff $ io.query (action Update)
                resolve unit
            ) win
        pure (Loop unit)
    ) unit
    pure unit 

