module Utakata.Main (main) where

import Control.Bind (bind, discard)
import Control.Monad (pure, void)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Halogen (Component)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component (component)
import Halogen.HTML (HTML(..))
import Halogen.Query (action)
import Halogen.VDom.Driver (runUI)
import Prelude (unit, ($))
import Utakata.Audio (createAudioContext)
import Utakata.LocalStorage (loadStorage)
import Utakata.Render (render)
import Utakata.Type (Effects, Input, Query(Update, Open), Storage(Storage), Output)
import Utakata.Eval (eval)

ui :: forall eff. Component HTML Query Input Output (Aff (Effects eff))
ui = component {
    render,
    eval,
    initialState: \context -> { 
        context,
        playing: false,
        filePath: Nothing,
        siblings: [],
        buffer: Nothing,
        source: Nothing,
        position: 0.0
    },
    receiver: \_ -> Nothing
}


main :: forall eff. Eff (Effects eff) Unit
main = runHalogenAff do
    body <- awaitBody
    context <- liftEff createAudioContext
    options <- liftEff loadStorage
    io <- runUI ui context body  
    case runExceptT options of 
        Identity (Right (Storage ops)) -> do 
            case ops.filePath of 
                NullOrUndefined (Just path) -> io.query (action (Open path))
                _ -> pure unit
        _ -> pure unit 

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

