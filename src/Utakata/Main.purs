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
import Data.Either (Either(Right, Left))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Unit (Unit)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component (Component, component)
import Halogen.HTML (HTML)
import Halogen.Query (action)
import Halogen.VDom.Driver (runUI)
import Prelude (unit, ($))
import Audio (createAudioContext)
import Utakata.Eval (eval)
import Utakata.LocalStorage (loadStorage)
import Utakata.Render (render)
import Utakata.Type (AudioState(..), Effects, Input, Mode(..), Output, Query(Update, Open), Storage(Storage))

ui :: forall eff. Storage -> Component HTML Query Input Output (Aff (Effects eff))
ui (Storage options) = component {
    render,
    eval,
    initialState: \context -> { 
        context,
        filePath: Nothing,
        siblings: [],
        randoms: [],
        position: 0.0,
        audio: NotLoaded, 
        playing: false,
        mode: case options.mode of 
            "RepeatOff" -> RepeatOff
            "RepeatOne" -> RepeatOne
            "Random" -> Random            
            _ -> RepeatAll,
        volume: options.volume,
        muted: false
    },
    receiver: \_ -> Nothing
}

main :: forall eff. Eff (Effects eff) Unit
main = runHalogenAff do
    body <- awaitBody
    context <- liftEff createAudioContext
    options <- liftEff loadStorage

    let ops = case unwrap (runExceptT options) of 
            Right o -> o
            Left err -> Storage { 
                filePath: NullOrUndefined Nothing, 
                mode: "", 
                volume: 1.0
            }
        
    io <- runUI (ui ops) context body  
    case runExceptT options of 
        Identity (Right (Storage ops')) -> do 
            case ops'.filePath of 
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



