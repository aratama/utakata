module Utakata.Main (main) where

import AffUtil (makeAffWithNonCanceler)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Control.Bind (bind, discard)
import Control.Monad (pure, void)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(Right, Left))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component (Component, component)
import Halogen.HTML (HTML)
import Halogen.Query (action)
import Halogen.VDom.Driver (runUI)
import Prelude (unit, ($))
import Utakata.Eval (eval)
import Utakata.LocalStorage (loadStorage)
import Utakata.Render (render)
import Utakata.Type (AudioState(..), Input, Mode(..), Output, Query(Update, Open), Storage(Storage))
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

ui :: Storage -> Component HTML Query Input Output Aff
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

main :: Effect Unit
main = runHalogenAff do
    body <- awaitBody
    context <- liftEffect newAudioContext
    options <- liftEffect loadStorage

    let ops = case unwrap (runExceptT options) of 
            Right o -> o
            Left err -> Storage { 
                filePath: Nothing, 
                mode: "", 
                volume: 1.0
            }
        
    io <- runUI (ui ops) context body  
    case runExceptT options of 
        Identity (Right (Storage ops')) -> do 
            case ops'.filePath of 
                Just path -> io.query (action (Open path))
                _ -> pure unit
        _ -> pure unit 

    win <- liftEffect window
    _ <- tailRecM (\_ -> do
        makeAffWithNonCanceler \_ resolve -> void do 
            requestAnimationFrame (do 
                runHalogenAff $ io.query (action Update)
                resolve unit
            ) win
        pure (Loop unit)
    ) unit
    pure unit 






