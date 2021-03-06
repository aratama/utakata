module Utakata.Render (render) where

import Audio.WebAudio.Extra (getDuration)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (mapWithIndex)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.EuclideanRing (mod)
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Show (show)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Global (readFloat)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i, input, option, select, span)
import Halogen.HTML.Events (input_, onClick, onKeyDown, onValueChange, onValueInput)
import Halogen.HTML.Properties (InputType(InputRange), class_, max, min, selected, step, type_, value)
import Node.Path (FilePath, basename, basenameWithoutExt, dirname, extname)
import Prelude (div) as Prelude
import Prelude (negate, not, ($), (<), (<$>), (==))
import Utakata.Type (AudioState(..), Mode(..), Query(..), State)
import Web.UIEvent.KeyboardEvent (key)


icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

render :: State -> HTML Void (Query Unit)
render state = div [
    onKeyDown \e -> case key e of 
        "d" -> Just (OpenDevTools unit)
        _ -> Nothing 
] [

    div [class_ (ClassName "top-row")] [
        button [class_ (ClassName "close-button"), onClick (input_ Minimize) ] [icon "window-minimize"],
        button [class_ (ClassName "close-button"), onClick (input_ Close) ] [icon "close"]    
    ], 

    div [class_ (ClassName "file-row")] [
        case state.mode of 
            RepeatOff -> modeButton RepeatOne "long-arrow-right"
            RepeatOne -> modeButton RepeatAll "repeat"
            RepeatAll -> modeButton Random "refresh"
            Random -> modeButton RepeatOff "random", 
        select [
            class_ (ClassName "audio-title"), 
            onValueChange \value -> case state.filePath of 
                Nothing -> Nothing 
                Just path -> Just (Open (dirname path <> "/" <> value) unit)
        ] (mapWithIndex renderOption state.siblings),
        button [
            class_ (ClassName "open"), 
            onClick (input_ ShowOpenDialog) 
        ] [icon "folder-open"]
    ],
    div [class_ (ClassName "controls")] [

        span [class_ (ClassName "position")] [
            let seconds = floor $ state.position in 
                text $ formatInt (Prelude.div seconds 60) <> ":" <> formatInt (mod seconds 60)],      
        div [class_ (ClassName "spacer")] [],        
        
        button [ onClick (input_ (Move (negate 1))) ] [icon "backward"],
        if state.playing 
            then button [ onClick (input_ Pause) ] [icon "pause"]
            else button [ onClick (input_ Play) ] [icon "play"],
        button [ onClick (input_ (Move 1)) ] [icon "forward"], 
        div [class_ (ClassName "spacer")] [],        
        button [ onClick (input_ (SetMute (not state.muted)))] [icon if state.muted then "volume-off" else "volume-up"], 
        input [
            class_ (ClassName "volume"), 
            type_ InputRange, 
            min 0.0, 
            max 1.0, 
            step (Step 0.01), 
            value (show state.volume), 
            onValueChange \value -> Just (SetVolume (readFloat value) unit), 
            onValueInput \value -> Just (SetVolume (readFloat value) unit)
        ]
    ], 
    div [class_ (ClassName "timeline")] [
        input [
            type_ InputRange, 
            min 0.0, 
            max case state.audio of 
                NotLoaded -> 1.0 
                Loaded { buffer } -> getDuration buffer  
                PlayingAudio { buffer } -> getDuration buffer, 
            step (Step 0.001), 
            value (show state.position), 
            onValueChange \value -> Just (SetPosition (readFloat value) unit), 
            onValueInput \value -> Just (SetPosition (readFloat value) unit)
        ]
    ]
] 
  where 
    modeButton next i = button [ onClick (input_ (SetMode next)) ] [icon i]

    renderOption :: forall p i. Int -> FilePath -> HTML p i    
    renderOption i entry = option [
        value entry, 
        selected $ Just entry == (basename <$> state.filePath)
    ] [ 
        let name = basenameWithoutExt entry (extname entry) in
        text case regex "^\\d+ " noFlags of 
            Left err -> "internal error" 
            Right pattern -> " " <> if test pattern name then name else formatInt (i + 1) <> " " <> name
    ]

    formatInt :: Int -> String
    formatInt n = if n < 10 then "0" <> show n else show n
