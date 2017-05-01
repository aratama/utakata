module Utakata.Render (render) where

import DOM.Event.KeyboardEvent (key)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (mapWithIndex)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.EuclideanRing (mod)
import Data.Formatter.Number (format)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Global (readFloat, readInt)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i, input, option, select, span)
import Halogen.HTML.Events (input_, onClick, onKeyDown, onValueChange, onValueInput)
import Halogen.HTML.Properties (InputType(..), class_, disabled, max, min, selected, step, type_, value)
import Node.Path (FilePath, basename, basenameWithoutExt, dirname, extname)
import Prelude (negate, not, ($), (-), (<$>), (<<<), (==))
import Prelude (div) as Prelude
import Utakata.Audio (getDuration)
import Utakata.Type (Audio(..), Mode(..), Query(..), State)


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
            onClick (input_ OpenFileDialog) 
        ] [icon "folder-open"]
    ],
    div [class_ (ClassName "controls")] [

        span [class_ (ClassName "position")] [
            let seconds = floor $ state.position in 
                text $ show (Prelude.div seconds 60) <> ":" <> formatInt (mod seconds 60)],      
        div [class_ (ClassName "spacer")] [],        
        
        button [ onClick (input_ (Move (negate 1))) ] [icon "backward"],
        case state.audio of
            NotLoaded -> button [ onClick (input_ Play), disabled true] [icon "play"]
            Loaded _ -> button [ onClick (input_ Play) ] [icon "play"]
            PlayingAudio _ -> button [ onClick (input_ Pause) ] [icon "pause"],
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
        input 
            [type_ InputRange, 
            min 0.0, 
            max case state.audio of 
                NotLoaded -> 1.0 
                Loaded { buffer } -> getDuration buffer  
                PlayingAudio { buffer } -> getDuration buffer, 
            step (Step 0.001), value (show state.position)
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
    formatInt = format {
        comma: false,
        before: 2, 
        after: 0, 
        abbreviations: false, 
        sign: false
    } <<< toNumber

