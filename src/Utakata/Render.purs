module Utakata.Render (render) where

import DOM.Event.KeyboardEvent (key)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (mapWithIndex)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.Formatter.Number (format)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.String.Regex (match, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i, input, option, select)
import Halogen.HTML.Events (input_, onClick, onKeyDown, onValueChange)
import Halogen.HTML.Properties (InputType(..), class_, max, min, selected, step, type_, value)
import Node.Path (FilePath, basename, basenameWithoutExt, dirname, extname)
import Prelude (negate, ($), (<$>), (==))
import Utakata.Type (Query(..), State)

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
        button [ onClick (input_ Stop) ] [icon "stop"],
        button [ onClick (input_ (Move (negate 1))) ] [icon "backward"],
        case state.playing of 
            false -> button [ onClick (input_ Play) ] [icon "play"]
            true -> button [ onClick (input_ Pause) ] [icon "pause"],
        button [ onClick (input_ (Move 1)) ] [icon "forward"], 
        button [ onClick (input_ OpenFileDialog) ] [icon "volume-up"]
    ], 
    div [] [
        input [type_ InputRange, min 0.0, max 1.0, step (Step 0.001), value (show state.position)]
    ]
] 
  where 
    renderOption :: forall p i. Int -> FilePath -> HTML p i    
    renderOption i entry = option [
        value entry, 
        selected $ Just entry == (basename <$> state.filePath)
    ] [ 
        let name = basenameWithoutExt entry (extname entry) in 
        case regex "^\\d+ " noFlags of 
            Left err -> text "internal error" 
            Right pattern -> text if test pattern name then name else formatInt (toNumber (i + 1)) <> " " <> name     
    ]

    formatInt i = format {
        comma: false,
        before: 2, 
        after: 0, 
        abbreviations: false, 
        sign: false
    } i 

