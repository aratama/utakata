module Utakata.Render where

import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (mapWithIndex)
import Data.Formatter.Number (format)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i, input, option, select)
import Halogen.HTML.Events (input_, onClick, onValueChange)
import Halogen.HTML.Properties (InputType(..), class_, max, min, selected, step, type_, value)
import Node.Path (FilePath, basename, basenameWithoutExt, dirname, extname)
import Prelude (($), (<$>), (==))
import Utakata.Type (Query(..), State)

icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

render :: State -> HTML Void (Query Unit)
render state = div [] [
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
        ] (mapWithIndex renderOption state.files),
        button [
            class_ (ClassName "open"), 
            onClick (input_ OpenFileDialog) 
        ] [icon "folder-open"]
    ],
    div [class_ (ClassName "controls")] [
        button [ onClick (input_ OpenFileDialog) ] [icon "backward"],
        case state.source of 
            Nothing -> button [ onClick (input_ Play) ] [icon "play"]
            Just _ -> button [ onClick (input_ Pause) ] [icon "pause"],            
        button [ onClick (input_ Stop) ] [icon "stop"],
        button [ onClick (input_ OpenFileDialog) ] [icon "forward"]
    ], 
    div [] [
        input [type_ InputRange, min 0.0, max 1.0, step (Step 0.001), value (show state.position)]
    ]
]
  where 
    renderOption :: forall p i. Int -> { path :: FilePath, title :: Maybe String } -> HTML p i    
    renderOption i entry = option [
        value entry.path, 
        selected $ Just entry.path == (basename <$> state.filePath)
    ] [ 
        text $ formatInt (toNumber i) <> " " <> fromMaybe (basenameWithoutExt entry.path (extname entry.path)) entry.title    
    ]

    formatInt i = format {
        comma: false,
        before: 2, 
        after: 0, 
        abbreviations: false, 
        sign: false
    } i 

