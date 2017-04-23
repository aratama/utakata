module Render where

import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i, input)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (InputType(..), class_, max, min, step, type_, value)
import Type (Query(..), State)


icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

render :: State -> HTML Void (Query Unit)
render state = div [] [
    div [class_ (ClassName "top-row")] [
        button [class_ (ClassName "close-button"), onClick (input_ Close) ] [icon "close"]    
    ], 
    div [class_ (ClassName "file-row")] [
        div [class_ (ClassName "audio-title")] [text state.title],
        button [class_ (ClassName "open"), onClick (input_ OpenFileDialog) ] [icon "folder-open"]
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
