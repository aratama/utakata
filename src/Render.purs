module Render where

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i, input)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (InputType(..), class_, type_, value)
import Type (Query(..), State)


icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

render :: State -> HTML Void (Query Unit)
render state = div [] [
    button [ onClick (input_ OpenDirectory) ] [icon "folder-open"],
    text state.directory,
    div [] [
        button [ onClick (input_ OpenDirectory) ] [icon "backward"],
        case state.source of 
            Nothing -> button [ onClick (input_ Play) ] [icon "play"]
            Just _ -> button [ onClick (input_ Pause) ] [icon "pause"],            
        button [ onClick (input_ Stop) ] [icon "stop"],
        button [ onClick (input_ OpenDirectory) ] [icon "forward"]
    ], 
    div [] [
        input [type_ InputRange, value (show state.position)]
    ]
]
