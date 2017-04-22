module Render where

import Data.Monoid ((<>))
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (button, div, i)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_)
import Type (Query(..), State)


icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

render :: State -> HTML Void (Query Unit)
render state = div [] [
    button [ onClick (input_ OpenDirectory) ] [icon "folder-open"],
    text state.directory,
    div [] [
        button [ onClick (input_ OpenDirectory) ] [icon "backward"],
        button [ onClick (input_ if state.playing then Pause else Play) ] [icon if state.playing then "pause" else "play"],
        button [ onClick (input_ OpenDirectory) ] [icon "forward"]
    ]
]
