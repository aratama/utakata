module Utakata.Type where

import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Void (Void)
import Halogen.Aff.Effects (HalogenEffects)
import Node.FS.Aff (FS)
import Node.Path (FilePath)
import Utakata.Audio (AudioBuffer, AudioContext, AudioGraph)
import Utakata.LocalStorage (STORAGE)

type State = {
    context :: AudioContext,
    playing :: Boolean,
    filePath :: Maybe FilePath, 
    siblings :: Array FilePath,
    buffer :: Maybe AudioBuffer,
    source :: Maybe AudioGraph,
    position :: Number,
    mode :: Mode,
    volume :: Number,
    muted :: Boolean
}

data Mode = RepeatOff | RepeatOne | RepeatAll | Random

data Query a = OpenFileDialog a
             | Open FilePath a
             | Play a
             | Pause a 
             | Stop a
             | Update a
             | Minimize a
             | Move Int a 
             | Close a
             | OpenDevTools a
             | SetMode Mode a
             | SetMute Boolean a
             | SetVolume Number a
             | End a

type Input = AudioContext

type Output = Void

type Effects eff = HalogenEffects (
    fs :: FS, 
    console :: CONSOLE,
    storage :: STORAGE "Utakata.Storage" Storage
        | eff)

newtype Storage = Storage {
    filePath :: NullOrUndefined FilePath
}

derive instance genericStorage :: Generic Storage _


