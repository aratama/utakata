module Utakata.Type where

import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import Data.Void (Void)
import Halogen.Aff.Effects (HalogenEffects)
import Node.FS.Aff (FS)
import Node.Path (FilePath)
import Utakata.Audio (AudioBuffer, AudioContext, AudioGraph, AudioTime)
import Utakata.LocalStorage (STORAGE)

type State = {
    context :: AudioContext,
    filePath :: Maybe FilePath,
    siblings :: Array FilePath,
    position :: Number,
    audio :: Audio,
    playing :: Boolean,
    mode :: Mode,
    volume :: Number,
    muted :: Boolean,
    history :: Array FilePath
}

data Audio = NotLoaded | 
             Loaded { buffer :: AudioBuffer } | 
             PlayingAudio { 
                 buffer :: AudioBuffer, 
                 source :: AudioGraph, 
                 playStart :: AudioTime,
                 startPosition :: Number, 
                 currentTime :: AudioTime 
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
             | SetPosition Number a 
             | End a

type Input = AudioContext

type Output = Void

type Effects eff = HalogenEffects (
    fs :: FS, 
    console :: CONSOLE,
    storage :: STORAGE "Utakata.Storage" Storage
        | eff)

newtype Storage = Storage {
    filePath :: NullOrUndefined FilePath,
    mode :: String, 
    volume :: Number
}

derive instance genericStorage :: Generic Storage _

derive instance genericMode :: Generic Mode _ 

instance showMode :: Show Mode where 
    show = genericShow
