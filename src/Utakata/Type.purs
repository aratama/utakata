module Utakata.Type where

import Utakata.Audio (AudioBuffer, AudioContext, AudioBufferSource)
import Utakata.LocalStorage (STORAGE)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Void (Void)
import Halogen.Aff.Effects (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import Node.FS.Aff (FS)
import Node.Path (FilePath)

type State = {
    context :: AudioContext,
    title :: String,
    filePath :: Maybe FilePath, 
    buffer :: Maybe AudioBuffer,
    source :: Maybe AudioBufferSource,
    position :: Number
}

newtype Storage = Storage {
    filePath :: NullOrUndefined FilePath
}

data Query a = OpenFileDialog a
             | Open FilePath a
             | Play a
             | Pause a 
             | Stop a
             | Update a
             | Close a

type Input = AudioContext

type Output = Void

type Effects eff = HalogenEffects (
    ajax :: AJAX, 
    fs :: FS, 
    storage :: STORAGE "Utakata.Storage" Storage
        | eff)

derive instance genericStorage :: Generic Storage _

instance decodeStorage :: Decode Storage where 
    decode = genericDecode defaultOptions { unwrapSingleConstructors = true }

instance encodeStorage :: Encode Storage where
    encode = genericEncode defaultOptions { unwrapSingleConstructors = true }


