module Type where

import Cuica.Audio (AudioBuffer, AudioContext, AudioBufferSource)
import Data.Maybe (Maybe)
import Data.Void (Void)
import Halogen.Aff.Effects (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import Node.FS.Aff (FS)
import Node.Path (FilePath)


type State = {
    context :: AudioContext,
    directory :: FilePath,
    buffer :: Maybe AudioBuffer,
    source :: Maybe AudioBufferSource,
    position :: Number
}

data Query a = OpenDirectory a
             | Play a
             | Pause a 
             | Stop a
             | Update a

type Input = AudioContext

type Output = Void

type Effects eff = HalogenEffects (ajax :: AJAX, fs :: FS | eff)

