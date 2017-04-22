module Type where

import Cuica.Audio (AudioContext)
import Data.Void (Void)
import Halogen.Aff.Effects (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import Node.FS.Aff (FS)
import Node.Path (FilePath)


type State = {
    context :: AudioContext,
    directory :: FilePath,
    playing :: Boolean
}

data Query a = OpenDirectory a
             | Play a
             | Pause a 

type Input = AudioContext

type Output = Void

type Effects eff = HalogenEffects (ajax :: AJAX, fs :: FS | eff)

