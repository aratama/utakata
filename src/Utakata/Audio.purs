module Utakata.Audio (
    AudioBuffer, AudioBufferSource, AudioContext, GainNode, AudioGraph,
    createAudioContext, loadAudio, play, stop, setGain, getDuration, addEndEventListener
) where 

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import DOM.Event.Event (Event)
import Data.Unit (Unit)
import Node.Path (FilePath)

foreign import data AudioBuffer :: Type

foreign import data AudioBufferSource :: Type

foreign import data AudioContext :: Type

foreign import data GainNode :: Type

type AudioGraph = { source :: AudioBufferSource, gain :: GainNode }

foreign import createAudioContext :: forall eff. Eff (dom :: DOM | eff) AudioContext

foreign import play :: forall eff. AudioBuffer -> AudioContext -> Eff (dom :: DOM | eff) AudioGraph

foreign import addEndEventListener :: forall eff. AudioBufferSource -> (Event -> Eff (dom :: DOM, avar :: AVAR | eff) Unit) -> Eff (dom :: DOM, avar :: AVAR | eff) Unit

foreign import stop :: forall eff. AudioGraph -> Eff (dom :: DOM | eff) Unit

foreign import setGain :: forall eff. Number -> AudioGraph -> Eff (dom :: DOM | eff) Unit
 
foreign import loadAudioEff :: forall eff. String -> AudioContext -> (Error -> Eff (dom :: DOM | eff) Unit) -> (AudioBuffer -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

loadAudio :: forall eff. FilePath -> AudioContext -> Aff (dom :: DOM | eff) AudioBuffer
loadAudio path context = makeAff (loadAudioEff path context)

foreign import currentTime :: forall eff. Eff (dom :: DOM | eff) Number

foreign import getDuration :: AudioBuffer -> Number

