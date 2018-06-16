module Audio (
    AudioBuffer, AudioBufferSource, AudioContext, GainNode, AudioGraph, AudioTime,
    createAudioContext, loadAudio, play, stop, setGain, getDuration, addEndEventListener, removeEndEventListener, currentTime
) where 

import AffUtil (makeAffWithNonCanceler)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Exception (Error)
import Web.Event.Event (Event)
import Data.Unit (Unit)
import Node.Path (FilePath)

type AudioTime = Number

foreign import data AudioBuffer :: Type

foreign import data AudioBufferSource :: Type

foreign import data AudioContext :: Type

foreign import data GainNode :: Type

type AudioGraph = { source :: AudioBufferSource, gain :: GainNode }

foreign import createAudioContext :: Effect AudioContext

foreign import play :: AudioBuffer -> AudioTime -> AudioContext -> Effect AudioGraph

foreign import addEndEventListener :: AudioBufferSource -> (Event -> Effect Unit) -> Effect Unit

foreign import removeEndEventListener :: AudioBufferSource -> Effect Unit

foreign import stop :: AudioGraph -> Effect Unit

foreign import setGain :: Number -> AudioGraph -> Effect Unit
 
foreign import loadAudioEff :: String -> AudioContext -> (Error -> Effect Unit) -> (AudioBuffer -> Effect Unit) -> Effect Unit

loadAudio :: FilePath -> AudioContext -> Aff AudioBuffer
loadAudio path context = makeAffWithNonCanceler (loadAudioEff path context)

foreign import currentTime :: AudioContext -> Effect Number

foreign import getDuration :: AudioBuffer -> Number




