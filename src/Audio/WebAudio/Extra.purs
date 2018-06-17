module Audio.WebAudio.Extra (
    AudioGraph, AudioTime,
    loadAudio, play, stop, getDuration, addEndEventListener, removeEndEventListener, currentTime, 
    module Audio.WebAudio.Types
) where 

import AffUtil (makeAffWithNonCanceler)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Exception (Error)
import Web.Event.Event (Event)
import Data.Unit (Unit)
import Node.Path (FilePath)
import Audio.WebAudio.Types (AudioBuffer, AudioBufferSourceNode, AudioContext, GainNode)

type AudioTime = Number

type AudioGraph = { source :: AudioBufferSourceNode, gain :: GainNode }

foreign import play :: AudioBuffer -> AudioTime -> AudioContext -> Effect AudioGraph

foreign import addEndEventListener :: AudioBufferSourceNode -> (Event -> Effect Unit) -> Effect Unit

foreign import removeEndEventListener :: AudioBufferSourceNode -> Effect Unit

-- stop audio with linearRampToValueAtTime
foreign import stop :: AudioGraph -> Effect Unit
 
foreign import loadAudioEff :: String -> AudioContext -> (Error -> Effect Unit) -> (AudioBuffer -> Effect Unit) -> Effect Unit

loadAudio :: FilePath -> AudioContext -> Aff AudioBuffer
loadAudio path context = makeAffWithNonCanceler (loadAudioEff path context)

foreign import currentTime :: AudioContext -> Effect Number

foreign import getDuration :: AudioBuffer -> Number


