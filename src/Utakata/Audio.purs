module Utakata.Audio (AudioBuffer, AudioBufferSource, AudioContext, Metadata, createAudioContext, loadAudio, play, stop, readMetadata) where 

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import Data.Unit (Unit)
import Node.Buffer (Buffer)
import Node.Path (FilePath)

foreign import data AudioBuffer :: Type

foreign import data AudioBufferSource :: Type

foreign import data AudioContext :: Type

foreign import createAudioContext :: forall eff. Eff (dom :: DOM | eff) AudioContext

foreign import play :: forall eff. AudioBuffer -> AudioContext -> Eff (dom :: DOM | eff) AudioBufferSource

foreign import stop :: forall eff. AudioBufferSource -> Eff (dom :: DOM | eff) Unit
 
foreign import loadAudioEff :: forall eff. String -> AudioContext -> (Error -> Eff (dom :: DOM | eff) Unit) -> (AudioBuffer -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

loadAudio :: forall eff. FilePath -> AudioContext -> Aff (dom :: DOM | eff) AudioBuffer
loadAudio path context = makeAff (loadAudioEff path context)

type Metadata = {
    artist :: Array String,
    album :: String,
    albumartist :: Array String, 
    title :: String,
    year :: String,
    track :: { no :: Int, of :: Int },
    disk :: { no :: Int, of :: Int },
    genre :: Array String,
    picture :: Array { format :: String, data :: Buffer },
    duration :: Number
}

foreign import readMetadataEff :: forall eff. String -> (Error -> Eff (dom :: DOM | eff) Unit) -> (Metadata -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

readMetadata :: forall eff. FilePath -> Aff (dom :: DOM | eff) Metadata
readMetadata path = makeAff (readMetadataEff path)

foreign import currentTime :: forall eff. Eff (dom :: DOM | eff) Number

