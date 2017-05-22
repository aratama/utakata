module Utakata.Eval (eval) where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Fork.Class (fork)
import Control.Monad.State (modify)
import Control.Monad.State.Class (get)
import Data.Array (catMaybes, findIndex, head, index, nub, (:))
import Data.BooleanAlgebra ((&&))
import Data.CommutativeRing ((+))
import Data.EuclideanRing ((-))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid ((<>))
import Data.NaturalTransformation (type (~>))
import Data.Show (show)
import Data.Traversable (for, for_)
import Data.Unit (unit)
import Halogen.Component (ComponentDSL)
import Halogen.Query (gets)
import Halogen.Query.EventSource (SubscribeStatus(..), eventSource)
import Halogen.Query.HalogenM (subscribe)
import Node.FS.Aff (readdir)
import Node.FS.Stats (isFile)
import Node.FS.Sync (stat)
import Node.Path (basename, dirname, extname)
import Prelude (($), (*), (<$>), (<<<), (<>), (==))
import Utakata.Audio (loadAudio, play, stop, setGain, addEndEventListener, removeEndEventListener, currentTime)
import Utakata.Electron (close, minimize, openDirectory, openDevTools)
import Utakata.LocalStorage (saveStorage')
import Utakata.Type (Audio(..), Effects, Mode(..), Output, Query(..), State, Storage(Storage))


eval :: forall eff. Query ~> ComponentDSL State Query Output (Aff (Effects eff))
eval = case _ of

    OpenFileDialog next -> do
        result <- openDirectory
        case result of 
            Nothing -> pure unit 
            Just dirOrFile -> do 
                s <- liftEff $ stat dirOrFile
                file <- if isFile s
                    then pure $ Just dirOrFile  
                    else do 
                        files <- liftAff (readdir dirOrFile)
                        pure $ (\p -> dirOrFile <> "/" <> p) <$> head files 
                case file of 
                    Nothing -> pure unit 
                    Just f -> eval (Open f unit)
        pure next 

    Open filePath next -> do
        liftEff $ log $ "open " <> filePath
        state <- get

        -- stop the audio and clear buffer
        case state.audio of 
            PlayingAudio { source } -> liftEff do 
                removeEndEventListener source.source
                stop source
            _ -> pure unit 
        modify _ { 
            filePath = Just filePath,
            audio = NotLoaded,
            history = nub (filePath : state.history), 
            position = 0.0
        }

        -- update file list 
        let dir = dirname filePath
        entries <- liftAff $ readdir dir
        files <- catMaybes <$> for entries \entry -> do 
            s <- liftEff $ stat $ dir <> "/" <> entry
            pure case extname entry of 
                ".mp3" -> Just entry 
                ".wave" -> Just entry 
                ".ogg" -> Just entry  
                _ -> Nothing
        modify _ { siblings = files }

        -- load the audio file
        audio <- liftAff $ loadAudio filePath state.context
        currentAudioPath <- gets _.filePath 
        state' <- get         
        case state'.audio of 
            NotLoaded | currentAudioPath == Just filePath -> do
                modify _ { 
                    audio = Loaded { buffer: audio } 
                }

                -- play current audio if playing 
                case state.audio of 
                    PlayingAudio _ -> do 
                        updateGain
                        eval (Play unit)
                    _ -> pure unit

            _ -> pure unit 

        pure next 


    Move delta next -> do 
        liftEff $ log $ "move " <> show delta
        state <- get  
        fromMaybe (pure unit) do 
            filePath <- state.filePath 
            i <- findIndex (\x -> x == basename filePath) state.siblings
            file <- index state.siblings (i + delta)
            pure $ eval (Open (dirname filePath <> "/" <> file) unit)

        saveOptions
        pure next

    Play next -> do
        liftEff $ log "play"
        state <- get 
        case state.audio of
            Loaded { buffer } -> do  
                liftEff $ log $ "play-file: " <> show state.filePath
                graph <- liftEff $ play buffer state.position state.context 
                liftEff $ setGain (state.volume * state.volume) graph
                startTime <- liftEff $ currentTime state.context
                subscribe $ eventSource (addEndEventListener graph.source) (\e -> Just (End Done))
                modify _ { 
                    audio = PlayingAudio {
                        buffer: buffer,
                        source: graph,
                        playStart: startTime,
                        startPosition: state.position,
                        currentTime: startTime
                    }
                }
                pure unit 

            _ -> pure unit 

        pure next 

    End next -> do
        liftEff $ log "end"
        state <- get 
        case state.audio, state.mode of 
            PlayingAudio _, RepeatOff -> eval (Stop unit) 
            PlayingAudio _, RepeatOne -> eval (Move 0 unit)
            PlayingAudio _, RepeatAll -> eval (Move 1 unit)
            PlayingAudio _, Random -> eval (Move 1 unit)
            _, _ -> pure unit 
        pure next 

    Pause next -> do 
        liftEff $ log "pause"
        state <- get 
        case state.audio of 
            PlayingAudio { buffer, source } -> do 
                modify _ { 
                    audio = Loaded { buffer }
                }    
                liftEff $ log $ "pause-position: " <> show state.position
                liftEff $ stop source
            _ -> pure unit
        pure next

    Stop next -> do 
        liftEff $ log "stop"
        eval (Pause unit)
        modify _ { 
            position = 0.0
        }
        pure next

    Update next -> do
        state <- get 
        case state.audio of
            PlayingAudio { playStart, startPosition } -> do  
                currentTime <- liftEff $ currentTime state.context 
                modify _ {
                    position = startPosition + currentTime - playStart
                }
            _ -> pure unit
        pure next

    Minimize next -> do 
        liftEff minimize 
        pure next

    Close next -> do 
        saveOptions
        liftEff close 
        pure next 


    OpenDevTools next -> do 
        liftEff openDevTools
        pure next 

    SetMode mode next -> do 
        modify _ { mode = mode }
        saveOptions
        pure next 

    SetMute mute next -> do 
        modify _ { muted = mute }
        saveOptions
        updateGain
        pure next 
    
    SetVolume value next -> do 
        modify _ { volume = value }
        saveOptions
        updateGain 
        pure next 

  where 
    saveOptions = do 
        state <- get 
        liftEff $ saveStorage' $ Storage {
            filePath: NullOrUndefined state.filePath,
            mode: show state.mode,
            volume: state.volume
        }

    updateGain = do 
        state <- get 
        case state.audio of
            PlayingAudio { source } -> liftEff $ setGain (if state.muted then 0.0 else state.volume * state.volume) source
            _ -> pure unit 

