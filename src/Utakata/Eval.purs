module Utakata.Eval (eval) where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.State (modify)
import Control.Monad.State.Class (get)
import Data.Array (catMaybes, deleteAt, elemIndex, findIndex, head, index, length, sort, (!!), (:))
import Data.CommutativeRing ((+))
import Data.EuclideanRing ((-))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Data.NaturalTransformation (type (~>))
import Data.Show (show)
import Data.Traversable (for)

import Halogen.Component (ComponentDSL)
import Halogen.Query (gets)
import Halogen.Query.EventSource (SubscribeStatus(..), eventSource)
import Halogen.Query.HalogenM (subscribe)
import Node.FS.Aff (readdir)
import Node.FS.Stats (isFile)
import Node.FS.Sync (stat)
import Node.Path (basename, dirname, extname, resolve)
import Prelude (mod, ($), (*), (/=), (<$>), (<>), (==), const, unit)
import Utakata.Audio (loadAudio, play, stop, setGain, addEndEventListener, removeEndEventListener, currentTime)
import Utakata.Electron (close, minimize, showOpenDialog, openDevTools)
import Utakata.LocalStorage (saveStorage')
import Utakata.Type (AudioState(..), Effects, Mode(..), Output, Query(..), State, Storage(Storage))


eval :: forall eff. Query ~> ComponentDSL State Query Output (Aff (Effects eff))
eval query = do 

    -- log query
    case query of
        Update next -> pure unit --ignore
        _ -> liftEff $ logShow (const unit <$> query)

    case query of

        ShowOpenDialog next -> do
            result <- showOpenDialog
            case result of 
                Nothing -> pure unit 
                Just dirOrFile -> do 
                    s <- liftEff $ stat dirOrFile
                    file <- if isFile s
                        then pure $ Just dirOrFile  
                        else do 
                            files <- liftAff (readdir dirOrFile)
                            pure $ (resolve [dirOrFile]) <$> head files 
                    case file of 
                        Nothing -> pure unit 
                        Just f -> eval (Open f unit)
            pure next 

        Open filePath next -> do
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
                position = 0.0
            }

            -- update file list 
            let dir = dirname filePath
            entries <- liftAff $ readdir dir

            files <- catMaybes <$> for entries \entry -> do 
                s <- liftEff $ stat $ resolve [dir] entry
                pure case extname entry of 
                    ".mp3" -> Just entry 
                    ".wave" -> Just entry 
                    ".ogg" -> Just entry  
                    _ -> Nothing

            -- update shuffled play list
            when (sort files /= sort state.randoms) do 
                randoms <- liftEff $ shuffle files
                modify _ { 
                    siblings = files, 
                    randoms = randoms
                }

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
                    when state.playing do eval (Play unit)
                    
                _ -> pure unit 

            pure next 


        Move delta next -> do 
            state <- get  
            fromMaybe (pure unit) do 
                let list = case state.mode of 
                        Random -> state.randoms 
                        _ -> state.siblings
                filePath <- state.filePath 
                i <- findIndex (\x -> x == basename filePath) list
                file <- index list (mod (length list + i + delta) (length list))
                pure $ eval (Open (resolve [dirname filePath] file) unit)

            saveOptions
            pure next

        Play next -> do
            modify \s -> s { playing = true }
            state <- get 
            case state.audio of
                Loaded { buffer } | state.playing -> do  
                    updateGain            
                    liftEff $ log $ "play-file: " <> show state.filePath
                    playAudio buffer
                    pure unit 

                _ -> pure unit 

            pure next 

        End next -> do
            state <- get 
            case state.audio, state.mode of 
                PlayingAudio _, RepeatOff -> eval (Stop unit) 
                PlayingAudio _, RepeatOne -> eval (Move 0 unit)
                PlayingAudio _, RepeatAll -> eval (Move 1 unit)
                PlayingAudio _, Random -> do 
                    fromMaybe (pure unit) do  
                        currentFilePath <- state.filePath
                        index <- elemIndex (basename currentFilePath) state.randoms
                        nextFile <- state.randoms !! (mod index (length state.randoms) + 1)
                        pure $ eval (Open (resolve [dirname currentFilePath] nextFile) unit)

                _, _ -> pure unit 
            pure next 

        Pause next -> do 
            modify \s -> s { playing = false }
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

        SetPosition position next -> do 
            state <- get
            case state.audio of 
                PlayingAudio { source, buffer } -> do  
                    modify _ { audio = Loaded { buffer }, position = position }
                    liftEff do 
                        removeEndEventListener source.source
                        stop source
                    playAudio buffer
                _ -> pure unit
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

        playAudio buffer = do
            state <- get 
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

        shuffle xs = do 
            i <- randomInt 0 (length xs - 1)
            fromMaybe (pure []) do 
                y <- xs !! i 
                ys <- deleteAt i xs
                Just do 
                    ys' <- shuffle ys
                    pure (y : ys')

        filterM f xs = catMaybes <$> for xs \x -> do 
            s <- f x
            if s then Just x else Nothing 
