module Utakata.Eval (eval) where

import Audio.WebAudio.GainNode (setGain)
import Audio.WebAudio.Extra (loadAudio, play, stop, addEndEventListener, removeEndEventListener, currentTime)
import Control.Applicative (pure, void, when)
import Control.Bind (bind, discard)
import Control.Monad.State (modify)
import Control.Monad.State.Class (get)
import Data.Array (catMaybes, deleteAt, elemIndex, findIndex, head, index, length, sort, (!!), (:))
import Data.CommutativeRing ((+))
import Data.EuclideanRing ((-))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.NaturalTransformation (type (~>))
import Data.Show (show)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Random (randomInt)
import Electron (close, minimize, showOpenDialog, openDevTools)
import Halogen.Component (ComponentDSL)
import Halogen.Query (gets)
import Halogen.Query.EventSource (SubscribeStatus(..), eventSource)
import Halogen.Query.HalogenM (subscribe)
import Node.FS.Aff (readdir)
import Node.FS.Stats (isFile)
import Node.FS.Sync (stat)
import Node.Path (basename, dirname, extname, resolve)
import Prelude (mod, ($), (*), (/=), (<$>), (<>), (==), const, unit)
import Utakata.LocalStorage (saveStorage)
import Utakata.Type (AudioState(..), Mode(..), Output, Query(..), State, Storage(Storage))

eval :: Query ~> ComponentDSL State Query Output Aff
eval query = do 

    -- log query
    case query of
        Update next -> pure unit --ignore
        _ -> liftEffect $ logShow (const unit <$> query)

    case query of

        ShowOpenDialog next -> do
            result <- showOpenDialog
            case result of 
                Nothing -> pure unit 
                Just dirOrFile -> do 
                    s <- liftEffect $ stat dirOrFile
                    file <- if isFile s
                        then pure $ Just dirOrFile  
                        else do 
                            files <- liftAff (readdir dirOrFile)
                            case head files of 
                                Nothing -> pure Nothing 
                                Just file -> Just <$> liftEffect (resolve [dirOrFile] file) 
                    case file of 
                        Nothing -> pure unit 
                        Just f -> eval (Open f unit)
            pure next 

        Open filePath next -> do
            state <- get



            -- stop the audio and clear buffer
            case state.audio of 
                PlayingAudio { source } -> liftEffect do 
                    removeEndEventListener source.source
                    stop source
                _ -> pure unit 
            _ <- modify _ {
                filePath = Just filePath,
                audio = NotLoaded,
                position = 0.0
            }

            -- update file list 
            let dir = dirname filePath
            entries <- liftAff $ readdir dir

            files <- catMaybes <$> for entries \entry -> liftEffect do 
                resolved <- resolve [dir] entry
                s <- stat resolved 
                pure case extname entry of 
                    ".mp3" -> Just entry 
                    ".wave" -> Just entry 
                    ".ogg" -> Just entry  
                    _ -> Nothing

            -- update shuffled play list


            when (sort files /= sort state.randoms) do 
                randoms <- liftEffect $ shuffle files
                _ <- modify _ { 
                    siblings = files, 
                    randoms = randoms
                }
                pure unit



            -- load the audio file
            audio <- liftAff $ loadAudio filePath state.context
            currentAudioPath <- gets _.filePath 
            state' <- get         
            case state'.audio of 
                NotLoaded | currentAudioPath == Just filePath -> do
                    _ <- modify _ { 
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
                pure do     
                    resolved <- liftEffect $ resolve [dirname filePath] file
                    eval (Open resolved unit)

            saveOptions
            pure next

        Play next -> do
            _ <- modify \s -> s { playing = true }
            state <- get 
            case state.audio of
                Loaded { buffer } | state.playing -> do  
                    updateGain            
                    liftEffect $ log $ "play-file: " <> show state.filePath
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
                        pure do 
                            resolved <- liftEffect $ resolve [dirname currentFilePath] nextFile
                            eval (Open resolved unit)

                _, _ -> pure unit 
            pure next 

        Pause next -> do 
            stopAutio
            pure next

        Stop next -> do 
            eval (Pause unit)
            _ <- modify _ { 
                position = 0.0
            }
            pure next

        Update next -> do
            state <- get 
            case state.audio of
                PlayingAudio { playStart, startPosition } -> do  
                    currentTime <- liftEffect $ currentTime state.context 
                    _ <- modify _ {
                        position = startPosition + currentTime - playStart
                    }
                    pure unit
                _ -> pure unit
            pure next

        Minimize next -> do 
            liftEffect minimize 
            pure next

        Close next -> do 
            stopAutio
            saveOptions
            liftEffect close 
            pure next 

        OpenDevTools next -> do 
            liftEffect openDevTools
            pure next 

        SetMode mode next -> do 
            _ <- modify _ { mode = mode }
            saveOptions
            pure next 

        SetMute mute next -> do 
            _ <- modify _ { muted = mute }
            saveOptions
            updateGain
            pure next 
        
        SetVolume value next -> do 
            _ <- modify _ { volume = value }
            saveOptions
            updateGain 
            pure next 

        SetPosition position next -> do 
            state <- get
            case state.audio of 
                PlayingAudio { source, buffer } -> do  
                    _ <- modify _ { audio = Loaded { buffer }, position = position }
                    liftEffect do 
                        removeEndEventListener source.source
                        stop source
                    playAudio buffer
                _ -> pure unit
            pure next

    where 
        saveOptions = do 
            state <- get 
            liftEffect $ saveStorage $ Storage {
                filePath: state.filePath,
                mode: show state.mode,
                volume: state.volume
            }

        updateGain = do 
            state <- get 
            case state.audio of
                PlayingAudio { source } -> liftEffect $ setGain (if state.muted then 0.0 else state.volume * state.volume) source.gain
                _ -> pure unit 

        playAudio buffer = void do
            state <- get 
            graph <- liftEffect $ play buffer state.position state.context 
            liftEffect $ setGain (state.volume * state.volume) graph.gain
            startTime <- liftEffect $ currentTime state.context
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

        stopAutio = do
            _ <- modify \s -> s { playing = false }
            state <- get 
            case state.audio of 
                PlayingAudio { buffer, source } -> do 
                    _ <- modify _ { 
                        audio = Loaded { buffer }
                    }    
                    liftEffect $ log $ "pause-position: " <> show state.position
                    liftEffect $ stop source
                _ -> pure unit

        shuffle :: forall a. Array a -> Effect (Array a)
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
