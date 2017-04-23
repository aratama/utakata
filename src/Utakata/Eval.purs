module Utakata.Eval (eval) where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (modify)
import Control.Monad.State.Class (get)
import DOM.HTML.Event.EventTypes (playing)
import DOM.Node.Element (localName)
import Data.Array (catMaybes, findIndex, head, index, length, modifyAt, (..))
import Data.Boolean (otherwise)
import Data.CommutativeRing ((+))
import Data.Either (Either(Right))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid ((<>))
import Data.NaturalTransformation (type (~>))
import Data.Traversable (for, for_)
import Data.Unit (unit)
import Halogen.Component (ComponentDSL)
import Halogen.Query (gets)
import Node.FS.Aff (readdir)
import Node.FS.Stats (isFile)
import Node.FS.Sync (stat)
import Node.Path (basename, dirname, extname)
import Prelude (negate, ($), (/=), (<$>), (<<<), (<=), (==))
import Utakata.Audio (loadAudio, play, stop)
import Utakata.Electron (close, minimize, openDirectory, openDevTools)
import Utakata.LocalStorage (saveStorage')
import Utakata.Type (Effects, Output, Query(..), State, Storage(Storage))

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

        state <- get
        for_ state.source (liftEff <<< stop) 

        modify _ { 
            filePath = Just filePath,
            buffer = Nothing,
            source = Nothing
        }

        let dir = dirname filePath
        entries <- liftAff $ readdir dir

        files <- catMaybes <$> for entries \entry -> do 
            s <- liftEff $ stat $ dir <> "/" <> entry
            pure case extname entry of 
                ".mp3" -> Just entry 
                ".wave" -> Just entry 
                ".ogg" -> Just entry  
                _ -> Nothing  

        -- load the audio file
        audio <- liftAff $ loadAudio filePath state.context
        currentAudioPath <- gets _.filePath
        when (currentAudioPath == Just filePath) do

            -- play current audio if playing 
            playing <- gets _.playing
            source <- if playing 
                then liftEff $ Just <$> play audio state.context
                else pure Nothing 

            modify _ { 
                siblings = files,
                buffer = Just audio, 
                source = source
            }

        pure next 


    Move delta next -> do 
        state <- get  
        fromMaybe (pure unit) do 
            filePath <- state.filePath 
            i <- findIndex (\x -> x == basename filePath) state.siblings
            file <- index state.siblings (i + delta)
            pure $ eval (Open (dirname filePath <> "/" <> file) unit)

        pure next

    Play next -> do
        state <- get 
        case state.buffer, state.source of 
            Just buffer, Nothing -> do  
                source <- liftEff $ play buffer state.context
                modify _ { 
                    playing = true,
                    source = Just source 
                }
            _, _ -> pure unit 

        pure next 

    Pause next -> do 
        state <- get 
        case state.source of 
            Just source -> do 
                liftEff $ stop source
                modify _ { 
                    playing = false,
                    source = Nothing
                }    
            Nothing -> pure unit
        pure next

    Stop next -> do 
        state <- get 
        case state.source of 
            Just source -> do 
                liftEff $ stop source
                modify _ { 
                    playing = false,
                    source = Nothing
                }    
            Nothing -> pure unit
        pure next

    Update next -> do
        state <- get 
        case state.source of 
            Just _ -> do  
                modify _ {
                    position = state.position + 0.0002
                }
            Nothing -> pure unit
        pure next

    Minimize next -> do 
        liftEff minimize 
        pure next

    Close next -> do 
        state <- get 
        liftEff $ saveStorage' $ Storage {
            filePath: NullOrUndefined state.filePath
        }
        liftEff close 
        pure next 


    OpenDevTools next -> do 
        liftEff openDevTools
        pure next 