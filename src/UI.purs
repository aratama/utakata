module UI (ui) where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.State (modify)
import Control.Monad.State.Class (get)
import Control.Plus ((<$))
import Cuica.Audio (loadAudio, play, stop, readMetadata)
import Cuica.Electron (close)
import Cuica.File (home, openDirectory)
import Cuica.LocalStorage (STORAGE, saveStorage)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array (head)
import Data.CommutativeRing ((+))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.NaturalTransformation (type (~>))
import Data.Show (show)
import Data.Traversable (for_)
import Data.Unit (unit)
import Halogen (Component)
import Halogen.Component (ComponentDSL, component)
import Halogen.HTML.Core (HTML)
import Network.HTTP.Affjax (get) as Ajax
import Node.FS.Aff (readdir)
import Node.FS.Stats (isFile)
import Node.FS.Sync (stat)
import Prelude (($), (<$>), (>>=))
import Render (render)
import Type (Input, Output, Query(..), State, Effects, Storage(..))

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

    Open f next -> do
        modify _ { title = "(Loading...)" }
        state <- get
        metadata <- liftAff $ readMetadata f
        audio <- liftAff (loadAudio f state.context) 
        modify _ { 
            filePath = Just f,
            title = metadata.title,
            buffer = Just audio 
        }
        pure next 

    Play next -> do
        state <- get 
        case state.buffer, state.source of 
            Just buffer, Nothing -> do  
                source <- liftEff $ play buffer state.context
                modify _ { source = Just source }
            _, _ -> pure unit 

        pure next 

    Pause next -> do 
        state <- get 
        case state.source of 
            Just source -> do 
                liftEff $ stop source
                modify _ { 
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

    Close next -> do 
        state <- get 
        liftEff $ saveStorage $ Storage {
            filePath: NullOrUndefined state.filePath
        }
        liftEff close 
        pure next 


ui :: forall eff. Component HTML Query Input Output (Aff (Effects eff))
ui = component {
    render,
    eval,
    initialState: \context -> { 
        title: "",
        context,
        filePath: Nothing,
        buffer: Nothing,
        source: Nothing,
        position: 0.0
    },
    receiver: \_ -> Nothing
}
