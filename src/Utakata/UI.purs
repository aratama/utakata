module Utakata.UI (ui) where

import Control.Applicative (pure, when)
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
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array (head, index, length, modifyAt, updateAt, (..))
import Data.CommutativeRing ((+))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
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
import Node.Path (basename, basenameWithoutExt, dirname, extname)
import Prelude (($), (/=), (<$>), (>>=))
import Utakata.Audio (loadAudio, play, stop, readMetadata)
import Utakata.Electron (close, minimize)
import Utakata.File (home, openDirectory)
import Utakata.LocalStorage (STORAGE, saveStorage)
import Utakata.Render (render)
import Utakata.Type (Input, Output, Query(..), State, Effects, Storage(..))

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

        --modify _ { title = "(Loading...)" }
        state <- get
        let playing = isJust state.source

        eval (Stop unit)

        let dir = dirname filePath
        files <- liftAff $ readdir dir

        -- load the audio file
        metadataEither <- liftAff $ attempt $ readMetadata filePath
        audio <- liftAff $ loadAudio filePath state.context
        modify _ { 
            filePath = Just filePath,
            files = { path: _, title: Nothing } <$> files,
            buffer = Just audio 
        }

        -- play current audio if playing 
        when playing do 
            eval (Play unit)

        -- get metadata of others and update audio file titles
        for_ (0 .. length files) \i -> do 
            s <- get
            case index s.files i of 
                Nothing -> pure unit 
                Just fp -> do 
                    met <- liftAff $ attempt $ readMetadata (dir <> "/" <> fp.path)
                    let title' = case met of 
                            Right m | m.title /= "" -> Just m.title
                            _ -> Nothing 
                    modify _ { 
                        files = fromMaybe s.files $ modifyAt i (_ { title = title' }) s.files 
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

    Minimize next -> do 
        liftEff minimize 
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
        context,
        filePath: Nothing,
        files: [],
        buffer: Nothing,
        source: Nothing,
        position: 0.0
    },
    receiver: \_ -> Nothing
}
