module UI (ui) where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.State (modify)
import Control.Monad.State.Class (get)
import Control.Plus ((<$))
import Cuica.Audio (loadAudio, play, readMetadata)
import Cuica.File (home, openDirectory)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.NaturalTransformation (type (~>))
import Data.Show (show)
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
import Type (Input, Output, Query(..), State, Effects)

eval :: forall eff. Query ~> ComponentDSL State Query Output (Aff (Effects eff))
eval = case _ of
    OpenDirectory next -> do
        state <- get
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
                    Just f -> do 
                        metadata <- liftAff $ readMetadata f
                        audio <- liftAff (loadAudio f state.context) 
                        liftEff $ play audio state.context
                        modify _ { directory = metadata.title }
        pure next 

    Play next -> do 
        modify _ { playing = true }
        pure next 

    Pause next -> do 
        modify _ { playing = false }    
        pure next


ui :: forall eff. Component HTML Query Input Output (Aff (Effects eff))
ui = component {
    render,
    eval,
    initialState: \context -> { directory: home, playing: false, context },
    receiver: \_ -> Nothing
}
