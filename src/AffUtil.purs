module AffUtil where 

import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect (Effect)
import Effect.Exception (Error)
import Data.Either (Either(..))
import Prelude (Unit, discard, pure, (<<<))

makeAffWithNonCanceler :: forall a. ((Error -> Effect Unit) -> (a -> Effect Unit) -> Effect Unit) -> Aff a
makeAffWithNonCanceler raw = makeAff \callback -> do 
    raw (callback <<< Left) (callback <<< Right)
    pure nonCanceler
