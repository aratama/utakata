module AffUtil where 

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Prelude (Unit, discard, pure, (<<<))

makeAffWithNonCanceler :: forall a eff. ((Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Eff eff Unit) -> Aff eff a
makeAffWithNonCanceler raw = makeAff \callback -> do 
    raw (callback <<< Left) (callback <<< Right)
    pure nonCanceler
