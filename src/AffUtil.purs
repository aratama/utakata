module AffUtil where 

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Prelude (Unit, bind, pure)

makeAffWithNonCanceler :: forall a eff. ((Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Eff eff Unit) -> Aff eff a
makeAffWithNonCanceler raw = makeAff \callback -> do 
    _ <- raw (\err -> callback (Left err) ) (\value -> callback (Right value) )
    pure nonCanceler










