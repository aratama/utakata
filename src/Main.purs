module Main where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Utakata.Type (Effects)
import Utakata.Main (main) as Utakata

main :: forall eff. Eff (Effects eff) Unit
main = Utakata.main 