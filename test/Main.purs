module Test.Main where

import Data.Unit (Unit)
import Effect (Effect)
import Prelude (pure, bind, discard, unit, void)


main :: Effect Unit
main = void do
    pure unit