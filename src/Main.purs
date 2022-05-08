module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RNG as RNG

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI RNG.component unit body
