module Components.Composition
  ( parent
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots = ( button :: forall query. H.Slot query Void Int )

_button = Proxy :: Proxy "button"

parent :: forall query input output m. H.Component query input output m
parent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
    render :: forall state action. state -> H.ComponentHTML action Slots m
    render _ =
      HH.div_ [ HH.slot_ _button 0 button { label: "Click Me" } ]

type Input = { label :: String }

type State = { label :: String }

button :: forall query output m. H.Component query Input output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
    initialState :: Input -> State
    initialState input = input

    render :: forall action. State -> H.ComponentHTML action () m
    render { label } = HH.button [ ] [ HH.text label ]
