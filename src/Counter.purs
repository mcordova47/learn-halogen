module Counter
  ( component
  )
  where

import Prelude

import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = Increment | Decrement

type State = Int

component :: ∀ query input output m. Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    initialState :: input -> State
    initialState _ = 0

    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.div_
        [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
        , HH.div_ [ HH.text $ show state ]
        , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
        ]

    handleAction :: Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
      Increment -> H.modify_ (_ + 1)
      Decrement -> H.modify_ (_ - 1)
