module Loupe
  ( Container
  , Component
  , Element
  , Reducer
  , Render
  , Dispatch
  , container
  , component
  , element
  , nest
  , focus
  , focusS
  , match
  , split
  , foreach
  ) where
import Prelude

import Data.Either (either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens (Getter', Prism', Review', matching, review, (^.))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import React (ReactClass, ReactElement)
import React as React
import Unsafe.Coerce (unsafeCoerce)

-- | Self-sufficient component
type Container = ReactClass {}

-- | Composable component
type Component st act = ReactClass { state :: st, dispatch :: Dispatch act }

-- | Generic type used for all elements
type Element st act = { state :: st, dispatch :: Dispatch act } -> ReactElement

-- | Reducer function used in containers
type Reducer st act = st -> act -> Effect st

-- | Render function with a state and a dispatcher
type Render st act = st -> (act -> Effect Unit) -> Element st act

type Dispatch act = act -> Effect Unit

-- | Creates a self-sufficient container class from an initial state,
-- | a reducer and a render function
container :: ∀ st act. st -> Reducer st act -> Render st act -> Container
container initialState reducer render =
  unsafeFunctionComponent $ mkEffectFn1 run
  where
    run {} = ado
      {state, dispatch} <- useReducer reducer initialState
      in render state dispatch {state, dispatch}

component' :: ∀ st act. Element st act -> Component st act
component' = unsafeCoerce

-- | Creates a composable component from a render function
component :: ∀ st act. Render st act -> Component st act
component render = component' run
  where
    run {state, dispatch} = render state dispatch {state, dispatch}

-- | Creates an element from a component
element :: ∀ st act. Component st act -> Element st act
element = React.createLeafElement

-- | Creates an element from a container
nest :: ∀ st act. Container -> Element st act
nest cl _ = React.createLeafElement cl {}

focus
  :: ∀ st1 st2 act1 act2
   . Getter' st2 st1
  -> Review' act2 act1
  -> Element st1 act1
  -> Element st2 act2
focus lens prism render {state, dispatch} =
  render { state: state ^. lens, dispatch: review prism >>> dispatch }

focusS :: ∀ st1 st2 act. Getter' st2 st1 -> Element st1 act -> Element st2 act
focusS lens = focus lens identity

match :: ∀ st act1 act2. Review' act2 act1 -> Element st act1 -> Element st act2
match prism = focus identity prism

split :: ∀ st1 st2 act. Prism' st1 st2 -> Element st2 act -> Element st1 act
split prism render {state, dispatch} =
  either mempty render' $ matching prism state
  where
    render' state' = render { state: state', dispatch }

foreach :: ∀ st act. (Int -> Element st act) -> Element (Array st) (Tuple Int act)
foreach render {state, dispatch} = foldMapWithIndex el state
  where
    el i item = render i { state: item, dispatch: dispatch <<< Tuple i }

unsafeFunctionComponent :: forall props. EffectFn1 props ReactElement -> ReactClass props
unsafeFunctionComponent = unsafeCoerce

useReducer
  :: ∀ st act
   . (st -> act -> Effect st)
  -> st
  -> Effect { state :: st, dispatch :: act -> Effect Unit }
useReducer reducer initialState = ado
  result <- runEffectFn2 useReducer_ (mkEffectFn2 reducer) initialState
  in { state: result.state, dispatch: runEffectFn1 result.dispatch }

foreign import useReducer_
  :: ∀ st act
   . EffectFn2
     (EffectFn2 st act st)
     st
     { state :: st, dispatch :: EffectFn1 act Unit }
