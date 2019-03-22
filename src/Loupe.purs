module Loupe
  ( Container
  , Component
  , Component'
  , Element
  , Reducer
  , Render
  , Dispatch
  , container
  , containerDerivedProps
  , reify
  , component
  , element
  , nest
  , fromClass
  , focus
  , focusS
  , match
  , split
  , foreach
  ) where
import Prelude

import Control.Coroutine (Producer, ($$))
import Control.Coroutine as Co
import Control.Monad.Rec.Class (forever)
import Data.Either (either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (applyFlipped)
import Data.Lens (Getter', Prism', Review', matching, review, (^.))
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import React (Children, ReactClass, ReactElement)
import React as React
import Unsafe.Coerce (unsafeCoerce)

-- | Self-sufficient component
type Container props = ReactClass props

-- | Composable component
type Component props st act =
  ReactClass { props :: props, state :: st, dispatch :: Dispatch act }

-- | Component with no props
type Component' st act = Component {} st act

-- | Generic type used for all elements
type Element st act = { state :: st, dispatch :: Dispatch act } -> ReactElement

-- | Reducer function used in containers
type Reducer st act
   = act
  -> (act -> Aff Unit)
  -> st
  -> Producer (st -> st) Aff Unit

-- | Render function with a state and a dispatcher
type Render props st act = props -> st -> Dispatch act -> Element st act

type Dispatch act = act -> Effect Unit

-- | Creates a self-sufficient container class from an initial state,
-- | a reducer and a render function
container
  :: ∀ props st act
   . st
  -> Reducer st act
  -> Render props st act
  -> Container props
container initialState reducer render =
  containerDerivedProps (const initialState) reducer render

containerDerivedProps
  :: ∀ props st act
   . (props -> st)
  -> Reducer st act
  -> Render props st act
  -> Container props
containerDerivedProps deriveState reducer render =
  unsafeFunctionComponent $ mkEffectFn1 constructor
  where
    constructor props = ado
      { state, setState } <- useState $ deriveState props
      let consumer = consume setState
      let dispatch' act = Co.runProcess $ reducer act dispatch' state $$ consumer
      let dispatch = launchAff_ <<< dispatch'
      in render props state dispatch {state, dispatch}
    consume setState = forever do
      modify <- Co.await
      liftEffect $ setState modify

component' :: ∀ props st act. (props -> Element st act) -> Component props st act
component' render = unsafeCoerce render'
  where
    render' {props, state, dispatch} = render props {state, dispatch}

reify :: ∀ st act. (st -> Dispatch act -> Element st act) -> Element st act
reify render {state, dispatch} = render state dispatch {state, dispatch}

-- | Creates a composable component from a render function
component :: ∀ props st act. Render props st act -> Component props st act
component render = component' $ reify <<< render

-- | Creates an element from a component
element :: ∀ props st act. Component props st act -> props -> Element st act
element cls props {state, dispatch} =
  React.createLeafElement cls {props, state, dispatch}

-- | Creates an element from a container and props
nest :: ∀ props st act. Container {| props } -> {| props } -> Element st act
nest cl = const <<< React.unsafeCreateLeafElement cl

fromClass
  :: ∀ props st act
   . ReactClass { children :: Children | props }
  -> {| props }
  -> Array (Element st act)
  -> Element st act
fromClass cl props children conf =
  React.unsafeCreateElement cl props $ applyFlipped conf <$> children

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

useState :: ∀ st. st -> Effect { state :: st, setState :: (st -> st) -> Effect Unit }
useState initialState = ado
  result <- runEffectFn1 useState_ initialState
  in { state: result.state, setState: runEffectFn1 result.setState }

foreign import useState_
  :: ∀ st
   . EffectFn1
     st
     { state :: st, setState :: EffectFn1 (st -> st) Unit }

useEffect :: Nullable (Array Foreign) -> Effect (Effect Unit) -> Effect Unit
useEffect deps fn = runEffectFn2 useEffect_ fn deps

foreign import useEffect_ :: EffectFn2 (Effect (Effect Unit)) (Nullable (Array Foreign)) Unit

useRef :: ∀ a. a -> Effect { get :: Effect a, set :: a -> Effect Unit }
useRef value = ado
  result <- runEffectFn1 useRef_ value
  in { get: result.get, set: runEffectFn1 result.set }

foreign import useRef_ :: ∀ a. EffectFn1 a { get :: Effect a, set :: EffectFn1 a Unit }
