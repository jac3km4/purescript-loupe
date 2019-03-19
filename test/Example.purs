module Example where
import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Loupe (Container, Component)
import Loupe (component, container, element) as L
import Loupe.DOM (div, text) as L
import Loupe.DOM.Props as P
import Partial.Unsafe (unsafePartial)
import React as React
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as Web
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toNonElementParentNode) as Web
import Web.HTML.Window (document) as Web

main :: Effect Unit
main = do
  elem <- Web.getElementById "main" <<< Web.toNonElementParentNode =<< Web.document =<< Web.window
  let el = unsafePartial $ fromJust elem
  let cmp = React.createLeafElement counter { }
  _ <- ReactDOM.render cmp el
  pure unit

data Action = Inc

counter :: Container {}
counter = L.container 1 reducer render
  where
    reducer st Inc = pure $ st + 1
    render _ st dispatch =
      L.div [ P.onClick $ const $ dispatch Inc ] [ L.element number ]

number :: ∀ act. Component Int act
number = L.component render
  where
    render st _ = L.text $ show st
