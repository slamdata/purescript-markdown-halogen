module Main where

import Prelude
import Data.Array ((\\))
import Data.Monoid
import Data.Void
import Data.Tuple
import Data.Either
import Data.Foldable
import qualified Data.StrMap as SM

import DOM
import DOM.BrowserFeatures.Detectors

import Data.BrowserFeatures

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events

import Control.Monad.Eff
import Control.Alternative

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Html
import Text.Markdown.SlamDown.Parser

onLoad :: forall e. Eff (dom :: DOM | e) Unit -> Eff (dom :: DOM | e) Unit
onLoad action = do
  let handler :: DOMEvent -> _
      handler _ = action
  addUIEventListener LoadEvent handler globalWindow

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- document globalWindow
  b <- body w
  appendChild b e

data State = State String SlamDownState

data Input
  = DocumentChanged String
  | FormFieldChanged SlamDownEvent

ui :: forall f eff. (Alternative f) => BrowserFeatures -> Component f Input Input
ui browserFeatures = render <$> stateful (State "" (initSlamDownState $ SlamDown mempty)) update
  where
  render :: State -> H.HTML (f Input)
  render (State md form) =
    H.div [ A.class_ (A.className "container") ]
          [ H.h2_ [ H.text "Markdown" ]
          , H.div_ [ H.textarea [ A.class_ (A.className "form-control")
                                , A.value md
                                , A.onInput (A.input DocumentChanged)
                                ] [] ]
          , H.h2_ [ H.text "HTML Output" ]
          , H.div [ A.class_ (A.className "well") ] (output md form)
          , H.h2_ [ H.text "Form State" ]
          , H.pre_ [ H.code_ [ H.text (show form) ] ]
          ]

  output :: String -> SlamDownState -> Array (H.HTML (f Input))
  output md form = ((FormFieldChanged <$>) <$>) <$> renderHalogen browserFeatures "slamdown-example" form (parseMd md)

  update :: State -> Input -> State
  update (State _ (SlamDownState form)) (DocumentChanged md) =
    let slamdown = parseMd md
    in case initSlamDownState slamdown of
      SlamDownState emptyForm ->
        let currentKeys = SM.keys emptyForm
            oldKeys = SM.keys form \\ currentKeys
            newForm = foldl (\f k -> SM.delete k f) (form `SM.union` emptyForm) oldKeys
        in State md (SlamDownState newForm)
  update (State s form) (FormFieldChanged e) = State s (applySlamDownEvent form e)

main = onLoad do
  browserFeatures <- detectBrowserFeatures
  Tuple node driver <- runUI $ ui browserFeatures
  appendToBody node

