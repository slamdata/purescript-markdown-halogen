module Main where

import Prelude
import Data.Maybe (maybe)
import qualified Data.StrMap as SM

import DOM
import DOM.BrowserFeatures.Detectors

import Data.BrowserFeatures
import Data.Functor.Coproduct
import Data.NaturalTransformation (Natural())

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Free (Free())
import Control.Plus

import qualified Halogen (modify, get, request, action, runUI, ChildF(..), HalogenF(), QueryF()) as H
import qualified Halogen.Util (appendToBody) as H
import qualified Halogen.Component as H

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Html
import Text.Markdown.SlamDown.Parser

type State =
  { markdown :: String
  , formState :: SlamDownFormState
  }

initialState :: State
initialState =
  { markdown : ""
  , formState : SM.empty
  }

data Query a = DocumentChanged String a

data SlamDownSlot = SlamDownSlot

instance ordSlamDownSlot :: Ord SlamDownSlot where
  compare _ _ = EQ

instance eqSlamDownSlot :: Eq SlamDownSlot where
  eq _ _ = true

type DemoInstalledState g = H.InstalledState State SlamDownState Query SlamDownQuery g SlamDownSlot
type DemoComponent g = H.Component (DemoInstalledState g) (Coproduct Query (H.ChildF SlamDownSlot SlamDownQuery)) g
type DemoRender g = H.RenderParent State SlamDownState Query SlamDownQuery g SlamDownSlot
type DemoEval g = H.EvalParent Query State SlamDownState Query SlamDownQuery g SlamDownSlot
type DemoPeek f g = H.Peek f State SlamDownState Query SlamDownQuery g SlamDownSlot
type DemoQueryF s g = H.QueryF State s Query SlamDownQuery g SlamDownSlot

ui :: forall g. (Plus g) => SlamDownConfig -> DemoComponent g
ui config = H.parentComponent' render eval peek
  where
    render :: DemoRender g
    render state = do
      H.div
        [ P.class_ $ H.className "container" ]
        [ H.h2_ [ H.text "Markdown" ]
        , H.div_
            [ H.textarea
                [ P.class_ $ H.className "form-control"
                , P.value state.markdown
                , E.onValueInput $ E.input DocumentChanged
                ]
            ]
        , H.h2_ [ H.text "HTML Output" ]
        , H.div
            [ P.class_ (H.className "well") ]
            [ H.slot SlamDownSlot \_ ->
                { component : slamDownComponent config
                , initialState : emptySlamDownState
                }
            ]
        , H.h2_ [ H.text "Form State" ]
        , H.pre_ [ H.code_ [ H.text (show state.formState) ] ]
        ]

    eval :: DemoEval g
    eval (DocumentChanged text next) = do
      H.query SlamDownSlot <<< H.action <<< SetDocument $ parseMd text
      updateFormState
      pure next

    peek :: forall f. DemoPeek f g
    peek _ = updateFormState

    updateFormState :: forall s. Free (H.HalogenF State Query (DemoQueryF s g)) Unit
    updateFormState =
      H.query SlamDownSlot (H.request GetFormState) >>=
        maybe (pure unit) \formState -> H.modify (_ { formState = formState })

main :: Eff (avar :: AVAR, err :: EXCEPTION, dom :: DOM) Unit
main = do
  browserFeatures <- detectBrowserFeatures
  let config = { formName : "slamdown-demo-form", browserFeatures : browserFeatures }
  runAff throwException (const (pure unit)) $ do
    app <- H.runUI (ui config) (H.installedState initialState)
    H.appendToBody app.node
