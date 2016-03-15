module Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())

import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural())
import Data.StrMap as SM

import DOM (DOM())
import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

import Text.Markdown.SlamDown.Html (SlamDownConfig(), SlamDownState(), SlamDownQuery(..), SlamDownFormState(), slamDownComponent, emptySlamDownState)
import Text.Markdown.SlamDown.Parser (parseMd)

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

type DemoInstalledState g = H.ParentState State SlamDownState Query SlamDownQuery g SlamDownSlot
type DemoComponent g = H.Component (DemoInstalledState g) (Coproduct Query (H.ChildF SlamDownSlot SlamDownQuery)) g
type DemoHTML g = H.ParentHTML SlamDownState Query SlamDownQuery g SlamDownSlot
type DemoDSL g = H.ParentDSL State SlamDownState Query SlamDownQuery g SlamDownSlot

ui :: forall g. (Functor g) => SlamDownConfig -> DemoComponent g
ui config = H.parentComponent { render, eval, peek: Just peek }
  where
    render :: State -> DemoHTML g
    render state = do
      HH.div
        [ HP.class_ $ HH.className "container" ]
        [ HH.h2_ [ HH.text "Markdown" ]
        , HH.div_
            [ HH.textarea
                [ HP.class_ $ HH.className "form-control"
                , HP.value state.markdown
                , HE.onValueInput $ HE.input DocumentChanged
                ]
            ]
        , HH.h2_ [ HH.text "HTML Output" ]
        , HH.div
            [ HP.class_ (HH.className "well") ]
            [ HH.slot SlamDownSlot \_ ->
                { component : slamDownComponent config
                , initialState : emptySlamDownState
                }
            ]
        , HH.h2_ [ HH.text "Form State" ]
        , HH.pre_ [ HH.code_ [ HH.text (show state.formState) ] ]
        ]

    eval :: Natural Query (DemoDSL g)
    eval (DocumentChanged text next) = do
      H.query SlamDownSlot <<< H.action <<< SetDocument $ parseMd text
      updateFormState
      pure next

    peek :: forall a. H.ChildF SlamDownSlot SlamDownQuery a -> DemoDSL g Unit
    peek _ = updateFormState

    updateFormState :: DemoDSL g Unit
    updateFormState =
      H.query SlamDownSlot (H.request GetFormState) >>=
        maybe (pure unit) \formState -> H.modify (_ { formState = formState })

main :: Eff (avar :: AVAR, err :: EXCEPTION, dom :: DOM) Unit
main = do
  browserFeatures <- detectBrowserFeatures
  let config = { formName : "slamdown-demo-form", browserFeatures : browserFeatures }
  runHalogenAff $
    H.runUI (ui config) (H.parentState initialState) =<< awaitBody
