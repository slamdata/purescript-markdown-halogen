module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Text.Markdown.SlamDown.Halogen.Component (SlamDownConfig, SlamDownMessage, SlamDownQuery(..), SlamDownFormState, slamDownComponent)
import Text.Markdown.SlamDown.Parser (parseMd)

type State =
  { markdown ∷ String
  , formState ∷ SlamDownFormState String
  }

initialState ∷ State
initialState =
  { markdown : ""
  , formState : SM.empty
  }

data Query a
  = ChangeDocument String a
  | HandleFormChange (SlamDownMessage String) a

data SlamDownSlot = SlamDownSlot

derive instance ordSlamDownSlot ∷ Ord SlamDownSlot
derive instance eqSlamDownSlot ∷ Eq SlamDownSlot

type DemoComponent m = H.Component HH.HTML Query Unit Void m
type DemoHTML m = H.ParentHTML Query (SlamDownQuery String) SlamDownSlot m
type DemoDSL m = H.ParentDSL State Query (SlamDownQuery String) SlamDownSlot Void m

ui ∷ ∀ m. SlamDownConfig → DemoComponent m
ui config =
  H.parentComponent
    { render
    , eval
    , initialState: const initialState
    , receiver: const Nothing
    }
  where
    render ∷ State → DemoHTML m
    render state = do
      HH.div
        [ HP.class_ $ HH.ClassName "container" ]
        [ HH.h2_ [ HH.text "Markdown" ]
        , HH.div_
            [ HH.textarea
                [ HP.class_ $ HH.ClassName "form-control"
                , HP.value state.markdown
                , HE.onValueInput $ HE.input ChangeDocument
                ]
            ]
        , HH.h2_ [ HH.text "HTML Output" ]
        , HH.div
            [ HP.class_ (HH.ClassName "well") ]
            [ HH.slot SlamDownSlot (slamDownComponent config)
                unit (Just <<< H.action <<< HandleFormChange)
            ]
        , HH.h2_ [ HH.text "Form State" ]
        , HH.pre_ [ HH.code_ [ HH.text (show state.formState) ] ]
        ]

    eval ∷ Query ~> DemoDSL m
    eval (ChangeDocument text next) = do
      for_ (parseMd text) \md →
        H.query SlamDownSlot $ H.action $ SetDocument md
      updateFormState
      pure next
    eval (HandleFormChange _ next) = do
      updateFormState
      pure next

    updateFormState ∷ DemoDSL m Unit
    updateFormState =
      H.query SlamDownSlot (H.request GetFormState) >>=
        maybe (pure unit) \formState → H.modify (_ { formState = formState })

main ∷ Eff (HA.HalogenEffects ()) Unit
main = do
  browserFeatures ← detectBrowserFeatures
  let config = { formName : "slamdown-demo-form", browserFeatures : browserFeatures }
  HA.runHalogenAff $
    runUI (ui config) unit =<< HA.awaitBody
