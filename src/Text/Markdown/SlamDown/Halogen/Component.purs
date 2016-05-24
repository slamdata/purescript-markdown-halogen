-- | This module defines a component for rendering SlamDown documents to HTML
module Text.Markdown.SlamDown.Halogen.Component
  ( SlamDownConfig(..)
  , defaultBrowserFeatures

  , slamDownComponent
  , renderSlamDown
  , evalSlamDownQuery

  , module SDS
  , module SDQ
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.BrowserFeatures (BrowserFeatures)
import Data.Either as E
import Data.Foldable as F
import Data.Functor.Compose (Compose(..), decompose)
import Data.Identity (Identity(..), runIdentity)
import Data.List as L
import Data.Maybe as M
import Data.Set as Set
import Data.String as S
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Validation as V

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Parser.Inline as SDPI
import Text.Markdown.SlamDown.Pretty as SDPR

import Text.Markdown.SlamDown.Halogen.InputType as SDIT
import Text.Markdown.SlamDown.Halogen.Fresh as Fresh
import Text.Markdown.SlamDown.Halogen.Component.State as SDS
import Text.Markdown.SlamDown.Halogen.Component.Query as SDQ

import Text.Parsing.Parser as P

-- | By default, no features are enabled.
defaultBrowserFeatures ∷ BrowserFeatures
defaultBrowserFeatures =
  { inputTypeSupported : const false
  }

evalSlamDownQuery
  ∷ ∀ g v
  . (SD.Value v)
  ⇒ H.Natural (SDQ.SlamDownQuery v) (H.ComponentDSL (SDS.SlamDownState v) (SDQ.SlamDownQuery v) g)
evalSlamDownQuery e =
  case e of
    SDQ.TextBoxChanged key tb next → do
      H.modify <<< SDS.modifyFormState $
        SM.insert key (SD.TextBox $ SD.transTextBox (Compose <<< map pure) tb)
      pure next

    SDQ.RadioButtonChanged key val next → do
      H.modify \state →
        flip SDS.modifyFormState state <<< SM.insert key $
          case SDS.getFormFieldValue key state of
            M.Just rb @ (SD.RadioButtons (Identity _) (Identity vals)) →
              case L.elemIndex val vals of
                M.Just _ → SD.RadioButtons (pure val) (pure vals)
                M.Nothing → rb
            _ → SD.RadioButtons (pure val) (pure (L.singleton val))
      pure next

    SDQ.DropDownChanged key mval next → do
      H.modify \state →
        flip SDS.modifyFormState state <<< SM.insert key $
          case SDS.getFormFieldValue key state of
            M.Just rb @ (SD.DropDown _ (Identity vals)) →
              case mval of
                M.Just val →
                  if F.elem val vals
                  then SD.DropDown (M.Just $ pure val) (pure vals)
                  else rb
                M.Nothing → SD.DropDown M.Nothing (pure vals)
            _ → SD.DropDown (pure <$> mval) (pure $ M.maybe L.Nil L.singleton mval)
      pure next

    SDQ.CheckBoxChanged key val checked next → do
      let
        updateSel ∷ Set.Set v → Set.Set v
        updateSel =
          if checked
          then Set.insert val
          else Set.delete val

        update ∷ M.Maybe (SDS.FormFieldValue v) → SDS.FormFieldValue v
        update (M.Just cb @ (SD.CheckBoxes (Identity sel) (Identity vals))) =
          SD.CheckBoxes (pure $ sel') (pure vals)
          where
            sel' = Set.toList $ Set.intersection (Set.fromList vals) $ updateSel $ Set.fromList sel
        update _ = SD.CheckBoxes (pure $ Set.toList $ updateSel Set.empty) (pure $ L.singleton val)

      H.modify \state →
        SDS.modifyFormState
          (SM.insert key <<< update $ SDS.getFormFieldValue key state)
          state
      pure next

    SDQ.GetFormState k → do
      SDS.SlamDownState state ← H.get
      let defaultFormState = SDS.formStateFromDocument state.document
      pure $ k $ SM.union state.formState defaultFormState

    SDQ.PopulateForm values next → do
      H.modify \(SDS.SlamDownState { document }) →
        let
          desc = SDS.formDescFromDocument document
          keysToPrune = L.filter (\newKey → not (newKey `SM.member` desc)) (L.toList (SM.keys values))
          prunedValues = F.foldr SM.delete values keysToPrune
        in
          SDS.SlamDownState
            { document
            , formState: prunedValues
            }
      pure next

    SDQ.SetDocument doc next → do
      H.modify $ SDS.replaceDocument doc
      pure next

type SlamDownConfig =
  { formName ∷ String
  , browserFeatures ∷ BrowserFeatures
  }

type FreshRenderer p v a = a → Fresh.Fresh (H.HTML p (SDQ.SlamDownQuery v))

-- | Render a `SlamDown` document into an HTML form.
renderSlamDown
  ∷ ∀ v
  . (SD.Value v)
  ⇒ SlamDownConfig
  → SDS.SlamDownState v
  → H.ComponentHTML (SDQ.SlamDownQuery v)
renderSlamDown config (SDS.SlamDownState state) =
  case state.document of
    SD.SlamDown bs →
      HH.div_
        <<< Fresh.runFresh config.formName
        <<< traverse renderBlock
          $ L.fromList bs

  where
    defaultFormState ∷ SDS.SlamDownFormState v
    defaultFormState = SDS.formStateFromDocument state.document

    h_ ∷ ∀ p a. Int → Array (H.HTML p a) → H.HTML p a
    h_ 1 = HH.h1_
    h_ 2 = HH.h2_
    h_ 3 = HH.h3_
    h_ 4 = HH.h4_
    h_ 5 = HH.h5_
    h_ _ = HH.h6_

    el_ ∷ ∀ p a. SD.ListType → Array (H.HTML p a) → H.HTML p a
    el_ (SD.Bullet _)  = HH.ul_
    el_ (SD.Ordered _) = HH.ol_

    renderInline ∷ ∀ p. FreshRenderer p v (SD.Inline v)
    renderInline i =
      case i of
        SD.Str s → pure $ HH.text s
        SD.Entity s → pure $ HH.text s
        SD.Space → pure $ HH.text " "
        SD.SoftBreak → pure $ HH.text "\n"
        SD.LineBreak → pure $ HH.br_
        SD.Emph is → HH.em_ <$> traverse renderInline (L.fromList is)
        SD.Strong is → HH.strong_ <$> traverse renderInline (L.fromList is)
        SD.Code _ c → pure $ HH.code_ [ HH.text c ]
        SD.Link body tgt → do
          let
            href (SD.InlineLink url) = url
            href (SD.ReferenceLink tgt') = M.maybe "" ("#" ++ _) tgt'
          HH.a [ HP.href $ href tgt ] <$> traverse renderInline (L.fromList body)
        SD.Image body url →
          pure $ HH.img
            [ HP.src url
            , HP.alt $ F.foldMap stripInline body
            ]
        SD.FormField label req def → do
          ident ← Fresh.fresh
          let
            unquote = SD.traverseFormField (SD.getLiteral >>> map pure)
            quote = SD.transFormField (runIdentity >>> SD.Literal)
            field =
              unquote <<< ensureValidField <<< M.maybe def quote $
                SM.lookup label state.formState <|> SM.lookup label defaultFormState
          el ←
            case field of
              M.Nothing → pure $ HH.text "Unsupported form element"
              M.Just fv → renderFormElement config state ident label fv

          let
            requiredLabel = if req then "*" else ""
            requiresId =
              case field of
                M.Just (SD.CheckBoxes _ _) → false
                M.Just (SD.RadioButtons _ _) → false
                _ → true

          pure $ HH.span
            [ HP.class_ (HH.className "slamdown-field") ]
            [ HH.label
              (if requiresId then [ HP.for ident ] else [])
              [ HH.text (label ++ requiredLabel) ]
            , el
            ]

    stripInline ∷ SD.Inline v → String
    stripInline i =
      case i of
        SD.Str s → s
        SD.Entity s → s
        SD.Space → " "
        SD.SoftBreak → "\n"
        SD.LineBreak → "\n"
        SD.Emph is → F.foldMap stripInline is
        SD.Strong is → F.foldMap stripInline is
        SD.Code _ c → c
        SD.Link body _ → F.foldMap stripInline body
        _ → ""

    renderBlock ∷ ∀ p. FreshRenderer p v (SD.Block v)
    renderBlock b =
      case b of
        SD.Paragraph is →
          HH.p_ <$> traverse renderInline (L.fromList is)
        SD.Header lvl is →
          h_ lvl <$> traverse renderInline (L.fromList is)
        SD.Blockquote bs →
          HH.blockquote_ <$> traverse renderBlock (L.fromList bs)
        SD.Lst lt bss → do
          let
            item ∷ FreshRenderer p v (L.List (SD.Block v))
            item bs = HH.li_ <$> traverse renderBlock (L.fromList bs)
          el_ lt <$> traverse item (L.fromList bss)
        SD.CodeBlock _ ss →
          pure $ HH.pre_ [ HH.code_ [ HH.text (S.joinWith "\n" $ L.fromList ss) ] ]
        SD.LinkReference l url →
          pure $ HH.p_
            [ HH.text (l <> ": ")
            , HH.a [ HP.name l, HP.id_ l, HP.href url ] [ HH.text url ]
            ]
        SD.Rule →
          pure HH.hr_

    -- | Make sure that the default value of a form field is valid, and if it is not, strip it out.
    ensureValidField ∷ SD.FormField v → SD.FormField v
    ensureValidField field =
      V.runV
        (const $ stripDefaultValue field)
        id
        (SDPI.validateFormField field)
      where
        stripDefaultValue ∷ SD.FormField v → SD.FormField v
        stripDefaultValue field =
          case field of
             SD.TextBox tb → SD.TextBox $ SD.transTextBox (\_ → Compose M.Nothing) tb
             SD.DropDown _ ls → SD.DropDown M.Nothing ls
             _ → field

renderRadioButton
  ∷ ∀ p v
  . (SD.Value v)
  ⇒ String  -- label
  → v       -- value
  → FreshRenderer p v Boolean
renderRadioButton label value checked = do
  id ← Fresh.fresh
  let renderedValue = SD.renderValue value
  pure $ HH.li_
    [ HH.input
        [ HP.checked checked
        , HP.inputType HP.InputRadio
        , HP.id_ id
        , HP.name label
        , HP.value renderedValue
        , HE.onValueChange (HE.input_ (SDQ.RadioButtonChanged label value))
        ]
    , HH.label [ HP.for id ] [ HH.text renderedValue ]
    ]

renderCheckBox
  ∷ ∀ p v
  . (SD.Value v)
  ⇒ String  -- label
  → v       -- value
  → FreshRenderer p v Boolean
renderCheckBox label value checked = do
  id ← Fresh.fresh
  let renderedValue = SD.renderValue value
  pure $ HH.li_
    [ HH.input
        [ HP.checked checked
        , HP.inputType HP.InputCheckbox
        , HP.id_ id
        , HP.name label
        , HP.value renderedValue
        , HE.onChecked (HE.input (SDQ.CheckBoxChanged label value))
        ]
    , HH.label [ HP.for id ] [ HH.text renderedValue ]
    ]

renderDropDown
  ∷ ∀ p v
  . (SD.Value v)
  ⇒ String    -- id
  → String    -- label
  → L.List v  -- choices
  → M.Maybe v -- value
  → H.HTML p (SDQ.SlamDownQuery v)
renderDropDown id label ls sel =
  HH.select
    [ HP.id_ id
    , HP.name label
    , HE.onSelectedIndexChange (HE.input (SDQ.DropDownChanged label <<< L.index ls))
    ]
    $ L.fromList $ M.maybe option option' sel <$> ls
  where
    option ∷ v → H.HTML p (SDQ.SlamDownQuery v)
    option value =
      let renderedValue = SD.renderValue value
      in HH.option [ HP.value renderedValue ] [ HH.text renderedValue ]

    option' ∷ v → v → H.HTML p (SDQ.SlamDownQuery v)
    option' sel value =
      let renderedValue = SD.renderValue value
      in HH.option [ HP.selected (value == sel), HP.value renderedValue ] [ HH.text renderedValue ]

renderFormElement
  ∷ ∀ p v
  . (SD.Value v)
  ⇒ SlamDownConfig
  → SDS.SlamDownStateR v
  → String -- element id
  → String -- label
  → FreshRenderer p v (SDS.FormFieldValue v)
renderFormElement config st id label field =
  case field of
    SD.TextBox tb →
      pure $ renderTextInput tb
    SD.RadioButtons (Identity sel) (Identity ls) → do
      let
        renderRadioButton' val = renderRadioButton label val $ sel == val
        options = L.fromList $ if sel `F.elem` ls then ls else L.Cons sel ls
      radios ← traverse renderRadioButton' options
      pure $ HH.ul [ HP.class_ (HH.className "slamdown-radios") ] radios
    SD.CheckBoxes (Identity sel) (Identity ls) → do
      let
        sel' = Set.fromList sel
        renderCheckBox' val = renderCheckBox label val (Set.member val sel')
      checkBoxes ← traverse renderCheckBox' ls
      pure $ HH.ul [ HP.class_ (HH.className "slamdown-checkboxes") ] $ L.fromList checkBoxes
    SD.DropDown msel (Identity ls) →
      pure $
        case msel of
          M.Nothing → renderDropDown id label ls M.Nothing
          M.Just (Identity sel) →
            let options = if sel `F.elem` ls then ls else L.Cons sel ls
            in renderDropDown id label options $ M.Just sel

  where
    renderTextInput
      ∷ SD.TextBox (Compose M.Maybe Identity)
      → H.HTML p (SDQ.SlamDownQuery v)
    renderTextInput tb =
      HH.input $
        [ HP.inputType compatibleInputType
        , HP.id_ id
        , HP.name label
        , HE.onValueInput (HE.input (SDQ.TextBoxChanged label <<< parseInput))
        ] <> M.maybe [] (\x → [HP.value x]) renderedValue
      where
        renderedValue =
          SDPR.prettyPrintTextBoxValue <$>
            SD.traverseTextBox decompose tb

        parser ∷ P.Parser String (SD.TextBox Identity)
        parser = SDPI.parseTextBox (\_ → true) (map pure) tb

        parseInput ∷ String → SD.TextBox M.Maybe
        parseInput str =
          case P.runParser str parser of
            E.Right tb' → SD.transTextBox (runIdentity >>> pure) tb'
            E.Left err → SD.transTextBox (\_ → M.Nothing) tb

        compatibleInputType =
          SDIT.inputTypeToHalogenInputType $
            SDIT.availableInputType config.browserFeatures tb

-- | Bundles up the SlamDown renderer and state machine into a Halogen component.
slamDownComponent
  ∷ ∀ g v
  . (SD.Value v)
  ⇒ SlamDownConfig
  → H.Component (SDS.SlamDownState v) (SDQ.SlamDownQuery v) g
slamDownComponent config =
  H.component
    { render: renderSlamDown config
    , eval: evalSlamDownQuery
    }
