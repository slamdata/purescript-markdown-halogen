-- | This module defines a component for rendering SlamDown documents to HTML
module Text.Markdown.SlamDown.Halogen.Component
  ( SlamDownConfig(..)
  , defaultBrowserFeatures
  , SlamDownMessage(..)
  , slamDownComponent
  , renderSlamDown
  , evalSlamDownQuery

  , module SDS
  , module SDQ
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array as A
import Data.BrowserFeatures (BrowserFeatures)
import Data.BrowserFeatures.InputType as IT
import Data.Either (Either(..), fromRight)
import Data.Either as E
import Data.Either.Nested (Either3)
import Data.Foldable as F
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.StrMap as SM
import Data.String as S
import Data.Traversable (traverse)
import Data.Validation.Semigroup as V
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.Date as DatePicker
import Halogen.Datepicker.Component.DateTime as DateTimePicker
import Halogen.Datepicker.Component.Time as TimePicker
import Halogen.Datepicker.Component.Types (PickerMessage(..), setValue, value)
import Halogen.Datepicker.Format.Date as DatePickerF
import Halogen.Datepicker.Format.DateTime as DateTimePickerF
import Halogen.Datepicker.Format.Time as TimePickerF
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.Query as SDQ
import Text.Markdown.SlamDown.Halogen.Component.State as SDS
import Text.Markdown.SlamDown.Halogen.Fresh as Fresh
import Text.Markdown.SlamDown.Halogen.InputType as SDIT
import Text.Markdown.SlamDown.Parser.Inline as SDPI
import Text.Markdown.SlamDown.Pretty as SDPR
import Text.Parsing.Parser as P

data SlamDownMessage v
  = TextBoxChanged String (SD.TextBox M.Maybe)
  | CheckBoxChanged String v Boolean
  | RadioButtonChanged String v
  | DropDownChanged String (M.Maybe v)

-- | By default, no features are enabled.
defaultBrowserFeatures ∷ BrowserFeatures
defaultBrowserFeatures =
  { inputTypeSupported : const false
  }

evalSlamDownQuery
  ∷ ∀ m v
  . (SD.Value v)
  ⇒ SlamDownConfig
  → SDQ.SlamDownQuery v
  ~> DSL v m
evalSlamDownQuery config e =
  case e of
    SDQ.ChangeTextBox key tb next → do
      H.modify <<< SDS.modifyFormState $
        SM.insert key (SD.TextBox $ SD.transTextBox (Compose <<< map pure) tb)
      H.raise $ TextBoxChanged key tb
      pure next

    SDQ.ChangeRadioButton key val next → do
      H.modify \state →
        flip SDS.modifyFormState state <<< SM.insert key $
          case SDS.getFormFieldValue key state of
            M.Just rb @ (SD.RadioButtons (Identity _) (Identity vals)) →
              case L.elemIndex val vals of
                M.Just _ → SD.RadioButtons (pure val) (pure vals)
                M.Nothing → rb
            _ → SD.RadioButtons (pure val) (pure (L.singleton val))
      H.raise $ RadioButtonChanged key val
      pure next

    SDQ.ChangeDropDown key mval next → do
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
      H.raise $ DropDownChanged key mval
      pure next

    SDQ.ChangeCheckBox key val checked next → do
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
            sel' = L.fromFoldable $ Set.intersection (Set.fromFoldable vals) $ updateSel $ Set.fromFoldable sel
        update _ = SD.CheckBoxes (pure $ L.fromFoldable $ updateSel Set.empty) (pure $ L.singleton val)

      H.modify \state →
        SDS.modifyFormState
          (SM.insert key <<< update $ SDS.getFormFieldValue key state)
          state
      H.raise $ CheckBoxChanged key val checked
      pure next

    SDQ.GetFormState k → do
      SDS.SlamDownState state ← H.get
      let defaultFormState = SDS.formStateFromDocument state.document
      pure $ k $ SM.union state.formState defaultFormState

    SDQ.PopulateForm values next → do
      H.modify \(SDS.SlamDownState { document }) →
        SDS.syncState document values
      pure next

    SDQ.SetDocument doc next → do
      state ← H.get
      let newState = SDS.replaceDocument doc state
      H.put newState
      let formState = newState # \(SDS.SlamDownState s) → SDS.formStateFromDocument s.document
      SM.foldM (updatePickers config.browserFeatures) unit formState
      pure next

updatePickers :: ∀ m v a
  . BrowserFeatures
  → Unit
  → String
  → SD.FormFieldP Identity a
  → DSL v m Unit
updatePickers {inputTypeSupported} _ label = case _ of
  SD.TextBox t → case t of
    SD.DateTime _ (Compose (Just (Identity v))) | inputTypeSupported IT.DateTimeLocal →
      void (H.query' cpDateTimePicker label $ setValue $ Just $ Right v)
    SD.Time _ (Compose (Just (Identity v))) | inputTypeSupported IT.DateTimeLocal →
      void (H.query' cpTimePicker label $ setValue $ Just $ Right v)
    SD.Date (Compose (Just (Identity v))) | inputTypeSupported IT.DateTimeLocal →
      void (H.query' cpDatePicker label $ setValue $ Just $ Right v)
    _ → pure unit
  _ → pure unit


type SlamDownConfig =
  { formName ∷ String
  , browserFeatures ∷ BrowserFeatures
  }

type FreshRenderer v m a = a → Fresh.Fresh (HTML v m)

type FormFieldDef a =
  { label ∷ String
  , required ∷ Boolean
  , field ∷ SD.FormField a
  }

type FormFieldBaked v m =
  { label ∷ String
  , required ∷ Boolean
  , field ∷ HTML v m
  , ident ∷ M.Maybe String
  }

-- | Render a `SlamDown` document into an HTML form.
renderSlamDown
  ∷ ∀ v m
  . (SD.Value v)
  ⇒ SlamDownConfig
  → SDS.SlamDownState v
  → HTML v m
renderSlamDown config (SDS.SlamDownState state) =
  case state.document of
    SD.SlamDown bs →
      HH.div_
        $ Fresh.runFresh config.formName
        $ renderBlocks bs

  where
    defaultFormState ∷ SDS.SlamDownFormState v
    defaultFormState = SDS.formStateFromDocument state.document

    h_ ∷ Int → Array (HTML v m) → HTML v m
    h_ 1 = HH.h1_
    h_ 2 = HH.h2_
    h_ 3 = HH.h3_
    h_ 4 = HH.h4_
    h_ 5 = HH.h5_
    h_ _ = HH.h6_

    el_ ∷ SD.ListType → Array (HTML v m) → HTML v m
    el_ (SD.Bullet _)  = HH.ul_
    el_ (SD.Ordered _) = HH.ol_

    renderInline ∷ FreshRenderer v m (SD.Inline v)
    renderInline i =
      case i of
        SD.Str s → pure $ HH.text s
        SD.Entity s → pure $ HH.text s
        SD.Space → pure $ HH.text " "
        SD.SoftBreak → pure $ HH.text "\n"
        SD.LineBreak → pure $ HH.br_
        SD.Emph is → HH.em_ <$> traverse renderInline (A.fromFoldable is)
        SD.Strong is → HH.strong_ <$> traverse renderInline (A.fromFoldable is)
        SD.Code _ c → pure $ HH.code_ [ HH.text c ]
        SD.Link body tgt → do
          let
            href (SD.InlineLink url) = url
            href (SD.ReferenceLink tgt') = M.maybe "" ("#" <> _) tgt'
          HH.a [ HP.href $ href tgt ] <$> traverse renderInline (A.fromFoldable body)
        SD.Image body url →
          pure $ HH.img
            [ HP.src url
            , HP.alt $ F.foldMap stripInline body
            ]
        SD.FormField label required field →
          renderInlineFormField <$> bakeFormField { label, required, field }

    bakeFormField ∷ FormFieldDef v → Fresh.Fresh (FormFieldBaked v m)
    bakeFormField { label, required, field } = do
      ident ← Fresh.fresh
      let
        unquote = SD.traverseFormField (SD.getLiteral >>> map pure)
        quote = SD.transFormField (unwrap >>> SD.Literal)
        field' =
          unquote <<< ensureValidField <<< M.maybe field quote $
            SM.lookup label state.formState <|> SM.lookup label defaultFormState
      el ←
        case field' of
          M.Nothing → pure $ HH.text "Unsupported form element"
          M.Just fv → renderFormElement config state ident label fv

      let
        requiresId =
          case field' of
            M.Just (SD.CheckBoxes _ _) → false
            M.Just (SD.RadioButtons _ _) → false
            _ → true
      pure
        { label
        , required
        , field: el
        , ident: if requiresId then M.Just ident else M.Nothing
        }

    renderInlineFormField ∷ FormFieldBaked v m → HTML v m
    renderInlineFormField { field, label, required, ident } =
      HH.span
        [ HP.class_ (HH.ClassName "slamdown-field") ]
        [ HH.label
            (M.maybe [] (pure <<< HP.for) ident)
            [ HH.text (label <> if required then "*" else "") ]
        , field
        ]

    renderFormSetFormField ∷ FormFieldBaked v m → HTML v m
    renderFormSetFormField { field, label, required, ident } =
      HH.tr_
        [ HH.th_
            [ HH.label
                (M.maybe [] (pure <<< HP.for) ident)
                [ HH.text (label <> if required then "*" else "") ]
            ]
        , HH.td_
            [ field ]
        ]

    renderFormSet ∷ FreshRenderer v m (L.List (FormFieldDef v))
    renderFormSet fs =
      HH.table [ HP.class_ (HH.ClassName "slamdown-formset") ]
        <$> traverse (map renderFormSetFormField <<< bakeFormField) (A.fromFoldable fs)

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

    renderBlock ∷ FreshRenderer v m (SD.Block v)
    renderBlock b =
      case b of
        SD.Paragraph is →
          HH.p_ <$> traverse renderInline (A.fromFoldable is)
        SD.Header lvl is →
          h_ lvl <$> traverse renderInline (A.fromFoldable is)
        SD.Blockquote bs →
          HH.blockquote_ <$> renderBlocks bs
        SD.Lst lt bss → do
          let
            item ∷ FreshRenderer v m (L.List (SD.Block v))
            item bs = HH.li_ <$> renderBlocks bs
          el_ lt <$> traverse item (A.fromFoldable bss)
        SD.CodeBlock _ ss →
          pure $ HH.pre_ [ HH.code_ [ HH.text (S.joinWith "\n" $ A.fromFoldable ss) ] ]
        SD.LinkReference l url →
          pure $ HH.p_
            [ HH.text (l <> ": ")
            , HH.a [ HP.id_ l, HP.href url ] [ HH.text url ]
            ]
        SD.Rule →
          pure HH.hr_

    renderBlocks ∷ L.List (SD.Block v) → Fresh.Fresh (Array (HTML v m))
    renderBlocks = go [] L.Nil
      where
      go html fs bs =
        case bs of
          L.Cons (SD.Paragraph (L.Cons (SD.FormField label required field) L.Nil)) bs' →
            go html (L.Cons { label, required, field } fs) bs'
          L.Cons b bs' → do
            bHtml ← renderBlock b
            if L.null fs
              then go (html <> pure bHtml) L.Nil bs'
              else do
                fsHtml ← renderFormSet (L.reverse fs)
                go (html <> pure fsHtml <> pure bHtml) L.Nil bs'
          L.Nil →
            if L.null fs
              then pure html
              else do
                fsHtml ← renderFormSet (L.reverse fs)
                pure (html <> pure fsHtml)

    -- | Make sure that the default value of a form field is valid, and if it is not, strip it out.
    ensureValidField ∷ SD.FormField v → SD.FormField v
    ensureValidField field =
      V.unV
        (const $ stripDefaultValue field)
        id
        (SDPI.validateFormField field)
      where
        stripDefaultValue ∷ SD.FormField v → SD.FormField v
        stripDefaultValue =
          case _ of
             SD.TextBox tb → SD.TextBox $ SD.transTextBox (\_ → Compose M.Nothing) tb
             SD.DropDown _ ls → SD.DropDown M.Nothing ls
             field' → field'

renderRadioButton
  ∷ ∀ m v
  . (SD.Value v)
  ⇒ String  -- label
  → v       -- value
  → FreshRenderer v m Boolean
renderRadioButton label value checked = do
  ident ← Fresh.fresh
  let renderedValue = SD.renderValue value
  pure $ HH.li_
    [ HH.input
        [ HP.checked checked
        , HP.type_ HP.InputRadio
        , HP.id_ ident
        , HP.name label
        , HP.value renderedValue
        , HE.onValueChange (HE.input_ (SDQ.ChangeRadioButton label value))
        ]
    , HH.label [ HP.for ident ] [ HH.text renderedValue ]
    ]

renderCheckBox
  ∷ ∀ m v
  . (SD.Value v)
  ⇒ String  -- label
  → v       -- value
  → FreshRenderer v m Boolean
renderCheckBox label value checked = do
  ident ← Fresh.fresh
  let renderedValue = SD.renderValue value
  pure $ HH.li_
    [ HH.input
        [ HP.checked checked
        , HP.type_ HP.InputCheckbox
        , HP.id_ ident
        , HP.name label
        , HP.value renderedValue
        , HE.onChecked (HE.input (SDQ.ChangeCheckBox label value))
        ]
    , HH.label [ HP.for ident ] [ HH.text renderedValue ]
    ]

renderDropDown
  ∷ ∀ m v
  . (SD.Value v)
  ⇒ String    -- id
  → String    -- label
  → L.List v  -- choices
  → M.Maybe v -- value
  → HTML v m
renderDropDown ident label ls sel =
  HH.select
    [ HP.id_ ident
    , HP.name label
    , HE.onSelectedIndexChange (HE.input (SDQ.ChangeDropDown label <<< L.index ls))
    ]
    $ A.fromFoldable $ M.maybe option option' sel <$> ls
  where
    option ∷ v → HTML v m
    option value =
      let renderedValue = SD.renderValue value
      in HH.option [ HP.value renderedValue ] [ HH.text renderedValue ]

    option' ∷ v → v → HTML v m
    option' sel' value =
      let renderedValue = SD.renderValue value
      in HH.option [ HP.selected (value == sel'), HP.value renderedValue ] [ HH.text renderedValue ]

data PickerType
  = DatePicker
  | TimePicker SD.TimePrecision
  | DateTimePicker SD.TimePrecision

textBoxToPicker ∷ ∀ f
  . BrowserFeatures
  → SD.TextBox f
  → Maybe PickerType
textBoxToPicker features tb = do
  guard (not features.inputTypeSupported $ SDIT.textBoxToInputType tb)
  case tb of
    SD.Date _ → Just DatePicker
    SD.Time p _ → Just $ TimePicker p
    SD.DateTime p _ → Just $ DateTimePicker p
    _ → Nothing

dateFormatString :: String
dateFormatString = "YYYY-MMMM-DD"

timeFormatString :: SD.TimePrecision → String
timeFormatString SD.Minutes = "HH:mm"
timeFormatString SD.Seconds = "HH:mm:ss"

datePickerFormat :: DatePickerF.Format
datePickerFormat = unsafePartial fromRight
  $ DatePickerF.fromString dateFormatString

timePickerFormat :: SD.TimePrecision → TimePickerF.Format
timePickerFormat p = unsafePartial fromRight
  $ TimePickerF.fromString
  $ timeFormatString p

dateTimePickerFormat :: SD.TimePrecision → DateTimePickerF.Format
dateTimePickerFormat p = unsafePartial fromRight
  $ DateTimePickerF.fromString
  $ dateFormatString <> " " <> timeFormatString p

renderFormElement
  ∷ ∀ m v
  . (SD.Value v)
  ⇒ SlamDownConfig
  → SDS.SlamDownStateR v
  → String -- element id
  → String -- label
  → FreshRenderer v m (SDS.FormFieldValue v)
renderFormElement config st ident label field =
  case field of
    SD.TextBox tb →
      pure case textBoxToPicker config.browserFeatures tb of
        Nothing → renderTextInput tb
        Just pickerType → renderPicker pickerType
    SD.RadioButtons (Identity sel) (Identity ls) → do
      let
        renderRadioButton' val = renderRadioButton label val $ sel == val
        options = A.fromFoldable $ if sel `F.elem` ls then ls else L.Cons sel ls
      radios ← traverse renderRadioButton' options
      pure $ HH.ul [ HP.class_ (HH.ClassName "slamdown-radios") ] radios
    SD.CheckBoxes (Identity sel) (Identity ls) → do
      let
        sel' = Set.fromFoldable sel
        renderCheckBox' val = renderCheckBox label val (Set.member val sel')
      checkBoxes ← traverse renderCheckBox' ls
      pure $ HH.ul [ HP.class_ (HH.ClassName "slamdown-checkboxes") ] $ A.fromFoldable checkBoxes
    SD.DropDown msel (Identity ls) →
      pure $
        case msel of
          M.Nothing → renderDropDown ident label ls M.Nothing
          M.Just (Identity sel) →
            let options = if sel `F.elem` ls then ls else L.Cons sel ls
            in renderDropDown ident label options $ M.Just sel

  where
    renderPicker
      ∷ PickerType
      → HTML v m
    renderPicker = case _ of
      DatePicker →
        HH.slot' cpDatePicker label (DatePicker.picker datePickerFormat) unit $ HE.input $
          \(NotifyChange n) → SDQ.ChangeTextBox label $ SD.Date $ value n
      TimePicker p →
        HH.slot' cpTimePicker label (TimePicker.picker $ timePickerFormat p) unit $ HE.input $
          \(NotifyChange n) → SDQ.ChangeTextBox label $ SD.Time SD.Minutes $ value n
      DateTimePicker p →
        HH.slot' cpDateTimePicker label (DateTimePicker.picker $ dateTimePickerFormat p) unit $ HE.input $
          \(NotifyChange n) → SDQ.ChangeTextBox label $ SD.DateTime SD.Minutes $ value n

    renderTextInput
      ∷ SD.TextBox (Compose M.Maybe Identity)
      → HTML v m
    renderTextInput tb =
      HH.input $
        [ HP.type_ compatibleInputType
        , HP.id_ ident
        , HP.name label
        , HE.onValueInput (HE.input (SDQ.ChangeTextBox label <<< parseInput))
        ]
        <> typeSettings
        <> M.maybe [] (\x → [HP.value x]) renderedValue
      where
        renderedValue =
          SDPR.prettyPrintTextBoxValue <$>
            SD.traverseTextBox unwrap tb

        parser ∷ P.Parser String (SD.TextBox Identity)
        parser = SDPI.parseTextBox (\_ → true) (map pure) tb

        parseInput ∷ String → SD.TextBox M.Maybe
        parseInput str =
          case P.runParser str parser of
            E.Right tb' → SD.transTextBox (unwrap >>> pure) tb'
            E.Left err → SD.transTextBox (\_ → M.Nothing) tb

        typeSettings :: forall r i. Array (HP.IProp (step ∷ StepValue | r) i)
        typeSettings = case compatibleInputType, tb of
          HP.InputTime, SD.Time SD.Seconds _ → [ step (Step 1.0) ]
          HP.InputDatetimeLocal, SD.DateTime SD.Seconds _ → [ step (Step 1.0) ]
          _, _ → []

        compatibleInputType =
          SDIT.inputTypeToHalogenInputType $
            SDIT.availableInputType config.browserFeatures tb

step ∷ ∀ r i. StepValue → HP.IProp (step ∷ StepValue | r) i
step = HP.prop (HC.PropName "step")

type ChildQuery = Coproduct3 DatePicker.Query TimePicker.Query DateTimePicker.Query
type Slot = Either3 String String String

cpDatePicker ∷ CP.ChildPath DatePicker.Query ChildQuery String Slot
cpDatePicker = CP.cp1

cpTimePicker ∷ CP.ChildPath TimePicker.Query ChildQuery String Slot
cpTimePicker = CP.cp2

cpDateTimePicker ∷ CP.ChildPath DateTimePicker.Query ChildQuery String Slot
cpDateTimePicker = CP.cp3


type HTML v m = H.ParentHTML (SDQ.SlamDownQuery v) ChildQuery Slot m
type DSL v m = H.ParentDSL (SDS.SlamDownState v) (SDQ.SlamDownQuery v) ChildQuery Slot (SlamDownMessage v) m


-- | Bundles up the SlamDown renderer and state machine into a Halogen component.
slamDownComponent
  ∷ ∀ m v
  . (SD.Value v)
  ⇒ SlamDownConfig
  → H.Component HH.HTML (SDQ.SlamDownQuery v) Unit (SlamDownMessage v) m
slamDownComponent config =
  H.parentComponent
    { render: renderSlamDown config
    , eval: evalSlamDownQuery config
    , initialState: const SDS.emptySlamDownState
    , receiver: const M.Nothing
    }
