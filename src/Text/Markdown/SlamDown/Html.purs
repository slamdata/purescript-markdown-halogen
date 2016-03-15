-- | This module defines a component for rendering SlamDown documents to HTML

module Text.Markdown.SlamDown.Html
  ( SlamDownQuery(..)
  , SlamDownFormState(..)
  , SlamDownStateR(..)
  , SlamDownState(..)
  , FormFieldValue(..)
  , emptySlamDownState
  , makeSlamDownState

  , SlamDownConfig(..)
  , defaultBrowserFeatures

  , slamDownComponent
  , renderSlamDown
  , evalSlamDownQuery
  )
  where

import Prelude

import Control.Monad.State (State(), evalState)
import Control.Monad.State.Class (get, modify)

import Data.BrowserFeatures (BrowserFeatures())
import Data.BrowserFeatures.InputType as IT
import Data.Foldable (foldMap, foldr, elem)
import Data.List (List(..), mapMaybe, fromList, toList, zipWithA, zip, singleton, filter, nub)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (mempty)
import Data.NaturalTransformation (Natural())
import Data.Set as S
import Data.String (joinWith)
import Data.StrMap as M
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation as V

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as SCG

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Parser.Inline (validateFormField)

data FormFieldValue
  = SingleValue SD.TextBoxType String
  | MultipleValues (S.Set String)

instance arbitraryFormFieldValue :: SC.Arbitrary FormFieldValue where
  arbitrary = do
    b <- SC.arbitrary
    if b
      then SingleValue <$> SC.arbitrary <*> SC.arbitrary
      else MultipleValues <<< S.fromList <<< toList <$> SCG.arrayOf SC.arbitrary

instance showFormFieldValue :: Show FormFieldValue where
  show (SingleValue t s) = "(SingleValue " ++ show t ++ " " ++ show s ++ ")"
  show (MultipleValues ss) = "(MultipleValues " ++ show ss ++ ")"

type SlamDownFormDesc = M.StrMap SD.FormField
type SlamDownFormState = M.StrMap FormFieldValue

type SlamDownStateR =
  { document :: SD.SlamDown
  , formState :: SlamDownFormState
  }

-- | The state of a SlamDown form
newtype SlamDownState = SlamDownState SlamDownStateR

modifySlamDownState :: (SlamDownFormState -> SlamDownFormState) -> SlamDownState -> SlamDownState
modifySlamDownState f (SlamDownState rec) = SlamDownState (rec { formState = f rec.formState })

instance showSlamDownState :: Show SlamDownState where
  show (SlamDownState rec) = "(SlamDownState " ++ show rec.formState ++ ")"

instance arbitrarySlamDownState :: SC.Arbitrary SlamDownState where
  arbitrary = do
    document <- SC.arbitrary
    formState <- M.fromList <<< toList <$> SCG.arrayOf SC.arbitrary
    pure $ SlamDownState
      { document : document
      , formState : formState
      }

-- | By default, no features are enabled.
defaultBrowserFeatures :: BrowserFeatures
defaultBrowserFeatures =
  { inputTypeSupported : const false
  }

-- | The initial empty state of the form, with an empty document.
emptySlamDownState :: SlamDownState
emptySlamDownState =
  SlamDownState
    { document : SD.SlamDown mempty
    , formState : M.empty
    }

-- | The initial state of the form based on a document value. All fields use
-- | their default values.
makeSlamDownState :: SD.SlamDown -> SlamDownState
makeSlamDownState doc =
  SlamDownState
    { document : doc
    , formState : formStateFromDocument doc
    }

formFieldGetDefaultValue :: SD.FormField -> Maybe FormFieldValue
formFieldGetDefaultValue f =
  case f of
    SD.TextBox tbt (Just (SD.Literal value)) ->
      Just $ SingleValue tbt value
    SD.CheckBoxes (SD.Literal sels) (SD.Literal vals) ->
      let chk (Tuple t v) = if t then Just v else Nothing in
      Just $ MultipleValues $ S.fromList $ chk `mapMaybe` zip sels vals
    SD.RadioButtons (SD.Literal value) _ ->
      Just $ SingleValue SD.PlainText value
    SD.DropDown _ (Just (SD.Literal value)) ->
      Just $ SingleValue SD.PlainText value
    _ -> Nothing

formStateFromDocument :: SD.SlamDown -> SlamDownFormState
formStateFromDocument = M.fromList <<< SD.everything (const mempty) phi
  where
    phi :: SD.Inline -> List (Tuple String FormFieldValue)
    phi (SD.FormField label _ field) =
      maybe mempty (singleton <<< Tuple label) $
        V.runV (const Nothing) Just (validateFormField field)
          >>= formFieldGetDefaultValue
    phi _ = mempty

formDescFromDocument :: SD.SlamDown -> SlamDownFormDesc
formDescFromDocument = M.fromList <<< SD.everything (const mempty) phi
  where
    phi :: SD.Inline -> List (Tuple String SD.FormField)
    phi (SD.FormField label _ field) = singleton (Tuple label field)
    phi _ = mempty

changeDocument :: SD.SlamDown -> SlamDownState -> SlamDownState
changeDocument doc (SlamDownState state) =
  SlamDownState
    { document : doc
    , formState : mergedFormState
    }

  where
    formDesc :: SlamDownFormDesc
    formDesc = formDescFromDocument doc

    -- | Returns the keys that are either not present in the new state, or have had their types changed.
    keysToPrune :: Array String
    keysToPrune =
      M.foldMap
        (\key oldVal ->
            case Tuple oldVal (M.lookup key formDesc) of
               Tuple (SingleValue tbt1 _) (Just (SD.TextBox tbt2 _)) -> if tbt1 == tbt2 then [] else [key]
               Tuple _ Nothing -> [key]
               _ -> [])
        state.formState

    prunedFormState :: SlamDownFormState
    prunedFormState = foldr M.delete state.formState keysToPrune

    mergedFormState :: SlamDownFormState
    mergedFormState = prunedFormState `M.union` formStateFromDocument doc

data SlamDownQuery a
  = TextChanged SD.TextBoxType String String a
  | CheckBoxChanged String String Boolean a
  | SetDocument SD.SlamDown a
  | GetFormState (SlamDownFormState -> a)
  | PopulateForm SlamDownFormState a

instance functorSlamDownQuery :: Functor SlamDownQuery where
  map f e =
    case e of
      TextChanged tbt k v a -> TextChanged tbt k v $ f a
      CheckBoxChanged k v b a -> CheckBoxChanged k v b $ f a
      SetDocument doc a -> SetDocument doc $ f a
      GetFormState k -> GetFormState (f <<< k)
      PopulateForm s a -> PopulateForm s $ f a

instance arbitrarySlamDownQuery :: (SC.Arbitrary a) => SC.Arbitrary (SlamDownQuery a) where
  arbitrary = do
    b <- SC.arbitrary
    if b
      then TextChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      else CheckBoxChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary

evalSlamDownQuery :: forall g. Natural SlamDownQuery (H.ComponentDSL SlamDownState SlamDownQuery g)
evalSlamDownQuery e =
  case e of
    TextChanged t key val next -> do
      H.modify <<< modifySlamDownState $
        M.insert key (SingleValue t val)
      pure next
    CheckBoxChanged key val checked next -> do
      let
        updateSet :: Maybe FormFieldValue -> FormFieldValue
        updateSet (Just (MultipleValues s))
          | checked = MultipleValues (S.insert val s)
          | otherwise = MultipleValues (S.delete val s)
        updateSet _
          | checked = MultipleValues (S.singleton val)
          | otherwise = MultipleValues S.empty
      H.modify <<< modifySlamDownState $
        M.alter (Just <<< updateSet) key
      pure next
    GetFormState k -> do
      SlamDownState state <- H.get
      pure $ k state.formState
    PopulateForm values next -> do
      H.modify \(SlamDownState { document }) ->
        let desc = formDescFromDocument document
            keysToPrune = filter (\newKey -> not (newKey `M.member` desc)) (toList (M.keys values))
            prunedValues = foldr M.delete values keysToPrune
        in SlamDownState { document, formState: prunedValues }
      pure next
    SetDocument doc next -> do
      H.modify $ changeDocument doc
      pure next

type SlamDownConfig =
  { formName :: String
  , browserFeatures :: BrowserFeatures
  }

type Fresh = State Int

fresh :: String -> Fresh String
fresh formName = do
  n <- get :: Fresh Int
  modify (+ 1)
  pure (formName ++ "-" ++ show n)

runFresh :: forall a. Fresh a -> a
runFresh m = evalState m 1

type FreshRenderer p a = a -> Fresh (H.HTML p SlamDownQuery)

-- | Render a `SlamDown` document into an HTML form.
renderSlamDown :: SlamDownConfig -> SlamDownState -> H.ComponentHTML SlamDownQuery
renderSlamDown config (SlamDownState state) =
  case state.document of
    SD.SlamDown bs ->
      HH.div_ <<< runFresh <<< traverse renderBlock $ fromList bs

  where
    h_ :: forall p a. Int -> Array (H.HTML p a) -> H.HTML p a
    h_ 1 = HH.h1_
    h_ 2 = HH.h2_
    h_ 3 = HH.h3_
    h_ 4 = HH.h4_
    h_ 5 = HH.h5_
    h_ _ = HH.h6_

    el_ :: forall p a. SD.ListType -> Array (H.HTML p a) -> H.HTML p a
    el_ (SD.Bullet _)  = HH.ul_
    el_ (SD.Ordered _) = HH.ol_

    renderInline :: forall p. FreshRenderer p SD.Inline
    renderInline i =
      case i of
        SD.Str s -> pure $ HH.text s
        SD.Entity s -> pure $ HH.text s
        SD.Space -> pure $ HH.text " "
        SD.SoftBreak -> pure $ HH.text "\n"
        SD.LineBreak -> pure $ HH.br_
        SD.Emph is -> HH.em_ <$> traverse renderInline (fromList is)
        SD.Strong is -> HH.strong_ <$> traverse renderInline (fromList is)
        SD.Code _ c -> pure $ HH.code_ [ HH.text c ]
        SD.Link body tgt -> do
          let
            href (SD.InlineLink url) = url
            href (SD.ReferenceLink tgt') = maybe "" ("#" ++) tgt'
          HH.a [ HP.href $ href tgt ] <$> traverse renderInline (fromList body)
        SD.Image body url ->
          pure $ HH.img
            [ HP.src url
            , HP.alt $ foldMap stripInline body
            ]
        SD.FormField label req el -> do
          ident <- fresh config.formName
          el' <- renderFormElement config state ident label (ensureValidField el)
          let
            requiredLabel = if req then "*" else ""
            requiresId = case el of
              SD.CheckBoxes _ _ -> false
              SD.RadioButtons _ _ -> false
              _ -> true

          pure $ HH.span
            [ HP.class_ (HH.className "slamdown-field") ]
            [ HH.label
              (if requiresId then [ HP.for ident ] else [])
              [ HH.text (label ++ requiredLabel) ]
            , el'
            ]

    stripInline :: SD.Inline -> String
    stripInline i =
      case i of
        SD.Str s -> s
        SD.Entity s -> s
        SD.Space -> " "
        SD.SoftBreak -> "\n"
        SD.LineBreak -> "\n"
        SD.Emph is -> foldMap stripInline is
        SD.Strong is -> foldMap stripInline is
        SD.Code _ c -> c
        SD.Link body _ -> foldMap stripInline body
        _ -> ""

    renderBlock :: forall p. FreshRenderer p SD.Block
    renderBlock b =
      case b of
        SD.Paragraph is ->
          HH.p_ <$> traverse renderInline (fromList is)
        SD.Header lvl is ->
          h_ lvl <$> traverse renderInline (fromList is)
        SD.Blockquote bs ->
          HH.blockquote_ <$> traverse renderBlock (fromList bs)
        SD.Lst lt bss -> do
          let
            item :: FreshRenderer p (List SD.Block)
            item bs = HH.li_ <$> traverse renderBlock (fromList bs)
          el_ lt <$> traverse item (fromList bss)
        SD.CodeBlock _ ss ->
          pure $ HH.pre_ [ HH.code_ [ HH.text (joinWith "\n" $ fromList ss) ] ]
        SD.LinkReference l url ->
          pure $ HH.p_
            [ HH.text (l <> ": ")
            , HH.a [ HP.name l, HP.id_ l, HP.href url ] [ HH.text url ]
            ]
        SD.Rule ->
          pure HH.hr_

    -- | Make sure that the default value of a form field is valid, and if it is not, strip it out.
    ensureValidField :: SD.FormField -> SD.FormField
    ensureValidField field =
      V.runV
        (const $ stripDefaultValue field)
        id
        (validateFormField field)
      where
        stripDefaultValue :: SD.FormField -> SD.FormField
        stripDefaultValue field =
          case field of
             SD.TextBox t _ -> SD.TextBox t Nothing
             SD.DropDown ls _ -> SD.DropDown ls Nothing
             _ -> field

renderRadio :: forall p. String -> String -> String -> FreshRenderer p Boolean
renderRadio formName label value checked = do
  id <- fresh formName
  pure $ HH.li_
    [ HH.input
        [ HP.checked checked
        , HP.inputType HP.InputRadio
        , HP.id_ id
        , HP.name label
        , HP.value value
        , HE.onValueChange (HE.input_ (TextChanged SD.PlainText label value))
        ]
    , HH.label [ HP.for id ] [ HH.text value ]
    ]

renderCheckBox :: forall p. String -> String -> String -> FreshRenderer p Boolean
renderCheckBox formName label value checked = do
  id <- fresh formName
  pure $ HH.li_
    [ HH.input
        [ HP.checked checked
        , HP.inputType HP.InputCheckbox
        , HP.id_ id
        , HP.name label
        , HP.value value
        , HE.onChecked (HE.input (CheckBoxChanged label value))
        ]
    , HH.label [ HP.for id ] [ HH.text value ]
    ]

renderDropDown
  :: forall p
   . String
  -> String
  -> List String
  -> Maybe String
  -> H.HTML p SlamDownQuery
renderDropDown id label ls sel =
  HH.select
    [ HP.id_ id
    , HP.name label
    , HE.onValueChange (HE.input (TextChanged SD.PlainText label))
    ]
    (fromList $ maybe option option' sel <$> ls)
  where
    option :: String -> H.HTML p SlamDownQuery
    option value = HH.option [ HP.value value ] [ HH.text value ]

    option' :: String -> String -> H.HTML p SlamDownQuery
    option' sel value = HH.option [ HP.selected (value == sel), HP.value value ] [ HH.text value ]

availableInputType :: BrowserFeatures -> SD.TextBoxType -> IT.InputType
availableInputType features tbt =
  if features.inputTypeSupported inputType
     then inputType
     else IT.Text
  where
   inputType :: IT.InputType
   inputType = textBoxTypeToInputType tbt

textBoxTypeToInputType :: SD.TextBoxType -> IT.InputType
textBoxTypeToInputType ty =
  case ty of
    SD.PlainText -> IT.Text
    SD.Date -> IT.Date
    SD.Time -> IT.Time
    SD.DateTime -> IT.DateTimeLocal
    SD.Numeric -> IT.Number

inputTypeToHalogenInputType :: IT.InputType -> HP.InputType
inputTypeToHalogenInputType it =
  case it of
    IT.Color -> HP.InputColor
    IT.Date -> HP.InputDate
    IT.DateTime -> HP.InputDatetime
    IT.DateTimeLocal -> HP.InputDatetimeLocal
    IT.Time -> HP.InputTime
    IT.Month -> HP.InputMonth
    IT.Week -> HP.InputWeek
    IT.Email -> HP.InputEmail
    IT.Url -> HP.InputUrl
    IT.Number -> HP.InputNumber
    IT.Search -> HP.InputSearch
    IT.Range -> HP.InputRange
    IT.Text -> HP.InputText

renderFormElement
  :: forall p
   . SlamDownConfig
  -> SlamDownStateR
  -> String
  -> String
  -> FreshRenderer p SD.FormField
renderFormElement config st id label field =
  case field of
    SD.TextBox t Nothing ->
      pure <<< renderTextInput t $ lookupTextValue label Nothing
    SD.TextBox t (Just (SD.Literal value)) ->
      pure <<< renderTextInput t <<< lookupTextValue label $ Just value
    SD.RadioButtons (SD.Literal def) (SD.Literal ls) -> do
      let
        sel = lookupTextValue label $ Just def
        renderRadio' val = renderRadio config.formName label val $ Just val == sel
        options = if def `elem` ls then ls else Cons def ls
      radios <- traverse renderRadio' <<< fromList $ nub options
      pure $ HH.ul [ HP.class_ (HH.className "slamdown-radios") ] radios
    SD.CheckBoxes (SD.Literal bs) (SD.Literal ls) -> do
      checkBoxes <- zipWithA (renderCheckBox config.formName label) ls bs
      pure $ HH.ul [ HP.class_ (HH.className "slamdown-checkboxes") ] $ fromList checkBoxes
    SD.DropDown (SD.Literal ls) Nothing ->
      pure $ renderDropDown id label ls Nothing
    SD.DropDown (SD.Literal ls) (Just (SD.Literal def)) -> do
      let
        options = if def `elem` ls then ls else Cons def ls
        sel = lookupTextValue label $ Just def
      pure $ renderDropDown id label options sel
    _ -> pure $ HH.text "Unsupported form element"

  where
    renderTextInput :: SD.TextBoxType -> Maybe String -> H.HTML p SlamDownQuery
    renderTextInput t value =
      HH.input
        [ HP.inputType compatibleInputType
        , HP.id_ id
        , HP.name label
        , HE.onValueInput (HE.input (TextChanged t label))
        , HP.value (fromMaybe "" value)
        ]
      where
        compatibleInputType =
          inputTypeToHalogenInputType $ availableInputType config.browserFeatures t

    lookupTextValue :: String -> Maybe String -> Maybe String
    lookupTextValue key def =
      case M.lookup key st.formState of
        Just (SingleValue _ val) -> Just val
        _ -> def

    lookupMultipleValues :: String -> List Boolean -> List String -> List Boolean
    lookupMultipleValues key def ls =
      case M.lookup key st.formState of
        Just (MultipleValues val) -> (`S.member` val) <$> ls
        _ -> def

-- | Bundles up the SlamDown renderer and state machine into a Halogen component.
slamDownComponent
  :: forall g
   . SlamDownConfig
  -> H.Component SlamDownState SlamDownQuery g
slamDownComponent config =
  H.component
    { render: renderSlamDown config
    , eval: evalSlamDownQuery
    }
