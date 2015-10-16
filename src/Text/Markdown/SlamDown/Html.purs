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
import Control.Alternative (Alternative)
import Control.Monad.State (State(), evalState)
import Control.Monad.State.Class (get, modify)

import Data.Foldable (foldMap, foldl)
import Data.List (List(..), mapMaybe, fromList, toList, zipWithA, zip, singleton)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import qualified Data.Validation as V

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Inline (validateFormField, validateTextOfType)

import qualified Data.Array as Array
import qualified Data.Set as S
import qualified Data.StrMap as M
import qualified Halogen (get, modify) as H
import qualified Halogen.Component as H
import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Data.BrowserFeatures
import qualified Data.BrowserFeatures.InputType as IT

import qualified Test.StrongCheck as SC
import qualified Test.StrongCheck.Gen as SC

data FormFieldValue
  = SingleValue TextBoxType String
  | MultipleValues (S.Set String)

instance arbitraryFormFieldValue :: SC.Arbitrary FormFieldValue where
  arbitrary = do
    b <- SC.arbitrary
    if b
      then SingleValue <$> SC.arbitrary <*> SC.arbitrary
      else MultipleValues <<< S.fromList <<< toList <$> SC.arrayOf SC.arbitrary

instance showFormFieldValue :: Show FormFieldValue where
  show (SingleValue t s) = "(SingleValue " ++ show t ++ " " ++ show s ++ ")"
  show (MultipleValues ss) = "(MultipleValues " ++ show ss ++ ")"

type SlamDownFormDesc = M.StrMap FormField
type SlamDownFormState = M.StrMap FormFieldValue

type SlamDownStateR =
  { document :: SlamDown
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
    formState <- M.fromList <<< toList <$> SC.arrayOf SC.arbitrary
    pure $ SlamDownState
      { document : document
      , formState : formState
      }

-- | By default, all features are enabled.
defaultBrowserFeatures :: BrowserFeatures
defaultBrowserFeatures =
  { inputTypeSupported : \_ -> true
  }

emptySlamDownState :: SlamDownState
emptySlamDownState =
  SlamDownState
    { document : SlamDown mempty
    , formState : M.empty
    }

formFieldGetDefaultValue :: FormField -> Maybe FormFieldValue
formFieldGetDefaultValue f =
  case f of
    TextBox tbt (Just (Literal value)) ->
      Just $ SingleValue tbt value
    CheckBoxes (Literal sels) (Literal vals) ->
      let chk (Tuple t v) = if t then Just v else Nothing in
      Just $ MultipleValues $ S.fromList $ chk `mapMaybe` zip sels vals
    RadioButtons (Literal value) _ ->
      Just $ SingleValue PlainText value
    DropDown _ (Just (Literal value)) ->
      Just $ SingleValue PlainText value
    _ -> Nothing

formStateFromDocument :: SlamDown -> SlamDownFormState
formStateFromDocument = M.fromList <<< everything (const mempty) phi
  where
    phi :: Inline -> List (Tuple String FormFieldValue)
    phi (FormField label _ field) =
      maybe mempty (singleton <<< Tuple label) $
        V.runV (const Nothing) Just (validateFormField field)
          >>= formFieldGetDefaultValue
    phi _ = mempty

formDescFromDocument :: SlamDown -> SlamDownFormDesc
formDescFromDocument = M.fromList <<< everything (const mempty) phi
  where
    phi :: Inline -> List (Tuple String FormField)
    phi (FormField label _ field) = singleton (Tuple label field)
    phi _ = mempty

-- | The initial state of form, in which all fields use their default values
makeSlamDownState :: SlamDown -> SlamDownState
makeSlamDownState doc =
  SlamDownState
    { document : doc
    , formState : formStateFromDocument doc
    }

changeDocument :: SlamDown -> SlamDownState -> SlamDownState
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
               Tuple (SingleValue tbt1 _) (Just (TextBox tbt2 _)) -> if tbt1 == tbt2 then [] else [key]
               Tuple _ Nothing -> [key]
               _ -> [])
        state.formState

    prunedFormState :: SlamDownFormState
    prunedFormState = foldl (flip M.delete) state.formState keysToPrune

    mergedFormState :: SlamDownFormState
    mergedFormState = prunedFormState `M.union` formStateFromDocument doc

data SlamDownQuery a
  = TextChanged TextBoxType String String a
  | CheckBoxChanged String String Boolean a
  | SetDocument SlamDown a
  | GetFormState (SlamDownFormState -> a)

instance functorSlamDownQuery :: Functor SlamDownQuery where
  map f e =
    case e of
      TextChanged tbt k v a -> TextChanged tbt k v $ f a
      CheckBoxChanged k v b a -> CheckBoxChanged k v b $ f a
      SetDocument doc a -> SetDocument doc $ f a
      GetFormState k -> GetFormState (f <<< k)

instance arbitrarySlamDownQuery :: (SC.Arbitrary a) => SC.Arbitrary (SlamDownQuery a) where
  arbitrary = do
    b <- SC.arbitrary
    if b
      then TextChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      else CheckBoxChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary

evalSlamDownQuery :: forall g. H.Eval SlamDownQuery SlamDownState SlamDownQuery g
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
    SetDocument doc next -> do
      H.modify $ changeDocument doc
      pure next

type SlamDownConfig =
  { browserFeatures :: BrowserFeatures
  , formName :: String
  }

type Fresh = State Int

fresh :: String -> Fresh String
fresh formName = do
  n <- get :: Fresh Int
  modify (+ 1)
  pure (formName ++ "-" ++ show n)

runFresh :: forall a. Fresh a -> a
runFresh m = evalState m 1

type FreshRenderer p a = a -> Fresh (H.HTML p (SlamDownQuery Unit))

-- | Render a `SlamDown` document into an HTML form.
renderSlamDown :: SlamDownConfig -> H.Render SlamDownState SlamDownQuery
renderSlamDown config (SlamDownState state) =
  case state.document of
    SlamDown bs ->
      H.div_ <<< runFresh <<< traverse renderBlock $ fromList bs

  where
    h_ :: forall p a. Int -> Array (H.HTML p a) -> H.HTML p a
    h_ 1 = H.h1_
    h_ 2 = H.h2_
    h_ 3 = H.h3_
    h_ 4 = H.h4_
    h_ 5 = H.h5_
    h_ _ = H.h6_

    el_ :: forall p a. ListType -> Array (H.HTML p a) -> H.HTML p a
    el_ (Bullet _)  = H.ul_
    el_ (Ordered _) = H.ol_

    renderInline :: forall p. FreshRenderer p Inline
    renderInline i =
      case i of
        Str s -> pure $ H.text s
        Entity s -> pure $ H.text s
        Space -> pure $ H.text " "
        SoftBreak -> pure $ H.text "\n"
        LineBreak -> pure $ H.br_
        Emph is -> H.em_ <$> traverse renderInline (fromList is)
        Strong is -> H.strong_ <$> traverse renderInline (fromList is)
        Code _ c -> pure $ H.code_ [ H.text c ]
        Link body tgt -> do
          let
            href (InlineLink url) = url
            href (ReferenceLink tgt) = maybe "" ("#" ++) tgt
          H.a [ P.href $ href tgt ] <$> traverse renderInline (fromList body)
        Image body url ->
          pure $ H.img
            [ P.src url
            , P.alt $ foldMap stripInline body
            ]
        FormField label req el -> do
          ident <- fresh config.formName
          el' <- renderFormElement config state ident label (ensureValidField el)
          let
            requiredLabel = if req then "*" else ""
            requiresId = case el of
              CheckBoxes _ _ -> false
              RadioButtons _ _ -> false
              _ -> true

          pure $ H.span
            [ P.class_ (H.className "slamdown-field") ]
            [ H.label
              (if requiresId then [ P.for ident ] else [])
              [ H.text (label ++ requiredLabel) ]
            , el'
            ]

    stripInline :: Inline -> String
    stripInline i =
      case i of
        Str s -> s
        Entity s -> s
        Space -> " "
        SoftBreak -> "\n"
        LineBreak -> "\n"
        Emph is -> foldMap stripInline is
        Strong is -> foldMap stripInline is
        Code _ c -> c
        Link body _ -> foldMap stripInline body
        _ -> ""

    renderBlock :: forall p. FreshRenderer p Block
    renderBlock b =
      case b of
        Paragraph is ->
          H.p_ <$> traverse renderInline (fromList is)
        Header lvl is ->
          h_ lvl <$> traverse renderInline (fromList is)
        Blockquote bs ->
          H.blockquote_ <$> traverse renderBlock (fromList bs)
        Lst lt bss -> do
          let
            item :: FreshRenderer p (List Block)
            item bs = H.li_ <$> traverse renderBlock (fromList bs)
          el_ lt <$> traverse item (fromList bss)
        CodeBlock _ ss ->
          pure $ H.pre_ [ H.code_ [ H.text (joinWith "\n" $ fromList ss) ] ]
        LinkReference l url ->
          pure $ H.p_
            [ H.text (l <> ": ")
            , H.a [ P.name l, P.id_ l, P.href url ] [ H.text url ]
            ]
        Rule ->
          pure H.hr_

    -- | Make sure that the default value of a form field is valid, and if it is not, strip it out.
    ensureValidField :: FormField -> FormField
    ensureValidField field =
      V.runV
        (const $ stripDefaultValue field)
        id
        (validateFormField field)
      where
        stripDefaultValue :: FormField -> FormField
        stripDefaultValue field =
          case field of
             TextBox t _ -> TextBox t Nothing
             DropDown ls _ -> DropDown ls Nothing
             _ -> field

renderRadio :: forall p. String -> String -> String -> FreshRenderer p Boolean
renderRadio formName label value checked = do
  id <- fresh formName
  pure $ H.li_
    [ H.input
        [ P.checked checked
        , P.inputType P.InputRadio
        , P.id_ id
        , P.name label
        , P.value value
        , E.onValueChange (E.input_ (TextChanged PlainText label value))
        ]
    , H.label [ P.for id ] [ H.text value ]
    ]

renderCheckBox :: forall p. String -> String -> String -> FreshRenderer p Boolean
renderCheckBox formName label value checked = do
  id <- fresh formName
  pure $ H.li_
    [ H.input
        [ P.checked checked
        , P.inputType P.InputRadio
        , P.id_ id
        , P.name label
        , P.value value
        , E.onChecked (E.input (CheckBoxChanged label value))
        ]
    , H.label [ P.for id ] [ H.text value ]
    ]

renderDropDown
  :: forall p
   . String
  -> String
  -> List String
  -> Maybe String
  -> H.HTML p (SlamDownQuery Unit)
renderDropDown id label ls sel =
  H.select
    [ P.id_ id
    , P.name label
    , E.onValueChange (E.input (TextChanged PlainText label))
    ]
    (fromList $ maybe option option' sel <$> ls)
  where
    option :: String -> H.HTML p (SlamDownQuery Unit)
    option value = H.option [ P.value value ] [ H.text value ]

    option' :: String -> String -> H.HTML p (SlamDownQuery Unit)
    option' sel value = H.option [ P.selected (value == sel), P.value value ] [ H.text value ]

availableInputType :: BrowserFeatures -> TextBoxType -> IT.InputType
availableInputType features tbt =
  if features.inputTypeSupported inputType
     then inputType
     else IT.Text
  where
   inputType :: IT.InputType
   inputType = textBoxTypeToInputType tbt

textBoxTypeToInputType :: TextBoxType -> IT.InputType
textBoxTypeToInputType ty =
  case ty of
    PlainText -> IT.Text
    Date -> IT.Date
    Time -> IT.Time
    DateTime -> IT.DateTimeLocal
    Numeric -> IT.Number

inputTypeToHalogenInputType :: IT.InputType -> P.InputType
inputTypeToHalogenInputType it =
  case it of
    IT.Color -> P.InputColor
    IT.Date -> P.InputDate
    IT.DateTime -> P.InputDatetime
    IT.DateTimeLocal -> P.InputDatetimeLocal
    IT.Time -> P.InputTime
    IT.Month -> P.InputMonth
    IT.Week -> P.InputWeek
    IT.Email -> P.InputEmail
    IT.Url -> P.InputUrl
    IT.Number -> P.InputNumber
    IT.Search -> P.InputSearch
    IT.Range -> P.InputRange
    IT.Text -> P.InputText

renderFormElement
  :: forall p
   . SlamDownConfig
  -> SlamDownStateR
  -> String
  -> String
  -> FreshRenderer p FormField
renderFormElement config st id label field =
  case field of
    TextBox t Nothing ->
      pure <<< renderTextInput t $ lookupTextValue label Nothing
    TextBox t (Just (Literal value)) ->
      pure <<< renderTextInput t <<< lookupTextValue label $ Just value
    RadioButtons (Literal def) (Literal ls) -> do
      let
        sel = lookupTextValue label $ Just def
        renderRadio' val = renderRadio config.formName label val $ Just val == sel
      radios <- traverse renderRadio' <<< fromList $ Cons def ls
      pure $ H.ul [ P.class_ (H.className "slamdown-radios") ] radios
    CheckBoxes (Literal bs) (Literal ls) -> do
      let bools = lookupMultipleValues label bs ls
      checkBoxes <- zipWithA (renderCheckBox config.formName label) ls bools
      pure $ H.ul [ P.class_ (H.className "slamdown-checkboxes") ] $ fromList checkBoxes
    DropDown (Literal ls) Nothing ->
      pure $ renderDropDown id label (Cons "" ls) Nothing
    DropDown (Literal ls) (Just (Literal sel)) ->
      pure $ renderDropDown id label ls (lookupTextValue label (Just sel))
    _ -> pure $ H.text "Unsupported form element"

  where
    renderTextInput :: TextBoxType -> Maybe String -> H.HTML p (SlamDownQuery Unit)
    renderTextInput t value =
      H.input
        [ P.inputType compatibleInputType
        , P.id_ id
        , P.name label
        , E.onValueInput (E.input (TextChanged t label))
        , P.value (fromMaybe "" value)
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
slamDownComponent :: forall g. SlamDownConfig -> H.Component SlamDownState SlamDownQuery g
slamDownComponent config = H.component (renderSlamDown config) evalSlamDownQuery
