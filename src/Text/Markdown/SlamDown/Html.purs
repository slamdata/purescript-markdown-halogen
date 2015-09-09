-- | This module defines functions for rendering Markdown to HTML.

module Text.Markdown.SlamDown.Html
  ( SlamDownEvent()
  , SlamDownState(..)
  , FormFieldValue(..)
  , defaultBrowserFeatures
  , initSlamDownState
  , applySlamDownEvent
  , renderHalogen
  )
  where

import Prelude
import Control.Alternative (Alternative)
import Control.Monad.State (State(), evalState)
import Control.Monad.State.Class (get, modify)

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List (List(..), mapMaybe, fromList, toList, zipWithA, zip, singleton)
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import qualified Data.Validation as V

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Inline (validateFormField, validateTextOfType)

import qualified Data.Array as Array
import qualified Data.Set as S
import qualified Data.StrMap as M
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

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

-- | The state of a SlamDown form - a mapping from input keys to values
newtype SlamDownState = SlamDownState (M.StrMap FormFieldValue)

instance showSlamDownState :: Show SlamDownState where
  show (SlamDownState m) = "(SlamDownState " ++ show m ++ ")"

instance arbitrarySlamDownState :: SC.Arbitrary SlamDownState where
  arbitrary = SlamDownState <<< M.fromList <<< toList <$> SC.arrayOf SC.arbitrary

-- | By default, all features are enabled.
defaultBrowserFeatures :: BrowserFeatures
defaultBrowserFeatures =
  { inputTypeSupported : \_ -> true
  }

-- | The initial state of form, in which all fields use their default values
initSlamDownState :: SlamDown -> SlamDownState
initSlamDownState = SlamDownState <<< M.fromList <<< everything (const mempty) go
  where
  go :: Inline -> List (Tuple String FormFieldValue)
  go (FormField label _ field) =
    maybe mempty (singleton <<< Tuple label) $
      V.runV (const Nothing) Just (validateFormField field) >>= getDefaultValue
  go _ = mempty

  getDefaultValue :: FormField -> Maybe FormFieldValue
  getDefaultValue (TextBox tbt (Just (Literal value))) = Just $ SingleValue tbt value
  getDefaultValue (CheckBoxes (Literal sels) (Literal vals)) = Just $ MultipleValues $ S.fromList $ chk `mapMaybe` zip sels vals
    where
    chk (Tuple true value) = Just value
    chk _ = Nothing
  getDefaultValue (RadioButtons (Literal value) _) = Just $ SingleValue PlainText value
  getDefaultValue (DropDown _ (Just (Literal value))) = Just $ SingleValue PlainText value
  getDefaultValue _ = Nothing

-- | The type of events which can be raised by SlamDown forms
data SlamDownEvent
  = TextChanged TextBoxType String String
  | CheckBoxChanged String String Boolean

instance arbitrarySlamDownEvent :: SC.Arbitrary SlamDownEvent where
  arbitrary = do
    b <- SC.arbitrary
    if b
      then TextChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      else CheckBoxChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary


-- | Apply a `SlamDownEvent` to a `SlamDownState`.
applySlamDownEvent :: SlamDownState -> SlamDownEvent -> SlamDownState
applySlamDownEvent (SlamDownState m) (TextChanged t key val) =
  SlamDownState (M.insert key (SingleValue t val) m)
applySlamDownEvent (SlamDownState m) (CheckBoxChanged key val checked) =
  SlamDownState (M.alter (Just <<< updateSet) key m)
  where
  updateSet :: Maybe FormFieldValue -> FormFieldValue
  updateSet (Just (MultipleValues s))
    | checked = MultipleValues (S.insert val s)
    | otherwise = MultipleValues (S.delete val s)
  updateSet _
    | checked = MultipleValues (S.singleton val)
    | otherwise = MultipleValues S.empty

type Fresh = State Int

-- | Render the SlamDown AST to an arbitrary Halogen HTML representation
renderHalogen :: forall f. (Alternative f) => BrowserFeatures -> String -> SlamDownState -> SlamDown -> Array (H.HTML (f SlamDownEvent))
renderHalogen featureSupport formName (SlamDownState m) (SlamDown bs) = evalState (traverse renderBlock (fromList bs)) 1

  where

  renderBlock :: Block -> Fresh (H.HTML (f SlamDownEvent))
  renderBlock (Paragraph is) = H.p_ <$> traverse renderInline (fromList is)
  renderBlock (Header level is) = h_ level <$> traverse renderInline (fromList is)
    where
    h_ :: forall a. Int -> Array (H.HTML (f a)) -> H.HTML (f a)
    h_ 1 = H.h1_
    h_ 2 = H.h2_
    h_ 3 = H.h3_
    h_ 4 = H.h4_
    h_ 5 = H.h5_
    h_ 6 = H.h6_
  renderBlock (Blockquote bs) = H.blockquote_ <$> traverse renderBlock (fromList bs)
  renderBlock (Lst lt bss) = el_ lt <$> traverse item (fromList bss)
    where
    el_ :: forall a. ListType -> Array (H.HTML (f a)) -> H.HTML (f a)
    el_ (Bullet _)  = H.ul_
    el_ (Ordered _) = H.ol_
    item :: List Block -> Fresh (H.HTML (f SlamDownEvent))
    item bs = H.li_ <$> traverse renderBlock (fromList bs)
  renderBlock (CodeBlock _ ss) =
    pure $ H.pre_ [ H.code_ [ H.text (joinWith "\n" $ fromList ss) ] ]
  renderBlock (LinkReference l url) =
    pure $ H.p_ [ H.text (l <> ": ")
                , H.a [ A.name l, A.id_ l, A.href url ] [ H.text url ]
                ]
  renderBlock Rule = pure $ H.hr_ []

  renderInline :: Inline -> Fresh (H.HTML (f SlamDownEvent))
  renderInline (Str s) = pure $ H.text s
  renderInline (Entity s) = pure $ H.text s
  renderInline Space = pure $ H.text " "
  renderInline SoftBreak = pure $ H.text "\n"
  renderInline LineBreak = pure $ H.br_ []
  renderInline (Emph is) = H.em_ <$> traverse renderInline (fromList is)
  renderInline (Strong is) = H.strong_ <$> traverse renderInline (fromList is)
  renderInline (Code _ c) = pure $ H.code_ [ H.text c ]
  renderInline (Link body tgt) = H.a [ A.href (href tgt) ] <$> traverse renderInline (fromList body)
    where
    href (InlineLink url) = url
    href (ReferenceLink tgt) = maybe "" ("#" ++) tgt
  renderInline (Image body url) = H.img [ A.src url ] <$> traverse renderInline (fromList body)
  renderInline (FormField label req el) = do
    id <- fresh
    el' <- renderFormElement id label (ensureValidField el)
    pure $ H.span [ A.class_ (A.className "slamdown-field") ]
                  [ H.label (if requiresId then [ A.for id ] else [])
                            [ H.text (label ++ requiredLabel) ]
                  , el'
                  ]
    where
    requiredLabel = if req then "*" else ""
    requiresId = case el of
      CheckBoxes _ _ -> false
      RadioButtons _ _ -> false
      _ -> true

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

  renderFormElement :: String -> String -> FormField -> Fresh (H.HTML (f SlamDownEvent))
  renderFormElement id label (TextBox t Nothing) =
    pure $ renderTextInput featureSupport id label t (lookupTextValue label Nothing)
  renderFormElement id label (TextBox t (Just (Literal value))) =
    pure $ renderTextInput featureSupport id label t (lookupTextValue label (Just value))
  renderFormElement _ label (RadioButtons (Literal def) (Literal ls)) =
    H.ul [ A.class_ (A.className "slamdown-radios") ] <$> traverse (\val -> radio (Just val == sel) val) (fromList (Cons def ls))
    where
    sel = lookupTextValue label (Just def)
    radio checked value = do
      id <- fresh
      pure $ H.li_ [ H.input [ A.checked checked
                             , A.type_ "radio"
                             , A.id_ id
                             , A.name label
                             , A.value value
                             , E.onChange (E.input_ (TextChanged PlainText label value))
                             ] []
                   , H.label [ A.for id ] [ H.text value ]
                   ]
  renderFormElement _ label (CheckBoxes (Literal bs) (Literal ls)) =
    H.ul [ A.class_ (A.className "slamdown-checkboxes") ] <$> (fromList <$> (zipWithA checkBox (lookupMultipleValues label bs ls) ls))
    where
    checkBox checked value = do
      id <- fresh
      pure $ H.li_ [ H.input [ A.checked checked
                             , A.type_ "checkbox"
                             , A.id_ id
                             , A.name label
                             , A.value value
                             , E.onChecked (E.input (CheckBoxChanged label value))
                             ] []
                   , H.label [ A.for id ] [ H.text value ]
                   ]
  renderFormElement id label (DropDown (Literal ls) Nothing) = do
    pure $ renderDropDown id label (Cons "" ls) Nothing
  renderFormElement id label (DropDown (Literal ls) (Just (Literal sel))) = do
    pure $ renderDropDown id label ls (lookupTextValue label (Just sel))
  renderFormElement _ _ _ = pure $ unsupportedFormElement

  lookupTextValue :: String -> Maybe String -> Maybe String
  lookupTextValue key def =
    case M.lookup key m of
      Just (SingleValue _ val) -> Just val
      _ -> def

  lookupMultipleValues :: String -> List Boolean -> List String -> List Boolean
  lookupMultipleValues key def ls =
    case M.lookup key m of
      Just (MultipleValues val) -> (`S.member` val) <$> ls
      _ -> def

  fresh :: Fresh String
  fresh = do
    n <- get :: Fresh Int
    modify (+ 1)
    pure (formName ++ "-" ++ show n)

availableInputType :: BrowserFeatures -> TextBoxType -> IT.InputType
availableInputType features tbt =
  if features.inputTypeSupported inputType
     then inputType
     else IT.Text
  where
   inputType :: IT.InputType
   inputType = textBoxTypeToInputType tbt

textBoxTypeToInputType :: TextBoxType -> IT.InputType
textBoxTypeToInputType PlainText = IT.Text
textBoxTypeToInputType Date = IT.Date
textBoxTypeToInputType Time = IT.Time
textBoxTypeToInputType DateTime = IT.DateTimeLocal
textBoxTypeToInputType Numeric = IT.Number

renderTextInput :: forall f. (Alternative f) => BrowserFeatures -> String -> String -> TextBoxType -> Maybe String -> H.HTML (f SlamDownEvent)
renderTextInput browserFeatures id label t value =
  H.input ([ A.type_ (IT.renderInputType $ availableInputType browserFeatures t)
           , A.id_ id
           , A.name label
           , E.onInput (E.input (TextChanged t label))
           , A.value (fromMaybe "" value)
           ]) []

renderDropDown :: forall f. (Alternative f) => String -> String -> List String -> Maybe String -> H.HTML (f SlamDownEvent)
renderDropDown id label ls sel =
  H.select [ A.id_ id
           , A.name label
           , E.onValueChanged (E.input (TextChanged PlainText label))
           ]
           (fromList $ maybe option option' sel <$> ls)
  where
  option :: String -> H.HTML (f SlamDownEvent)
  option value = H.option [ A.value value ] [ H.text value ]
  option' :: String -> String -> H.HTML (f SlamDownEvent)
  option' sel value = H.option [ A.selected (value == sel), A.value value ] [ H.text value ]

textBoxTypeName :: TextBoxType -> String
textBoxTypeName t = case t of
  PlainText -> "text"
  Date -> "date"
  Time -> "time"
  DateTime -> "datetime-local"
  Numeric -> "number"

unsupportedFormElement :: forall a. H.HTML a
unsupportedFormElement = H.text "Unsupported form element"
