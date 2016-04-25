module Text.Markdown.SlamDown.Halogen.Component.State
  ( FormFieldValue
  , SlamDownFormDesc
  , SlamDownFormState
  , SlamDownStateR
  , SlamDownState(..)

  , emptySlamDownState

  , getDocument
  , getFormState
  , modifyFormState

  , replaceDocument

  , formDescFromDocument
  , formStateFromDocument

  , formFieldGetDefaultValue
  , getFormFieldValue
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Foldable as F
import Data.Identity as Id
import Data.List as L
import Data.Maybe as M
import Data.Monoid (mempty)
import Data.NaturalTransformation (Natural)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Validation as V
import Test.StrongCheck as SC

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Traverse as SDT
import Text.Markdown.SlamDown.Parser.Inline as SDPI

type FormFieldValue = SD.FormFieldP Id.Identity
type SlamDownFormDesc a = SM.StrMap (SD.FormField a)
type SlamDownFormState a = SM.StrMap (FormFieldValue a)

type SlamDownStateR a =
  { document ∷ SD.SlamDownP a
  , formState ∷ SlamDownFormState a
  }

-- | The state of a SlamDown form
newtype SlamDownState a = SlamDownState (SlamDownStateR a)

instance functorSlamDownState ∷ Functor SlamDownState where
  map f (SlamDownState st) =
    SlamDownState
      { document: f <$> st.document
      , formState: map f <$> st.formState
      }

getDocument ∷ Natural SlamDownState SD.SlamDownP
getDocument (SlamDownState rec) = rec.document

getFormState ∷ Natural SlamDownState SlamDownFormState
getFormState (SlamDownState rec) = rec.formState

modifyFormState
  ∷ ∀ a
  . (SlamDownFormState a → SlamDownFormState a)
  → SlamDownState a
  → SlamDownState a
modifyFormState f (SlamDownState rec) =
  SlamDownState (rec { formState = f rec.formState })

instance showSlamDownState ∷ (Show a) ⇒ Show (SlamDownState a) where
  show (SlamDownState rec) = "(SlamDownState " ++ show rec.formState ++ ")"

instance arbitrarySlamDownState ∷ (SC.Arbitrary a, Ord a) ⇒ SC.Arbitrary (SlamDownState a) where
  arbitrary = do
    document ← SC.arbitrary
    formState ← SM.fromList <$> SC.arbitrary
    pure $ SlamDownState
      { document : document
      , formState : formState
      }

-- | Gets the form field value, or the default if none is present.
getFormFieldValue
  ∷ ∀ v
  . String
  → SlamDownState v
  → M.Maybe (FormFieldValue v)
getFormFieldValue key state =
  case SM.lookup key $ getFormState state of
    M.Just x → M.Just x
    M.Nothing → SM.lookup key <<< formStateFromDocument $ getDocument state

formStateFromDocument ∷ Natural SD.SlamDownP SlamDownFormState
formStateFromDocument =
  SM.fromList
    <<< SDT.everything (const mempty) phi
  where
    phi
      ∷ ∀ v
      . SD.Inline v
      → L.List (Tuple String (FormFieldValue v))
    phi (SD.FormField label _ field) =
      M.maybe mempty (L.singleton <<< Tuple label) $
        V.runV (const M.Nothing) M.Just (SDPI.validateFormField field)
          >>= formFieldGetDefaultValue
    phi _ = mempty

formFieldGetDefaultValue
  ∷ ∀ v
  . SD.FormField v
  → M.Maybe (FormFieldValue v)
formFieldGetDefaultValue =
  SD.traverseFormField (SD.getLiteral >>> map pure)

-- | The initial empty state of the form, with an empty document.
emptySlamDownState ∷ ∀ v. SlamDownState v
emptySlamDownState =
  SlamDownState
    { document : SD.SlamDown mempty
    , formState : SM.empty
    }

-- | The initial state of the form based on a document value. All fields use
-- | their default values.
makeSlamDownState ∷ Natural SD.SlamDownP SlamDownState
makeSlamDownState doc =
  SlamDownState
    { document : doc
    , formState : formStateFromDocument doc
    }


formDescFromDocument ∷ Natural SD.SlamDownP SlamDownFormDesc
formDescFromDocument =
  SM.fromList
    <<< SDT.everything (const mempty) phi
  where
    phi ∷ ∀ v. SD.Inline v → L.List (Tuple String (SD.FormField v))
    phi (SD.FormField label _ field) = L.singleton (Tuple label field)
    phi _ = mempty

replaceDocument
  ∷ ∀ v
  . (SD.Value v)
  ⇒ SD.SlamDownP v
  → SlamDownState v
  → SlamDownState v
replaceDocument doc (SlamDownState state) =
  SlamDownState
    { document : doc
    , formState : prunedFormState state.formState
    }

  where
    formDesc ∷ SlamDownFormDesc v
    formDesc = formDescFromDocument doc

    eraseTextBox ∷ ∀ f. SD.TextBox f → SD.TextBox (Const Unit)
    eraseTextBox = SD.transTextBox \_ → Const unit

    -- | Returns the keys that are either not present in the new state, or have had their types changed.
    keysToPrune ∷ SlamDownFormState v → Array String
    keysToPrune =
      SM.foldMap \(key ∷ String) (oldVal ∷ FormFieldValue v) →
        case Tuple oldVal (SM.lookup key formDesc) of
          Tuple (SD.TextBox tb1) (M.Just (SD.TextBox tb2)) →
            if eraseTextBox tb1 == eraseTextBox tb2
            then []
            else [ key ]
          Tuple (SD.CheckBoxes _ (Id.Identity xs1)) (M.Just (SD.CheckBoxes _ (SD.Literal xs2))) →
            if xs1 == xs2
            then []
            else [ key ]
          Tuple (SD.DropDown _ (Id.Identity xs1)) (M.Just (SD.DropDown _ (SD.Literal xs2))) →
            if xs1 == xs2
            then []
            else [ key ]
          Tuple (SD.RadioButtons _ (Id.Identity xs1)) (M.Just (SD.RadioButtons _ (SD.Literal xs2))) →
            if xs1 == xs2
            then []
            else [ key ]
          Tuple _ M.Nothing →
            [ key ]
          _ → []

    prunedFormState ∷ SlamDownFormState v → SlamDownFormState v
    prunedFormState st = F.foldr SM.delete st $ keysToPrune st

