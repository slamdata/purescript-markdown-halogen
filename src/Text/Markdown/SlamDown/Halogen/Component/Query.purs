module Text.Markdown.SlamDown.Halogen.Component.Query
  ( SlamDownQuery(..)
  ) where

import Prelude

import Data.List as L
import Data.Maybe as M
import Data.StrMap as SM
import Data.Tuple as T

import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDS

data SlamDownQuery v a
  = ChangeTextBox String (SD.TextBox M.Maybe) a
  | ChangeCheckBox String v Boolean a
  | ChangeRadioButton String v a
  | ChangeDropDown String (M.Maybe v) a
  | SetDocument (SD.SlamDownP v) a
  | GetFormState (SDS.SlamDownFormState v → a)
  | PopulateForm (SDS.SlamDownFormState v) a

derive instance functorSlamDownQuery ∷ Functor (SlamDownQuery v)

newtype ArbStrMap a = ArbStrMap (SM.StrMap a)

getArbStrMap ∷ ArbStrMap ~> SM.StrMap
getArbStrMap (ArbStrMap a) = a

instance arbitraryArbStrMap ∷ (SCA.Arbitrary a) ⇒ SCA.Arbitrary (ArbStrMap a) where
  arbitrary =
    ArbStrMap <<< SM.fromFoldable <$> SCA.arbitrary :: Gen.Gen (L.List (T.Tuple String a))

instance coarbitraryArbStrMap ∷ (SCA.Coarbitrary a) ⇒ SCA.Coarbitrary (ArbStrMap a) where
  coarbitrary (ArbStrMap a) =
    SCA.coarbitrary (SM.toList a)

instance arbitrarySlamDownQuery ∷ (SCA.Arbitrary v, SCA.Coarbitrary v, SCA.Arbitrary a, Eq v) ⇒ SCA.Arbitrary (SlamDownQuery v a) where
  arbitrary = do
    i ← Gen.chooseInt 0 6
    case i of
      0 -> ChangeTextBox <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      1 -> ChangeCheckBox <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      2 -> ChangeRadioButton <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      3 -> ChangeDropDown <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      4 -> SetDocument <$> SCA.arbitrary <*> SCA.arbitrary
      5 -> GetFormState <<< (_ <<< ArbStrMap) <$> SCA.arbitrary
      _ -> PopulateForm <<< getArbStrMap <$> SCA.arbitrary <*> SCA.arbitrary
