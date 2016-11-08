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
  = TextBoxChanged String (SD.TextBox M.Maybe) a
  | CheckBoxChanged String v Boolean a
  | RadioButtonChanged String v a
  | DropDownChanged String (M.Maybe v) a
  | SetDocument (SD.SlamDownP v) a
  | GetFormState (SDS.SlamDownFormState v → a)
  | PopulateForm (SDS.SlamDownFormState v) a

instance functorSlamDownQuery ∷ Functor (SlamDownQuery v) where
  map f e =
    case e of
      TextBoxChanged k tb a → TextBoxChanged k tb $ f a
      CheckBoxChanged k v b a → CheckBoxChanged k v b $ f a
      RadioButtonChanged k v a → RadioButtonChanged k v $ f a
      DropDownChanged k v a → DropDownChanged k v $ f a
      SetDocument doc a → SetDocument doc $ f a
      GetFormState k → GetFormState (f <<< k)
      PopulateForm s a → PopulateForm s $ f a

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
      0 -> TextBoxChanged <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      1 -> CheckBoxChanged <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      2 -> RadioButtonChanged <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      3 -> DropDownChanged <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      4 -> SetDocument <$> SCA.arbitrary <*> SCA.arbitrary
      5 -> GetFormState <<< (_ <<< ArbStrMap) <$> SCA.arbitrary
      _ -> PopulateForm <<< getArbStrMap <$> SCA.arbitrary <*> SCA.arbitrary
