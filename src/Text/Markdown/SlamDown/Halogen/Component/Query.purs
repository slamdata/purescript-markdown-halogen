module Text.Markdown.SlamDown.Halogen.Component.Query
  ( SlamDownQuery(..)
  ) where

import Prelude

import Data.Maybe as M
import Data.NaturalTransformation (Natural)
import Data.StrMap as SM
import Test.StrongCheck as SC
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

getArbStrMap ∷ Natural ArbStrMap SM.StrMap
getArbStrMap (ArbStrMap a) = a

instance arbitraryArbStrMap ∷ (SC.Arbitrary a) ⇒ SC.Arbitrary (ArbStrMap a) where
  arbitrary =
    ArbStrMap <<< SM.fromList <$> SC.arbitrary

instance coarbitraryArbStrMap ∷ (SC.CoArbitrary a) ⇒ SC.CoArbitrary (ArbStrMap a) where
  coarbitrary (ArbStrMap a) =
    SC.coarbitrary (SM.toList a)

instance arbitrarySlamDownQuery ∷ (SC.Arbitrary v, SC.CoArbitrary v, SC.Arbitrary a, Eq v) ⇒ SC.Arbitrary (SlamDownQuery v a) where
  arbitrary = do
    i ← Gen.chooseInt 0.0 6.0
    case i of
      0 -> TextBoxChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      1 -> CheckBoxChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      2 -> RadioButtonChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      3 -> DropDownChanged <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      4 -> SetDocument <$> SC.arbitrary <*> SC.arbitrary
      5 -> GetFormState <<< (_ <<< ArbStrMap) <$> SC.arbitrary
      _ -> PopulateForm <<< getArbStrMap <$> SC.arbitrary <*> SC.arbitrary

