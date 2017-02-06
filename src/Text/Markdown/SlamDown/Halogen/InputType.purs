module Text.Markdown.SlamDown.Halogen.InputType where

import Data.BrowserFeatures (BrowserFeatures)
import Data.BrowserFeatures.InputType as IT

import Halogen.HTML.Properties as HP

import Text.Markdown.SlamDown as SD

availableInputType
  ∷ ∀ f
  . BrowserFeatures
  → SD.TextBox f
  → IT.InputType
availableInputType features tb =
  if features.inputTypeSupported inputType
   then inputType
   else IT.Text
  where
    inputType ∷ IT.InputType
    inputType = textBoxToInputType tb

textBoxToInputType
  ∷ ∀ f
  . SD.TextBox f
  → IT.InputType
textBoxToInputType ty =
  case ty of
    SD.PlainText _ → IT.Text
    SD.Date _ → IT.Date
    SD.Time _ _ → IT.Time
    SD.DateTime _ _ → IT.DateTimeLocal
    SD.Numeric _ → IT.Number

inputTypeToHalogenInputType
  ∷ IT.InputType
  → HP.InputType
inputTypeToHalogenInputType it =
  case it of
    IT.Color → HP.InputColor
    IT.Date → HP.InputDate
    IT.DateTime → HP.InputDatetime
    IT.DateTimeLocal → HP.InputDatetimeLocal
    IT.Time → HP.InputTime
    IT.Month → HP.InputMonth
    IT.Week → HP.InputWeek
    IT.Email → HP.InputEmail
    IT.Url → HP.InputUrl
    IT.Number → HP.InputNumber
    IT.Search → HP.InputSearch
    IT.Range → HP.InputRange
    IT.Text → HP.InputText
