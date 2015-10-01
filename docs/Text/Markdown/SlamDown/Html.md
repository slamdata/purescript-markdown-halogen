## Module Text.Markdown.SlamDown.Html

This module defines a component for rendering SlamDown documents to HTML

#### `FormFieldValue`

``` purescript
data FormFieldValue
  = SingleValue TextBoxType String
  | MultipleValues (Set String)
```

##### Instances
``` purescript
instance arbitraryFormFieldValue :: Arbitrary FormFieldValue
instance showFormFieldValue :: Show FormFieldValue
```

#### `SlamDownFormState`

``` purescript
type SlamDownFormState = StrMap FormFieldValue
```

#### `SlamDownStateR`

``` purescript
type SlamDownStateR = { document :: SlamDown, formState :: SlamDownFormState }
```

#### `SlamDownState`

``` purescript
newtype SlamDownState
  = SlamDownState SlamDownStateR
```

The state of a SlamDown form

##### Instances
``` purescript
instance showSlamDownState :: Show SlamDownState
instance arbitrarySlamDownState :: Arbitrary SlamDownState
```

#### `defaultBrowserFeatures`

``` purescript
defaultBrowserFeatures :: BrowserFeatures
```

By default, all features are enabled.

#### `emptySlamDownState`

``` purescript
emptySlamDownState :: SlamDownState
```

#### `makeSlamDownState`

``` purescript
makeSlamDownState :: SlamDown -> SlamDownState
```

The initial state of form, in which all fields use their default values

#### `SlamDownQuery`

``` purescript
data SlamDownQuery a
  = TextChanged TextBoxType String String a
  | CheckBoxChanged String String Boolean a
  | SetDocument SlamDown a
  | GetFormState (SlamDownFormState -> a)
```

##### Instances
``` purescript
instance functorSlamDownQuery :: Functor SlamDownQuery
instance arbitrarySlamDownQuery :: (Arbitrary a) => Arbitrary (SlamDownQuery a)
```

#### `evalSlamDownQuery`

``` purescript
evalSlamDownQuery :: forall g. Eval SlamDownQuery SlamDownState SlamDownQuery g
```

#### `SlamDownConfig`

``` purescript
type SlamDownConfig = { browserFeatures :: BrowserFeatures, formName :: String }
```

#### `renderSlamDown`

``` purescript
renderSlamDown :: SlamDownConfig -> Render SlamDownState SlamDownQuery
```

Render a `SlamDown` document into an HTML form.

#### `slamDownComponent`

``` purescript
slamDownComponent :: forall g. SlamDownConfig -> Component SlamDownState SlamDownQuery g
```

Bundles up the SlamDown renderer and state machine into a Halogen component.


