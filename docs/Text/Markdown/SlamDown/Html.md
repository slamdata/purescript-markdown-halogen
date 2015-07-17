## Module Text.Markdown.SlamDown.Html

This module defines functions for rendering Markdown to HTML.

#### `FormFieldValue`

``` purescript
data FormFieldValue
  = SingleValue TextBoxType String
  | MultipleValues (Set String)
```

##### Instances
``` purescript
instance showFormFieldValue :: Show FormFieldValue
```

#### `SlamDownState`

``` purescript
newtype SlamDownState
  = SlamDownState (StrMap FormFieldValue)
```

The state of a SlamDown form - a mapping from input keys to values

##### Instances
``` purescript
instance showSlamDownState :: Show SlamDownState
```

#### `initSlamDownState`

``` purescript
initSlamDownState :: SlamDown -> SlamDownState
```

The initial state of form, in which all fields use their default values

#### `SlamDownEvent`

``` purescript
data SlamDownEvent
```

The type of events which can be raised by SlamDown forms

#### `applySlamDownEvent`

``` purescript
applySlamDownEvent :: SlamDownState -> SlamDownEvent -> SlamDownState
```

Apply a `SlamDownEvent` to a `SlamDownState`.

#### `renderHalogen`

``` purescript
renderHalogen :: forall f. (Alternative f) => String -> SlamDownState -> SlamDown -> Array (HTML (f SlamDownEvent))
```

Render the SlamDown AST to an arbitrary Halogen HTML representation


