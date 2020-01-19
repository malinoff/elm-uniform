module Uniform.Classic exposing
    ( field, FieldConfig, textField
    , viewField, ViewConfig
    )

{-| This is a tiny wrapper around `Uniform.field` and `Uniform.viewField` that makes all fields editable by default,
and does not require you to provide `valueState` in your `values`.


# Fields

@docs field, FieldConfig, textField


# View

@docs viewField, ViewConfig

-}

import Uniform exposing (Field, FieldState(..), Form, FormField)


{-| Define a "classic", always-editable form field.

First argument is a function telling if the given value is empty.
Useful for custom field types, but if you find yourself working mostly with text fields, take a look at [`textField`](#textField).

-}
field :
    (value -> Bool)
    -> FieldConfig value output values
    -> FormField value output values
field isEmpty config =
    Uniform.field
        isEmpty
        { parser = config.parser
        , value = config.value
        , updateValue = config.update
        , state = always Editing
        , updateState = always
        }


{-| Describe how to get, set, parse the field's value.

  - `parser` specifies how to validate the value.
    It needs a function that processes the value of the field and produces a `Result` of either:
      - a `String` describing an error,
      - a correct `output`.
  - `value` specifies how to get the value from `values`. Most of the time it will be in a form of `.fieldName`.
  - `update` specified how to update `values` with a new value.
    Unfortunately, elm does not provide a special syntax for functions-setters like for getters, that's why
    most of the time it will look like `\values value -> { values | fieldName = value }`

-}
type alias FieldConfig value output values =
    { parser : value -> Result String output
    , value : values -> value
    , update : values -> value -> values
    }


{-| Define a "classic" text field. Not mandatory to use, but can help to shorten your fields definition a bit.

All it does is calling `field String.isEmpty`.

It is intentional that there are no other similar helpers in the library.
I encourage you to implement similar helper functions for your custom value types.

-}
textField : FieldConfig String output values -> FormField String output values
textField =
    field String.isEmpty


{-| A type-safe way to view a single field from the filled form's `fields`.

    viewNameField =
        Uniform.viewField viewName

    viewAgeField =
        Uniform.viewField viewAge

    viewForm model =
        let
            filled =
                Uniform.fill userForm model.formValues
        in
        Html.div
            []
            [ viewNameField
            , viewAgeField
            ]

-}
viewField : ViewConfig value values element -> Field value output values -> element
viewField viewFn field_ =
    viewFn
        { value = field_.value
        , update = field_.updateValue
        }


{-| Describe how to view a form's field.

It is a function that accepts the `value` to display and the `update` function that should be called in `onInput` handler like this:

        viewName { value, update } =
            Html.input
                [ Html.Events.onInput (FormUpdated << update) ]
                , Html.Attributes.value value
                ]
                []

-}
type alias ViewConfig value values element =
    { value : value, update : value -> values } -> element
