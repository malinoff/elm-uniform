module Uniform exposing
    ( Form, succeed
    , FormField, field, FieldConfig, append, TextField, textField, optional, dynamicField
    , fill, Field, FieldState(..), Error(..), mapOutput
    , viewField, ViewConfig
    )

{-| Build in-place editable [forms](#Form) comprised of [fields](#Field).

An in-place editable form is a form representing an existing entity where each individual field is in "Displaying" state
initially, but can be switched to "Editing" state by clicking on it.
The updated entity is usually being validated and returned `onBlur`.

If you're looking for a classic form which can represent a non-existing entity (like a sign up form) and which is
comprised of editable fields, take a look at `Uniform.Classic` module.


# Form definition

@docs Form, succeed


# Fields

@docs FormField, field, FieldConfig, append, TextField, textField, optional, dynamicField


# Outputs

@docs fill, Field, FieldState, Error, mapOutput


# View

The form definition is completely decoupled from its view.
You are responsible for implementing views for your form; as there are no two identical forms,
I don't even try to provide a "universal, configurable" solution to this problem.

However, I _can_ provide some helpers so that it's **easy** to display the form's fields **properly**.
If these helpers don't work for you, opt-out and inspect the filled form's [`fields`](#Field) directly.

@docs viewField, ViewConfig

-}


{-| Unsurprisingly, a `Form` is a collection of `fields` which produces an `output` when filled with `values`.
Of course, this does nothing by itself. It needs to have some [`fields`](#append)!

    type alias User =
        { name : String
        , age : Int
        }

    type alias Values =
        { name : String
        , nameState : Uniform.FieldState
        , age : String
        , ageState : Uniform.FieldState
        }

    type alias Fields =
        { name : Uniform.Field String String Values
        , age : Uniform.Field String Int Values
        }

    type alias UserForm =
        Uniform.Form Values Fields User

-}
type Form values fields output
    = Form (values -> { fields : fields, output : Maybe output })


{-| Make a empty form with no fields that always successfully produces the given output.

Useful as a starting point to which you can [`append`](#append) fields.

-}
succeed : fields -> output -> Form values fields output
succeed fields output =
    Form (always { fields = fields, output = Just output })


{-| Represents a field filled with `values`.

  - `value` contains the "raw" value that can be displayed in a view,
  - `updateValue` is a function that is usually used in an `onInput` event,
  - `state` contains the current [`state`](#FieldState) of the field,
  - `updateState` is a function that is usually used in `onClick`/`onBlur` events,
  - `output` contains either a successfully parsed `output` or an [`error`](#Error).

-}
type alias Field value output values =
    { value : value
    , updateValue : value -> values
    , state : FieldState
    , updateState : values
    , output : Result Error output
    }


{-| A value that knows how to fill a specific field with an appropriate value from `values`.

You shouldn't care about this type at all, it's exported so that `Uniform.Classic.field` can be implemented.

-}
type FormField value output values
    = FormField (values -> Field value output values)


{-| Define an actual field. It is not tied to a specific form yet, go and [`append`](#append) it!

First argument is a function telling if the given value is empty.
Useful for custom field types, but if you find yourself working mostly with text fields, take a look at [`textField`](#textField).

Second argument is where you describe how to get and set the value, and how to parse it into the `output`.

    ageField =
        Uniform.field
            String.isEmpty
            { parser = Result.fromMaybe "Not an integer" String.toInt
            , value = .age
            , updateValue = \values age -> { values | age = age }
            , state = Uniform.Displaying
            , updateState = \values state -> { values | ageState = state }
            }

-}
field :
    (value -> Bool)
    -> FieldConfig value output values
    -> FormField value output values
field isEmpty config =
    FormField <|
        \values ->
            let
                parse value_ =
                    if isEmpty value_ then
                        Err RequiredFieldIsEmpty

                    else
                        Result.mapError ValidationFailed (config.parser value_)

                ( value, state ) =
                    config.value values

                flipState state_ =
                    case state_ of
                        Editing ->
                            Displaying

                        Displaying ->
                            Editing
            in
            { value = value
            , updateValue = \newValue -> config.update values ( newValue, state )
            , state = state
            , updateState = config.update values ( value, flipState state )
            , output = parse value
            }


{-| Describe how to get, set, parse the field's value and state.

  - `parser` specifies how to validate the value.
    It needs a function that processes the value of the field and produces a `Result` of either:
      - a `String` describing an error,
      - a correct `output`.
  - `value` specifies how to get the value and the state from `values`.
    Most of the time it will be in a form of `.fieldName`.
  - `update` specified how to update `values` with a new value and/or a new state.
    Most of the time it will look like `\values value -> { values | fieldName = value }`

-}
type alias FieldConfig value output values =
    { parser : value -> Result String output
    , value : values -> ( value, FieldState )
    , update : values -> ( value, FieldState ) -> values
    }


{-| Take a [`field`](#Field) and append it to the form.

    userForm =
        Uniform.succeed User Fields
            |> Uniform.append nameField
            |> Uniform.append ageField

-}
append :
    FormField value fieldOutput values
    -> Form values (Field value fieldOutput values -> fields) (fieldOutput -> output)
    -> Form values fields output
append field_ form =
    Form <|
        \values ->
            let
                filledField =
                    fillField field_ values

                filledForm =
                    fill form values
            in
            { fields = filledForm.fields filledField
            , output = Maybe.map2 (|>) (Result.toMaybe filledField.output) filledForm.output
            }


{-| Lots of fields, however, are just plain strings with custom validation.
This alias is not mandatory to use, but can help to shorten your fields definition a bit.

It is intentional that there are no other similar aliases in the library.
I encourage you to define your own aliases for your custom value types.

-}
type alias TextField output values =
    Field String output values


{-| Define a text field. Not mandatory to use, but can help to shorten your fields definition a bit.

All it does is calling `field String.isEmpty`.

It is intentional that there are no other similar helpers in the library.
I encourage you to implement similar helper functions for your custom value types.

-}
textField : FieldConfig String output values -> FormField String output values
textField =
    field String.isEmpty


{-| All fields are required by default, but you can make them `optional`.

    Uniform.succeed SomeOutput SomeFields
        |> Uniform.append field1
        |> Uniform.append (optional field2)

The corresponding field in your output model must be `Maybe output` instead of just `output`.

-}
optional : FormField value output values -> FormField value (Maybe output) values
optional field_ =
    FormField <|
        \values ->
            let
                filledField =
                    fillField field_ values

                output =
                    case filledField.output of
                        Ok output_ ->
                            Ok (Just output_)

                        Err RequiredFieldIsEmpty ->
                            Ok Nothing

                        Err error ->
                            Err error
            in
            { value = filledField.value
            , updateValue = filledField.updateValue
            , state = filledField.state
            , updateState = filledField.updateState
            , output = output
            }


{-| This function allows fields to peek into other fields values.

You can use it for plenty different use cases:

  - define different fields (with possibly different value types) depending on the value of an other field,
  - extend parser capabilities with parsing dependent values
  - extend parser capabilities with an external data (who said that `values` should _only_ represent form values?
    Nothing stops you from defining an extra field that you can inspect later).

```
fieldWithComplexValidation =
    Uniform.dynamicField
        \values ->
            Uniform.textField
                { parser = complexParser values
                , ...
                }
```

-}
dynamicField : (values -> FormField value output values) -> FormField value output values
dynamicField fn =
    FormField <|
        \values ->
            fillField (fn values) values


{-| Once you have your form defined and fields appended, you can now `fill` it with `values`.

If all values are properly parsed into fields, the returned `output` field will contain `Just output`, otherwise `Nothing`.
You can inspect individual fields from the returned `fields`, each one will have a type of [`Field`](#Field).

However, I would advise not to implement your `view` for the form by inspecting the returned `fields` directly.
It is error-prone: it is easy to forget to branch on the state, or to call wrong update functions.
[`viewField`](#viewField) function is a type-safe way to view fields, it should cover all your needs.

-}
fill : Form values fields output -> values -> { fields : fields, output : Maybe output }
fill (Form filled) =
    filled


{-| Each field in an in-place editable form can be either `Editing` or `Displaying`.
I hope these names are obvious :) If not, submit a PR!
-}
type FieldState
    = Editing
    | Displaying


{-| This represents validation errors that may happen during `fill`ing a form.
Again, naming should be obvious. If not, submit a PR!
-}
type Error
    = RequiredFieldIsEmpty
    | ValidationFailed String


{-| Transform the output of the form.

This function can help you to keep forms decoupled from specific view messages:

    Uniform.mapOutput SignUp userForm

-}
mapOutput : (a -> b) -> Form values fields a -> Form values fields b
mapOutput fn form =
    Form <|
        \values ->
            let
                filledForm =
                    fill form values
            in
            { fields = filledForm.fields
            , output = Maybe.map fn filledForm.output
            }


{-| Describe how to view a form's field in a proper state.

  - `viewWhenDisplaying` is a function that is called when the field is in `Displaying` state.
    Accepts the `value` to display and the `makeEditable` function that should be called in `onClick` handler like this:

        viewWhenDisplaying { value, makeEditable } =
            Html.div
                [ Html.Events.onClick (FormUpdated makeEditable) ]
                [ Html.text value ]

  - `viewWhenEditing` is a function that is called when the field is in `Editing` state.
    Accepts the `value` to display, the `updateValue` function that should be called in `onInput` handler,
    and the `finishEditing` function that should be called in `onBlur` handler like this:

        viewWhenEditing { value, updateValue, finishEditing } =
            Html.input
                [ Html.Events.onBlur (FormUpdated finishEditing)
                , Html.Events.onInput (FormUpdated << updateValue)
                , Html.Attributes.value value
                , Html.Attributes.type\_ "text"
                []

-}
type alias ViewConfig value values element =
    { viewWhenDisplaying :
        { value : value
        , makeEditable : values
        }
        -> element
    , viewWhenEditing :
        { value : value
        , updateValue : value -> values
        , finishEditing : values
        }
        -> element
    }


{-| A type-safe way to view a single field from the filled form's `fields`.

    viewNameField =
        Uniform.viewField
            { viewWhenDisplaying = ...
            , viewWhenEditing = ...
            }

    viewAgeField =
        Uniform.viewField
            { viewWhenDisplaying = ...
            , viewWhenEditing = ...
            }

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
viewField config field_ =
    case field_.state of
        Displaying ->
            config.viewWhenDisplaying
                { value = field_.value
                , makeEditable = field_.updateState
                }

        Editing ->
            config.viewWhenEditing
                { value = field_.value
                , updateValue = field_.updateValue
                , finishEditing = field_.updateState
                }



-- Private helpers


fillField : FormField value output values -> values -> Field value output values
fillField (FormField filled) =
    filled
