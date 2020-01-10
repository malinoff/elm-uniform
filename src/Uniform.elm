module Uniform exposing
    ( Error(..)
    , Field
    , FieldState(..)
    , FilledForm
    , Form
    , append
    , dependent
    , disabled
    , errors
    , field
    , fill
    , isEmpty
    , optional
    , succeed
    )

import Maybe.Extra



-- FORMS


type Form field msg values
    = Form (values -> FilledForm field msg)


succeed : msg -> Form field msg values
succeed msg =
    Form (always { fields = [], submit = Just msg })



-- FIELDS


type alias FieldConfig value parsed values =
    { parser : value -> Result String parsed
    , value : values -> value
    , update : values -> value -> values
    }


field :
    { isEmpty : value -> Bool, type_ : Field value values -> field }
    -> FieldConfig value parsed values
    -> Form field parsed values
field spec config =
    let
        parse values =
            if spec.isEmpty (config.value values) then
                Err RequiredFieldIsEmpty

            else
                config.parser (config.value values)
                    |> Result.mapError (\msg -> ValidationFailed msg)

        filledField : values -> Maybe Error -> FilledField field
        filledField values error =
            { field =
                spec.type_
                    { value = config.value values
                    , update = config.update values
                    }
            , error = error
            , state = CanBeEdited
            }
    in
    Form <|
        \values ->
            let
                ( parsed, error ) =
                    case parse values of
                        Ok msg_ ->
                            ( Just msg_, Nothing )

                        Err error_ ->
                            ( Nothing, Just error_ )
            in
            { fields = [ filledField values error ], submit = parsed }



-- COMPOSITION


append : Form field msg1 values -> Form field (msg1 -> msg2) values -> Form field msg2 values
append new current =
    Form <|
        \values ->
            let
                filledCurrent =
                    fill current values

                filledNew =
                    fill new values

                fields =
                    filledCurrent.fields ++ filledNew.fields

                submit =
                    Maybe.Extra.andMap filledNew.submit filledCurrent.submit
            in
            { fields = fields, submit = submit }


optional : Form field msg values -> Form field msg values
optional form =
    Form <|
        \values ->
            let
                filled =
                    fill form values

                cleanErrors =
                    List.map (\field_ -> { field_ | error = Nothing })
            in
            if isEmpty filled then
                { filled | fields = cleanErrors filled.fields }

            else
                filled


disabled : Form field msg values -> Form field msg values
disabled form =
    Form <|
        \values ->
            let
                filled =
                    fill form values

                disable_ =
                    List.map (\field_ -> { field_ | state = Disabled })
            in
            { filled | fields = disable_ filled.fields }


dependent : (values -> Form field msg values) -> Form field msg values
dependent fn =
    Form (\values -> fill (fn values) values)



-- OUTPUT


type alias FilledForm field msg =
    { fields : List (FilledField field)
    , submit : Maybe msg
    }


type alias Field value values =
    { value : value
    , update : value -> values
    }


type alias FilledField field =
    { field : field
    , error : Maybe Error
    , state : FieldState
    }


type FieldState
    = CanBeEdited
    | CanBeViewed
    | Disabled


type Error
    = RequiredFieldIsEmpty
    | ValidationFailed String


fill : Form field msg values -> values -> FilledForm field msg
fill (Form filled) =
    filled


errors : FilledForm msg fields -> List Error
errors form =
    form.fields
        |> List.map .error
        |> Maybe.Extra.values


isEmpty : FilledForm field msg -> Bool
isEmpty form =
    List.all (\f -> f.error == Just RequiredFieldIsEmpty) form.fields
