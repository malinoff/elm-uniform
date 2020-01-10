module Tests exposing (..)

import Expect
import Test exposing (..)
import Uniform


type alias Form =
    { input : String }


type Field
    = Text (Uniform.Field String Form)


textField =
    Uniform.field { isEmpty = String.isEmpty, type_ = Text }


classic : Test
classic =
    describe "classic"
        [ test "returns an empty form that always produces the given output" <|
            \_ ->
                let
                    form =
                        Uniform.succeed ()
                in
                Uniform.fill form ()
                    |> Expect.equal
                        { fields = []
                        , submit = Just ()
                        }
        ]


text : Test
text =
    let
        form =
            textField
                { parser =
                    \value ->
                        if value == "bad input" then
                            Err "Bad input!"

                        else
                            Ok value
                , value = .input
                , update = \values value -> { values | input = value }
                }
    in
    describe "text"
        [ describe "when filled with a good value"
            [ test "produces the given value" <|
                \_ ->
                    Uniform.fill form { input = "input" }
                        |> .submit
                        |> Expect.equal (Just "input")
            , test "contains no errors" <|
                \_ ->
                    Uniform.fill form { input = "input" }
                        |> Uniform.errors
                        |> List.length
                        |> Expect.equal 0
            , test "is not empty" <|
                \_ ->
                    Uniform.fill form { input = "input" }
                        |> Uniform.isEmpty
                        |> Expect.equal False
            ]
        , describe "when filled with a bad value"
            [ test "does not produce a value" <|
                \_ ->
                    Uniform.fill form { input = "bad input" }
                        |> .submit
                        |> Expect.equal Nothing
            , test "contains the validation error" <|
                \_ ->
                    Uniform.fill form { input = "bad input" }
                        |> Uniform.errors
                        |> (\errors ->
                                case errors of
                                    [ error ] ->
                                        Expect.equal (Uniform.ValidationFailed "Bad input!") error

                                    _ ->
                                        Expect.fail "does not contain an error"
                           )
            , test "is not empty" <|
                \_ ->
                    Uniform.fill form { input = "bad input" }
                        |> Uniform.isEmpty
                        |> Expect.equal False
            ]
        , describe "when filled with an empty value"
            [ test "does not produce a value" <|
                \_ ->
                    Uniform.fill form { input = "" }
                        |> .submit
                        |> Expect.equal Nothing
            , test "contains the required field is missing error" <|
                \_ ->
                    Uniform.fill form { input = "" }
                        |> Uniform.errors
                        |> (\errors ->
                                case errors of
                                    [ error ] ->
                                        Expect.equal Uniform.RequiredFieldIsEmpty error

                                    _ ->
                                        Expect.fail "does not contain an error"
                           )
            , test "is empty" <|
                \_ ->
                    Uniform.fill form { input = "" }
                        |> Uniform.isEmpty
                        |> Expect.equal True
            ]
        ]
