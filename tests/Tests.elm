module Tests exposing (..)

import Expect
import Test exposing (..)
import Uniform
import Uniform.Classic



-- This example is taken from https://korban.net/posts/elm/2018-11-27-build-complex-forms-validation-elm/


type alias UserDetails =
    { name : Maybe String
    , email : Email
    , password : Password
    , plan : Plan
    }


type Email
    = Email String


type Password
    = Password String


type Plan
    = Basic
    | Pro
    | Enterprise


type alias SignupFormValues =
    { name : String
    , email : String
    , password : String
    , repeatedPassword : String
    , plan : String
    , agreedToTerms : Bool
    }


type alias SignupFormFields =
    { name : Uniform.Field String (Maybe String) SignupFormValues
    , email : Uniform.TextField Email SignupFormValues
    , password : Uniform.TextField Password SignupFormValues
    , repeatedPassword : Uniform.TextField () SignupFormValues
    , plan : Uniform.TextField Plan SignupFormValues
    , agreedToTerms : Uniform.Field Bool () SignupFormValues
    }


signupForm : Uniform.Form SignupFormValues SignupFormFields UserDetails
signupForm =
    let
        nameField : Uniform.FormField String String SignupFormValues
        nameField =
            Uniform.Classic.field
                String.isEmpty
                { parser = Ok
                , value = .name
                , update = \values value -> { values | name = value }
                }

        emailField : Uniform.FormField String Email SignupFormValues
        emailField =
            Uniform.Classic.textField
                { parser =
                    \value ->
                        if String.contains "@" value then
                            Ok <| Email value

                        else
                            Err "Invalid email"
                , value = .email
                , update = \values value -> { values | email = value }
                }

        passwordField : Uniform.FormField String Password SignupFormValues
        passwordField =
            Uniform.Classic.textField
                { parser =
                    \value ->
                        if String.length value >= 6 then
                            Ok <| Password value

                        else
                            Err "Invalid password"
                , value = .password
                , update = \values value -> { values | password = value }
                }

        repeatedPasswordField : Uniform.FormField String () SignupFormValues
        repeatedPasswordField =
            Uniform.dynamicField <|
                \otherValues ->
                    Uniform.Classic.textField
                        { parser =
                            \value ->
                                if value == otherValues.password then
                                    Ok ()

                                else
                                    Err "The passwords should match"
                        , value = .repeatedPassword
                        , update = \values value -> { values | repeatedPassword = value }
                        }

        planField =
            Uniform.Classic.textField
                { parser =
                    \value ->
                        case value of
                            "Basic" ->
                                Ok Basic

                            "Pro" ->
                                Ok Pro

                            "Enterprise" ->
                                Ok Enterprise

                            _ ->
                                Err "Wrong plan"
                , value = .plan
                , update = \values value -> { values | plan = value }
                }

        agreedToTermsField =
            Uniform.Classic.field
                (always False)
                { parser =
                    \value ->
                        if value then
                            Ok ()

                        else
                            Err "You must accept the terms"
                , value = .agreedToTerms
                , update = \values value -> { values | agreedToTerms = value }
                }
    in
    (\name email password _ plan _ ->
        UserDetails name email password plan
    )
        |> Uniform.succeed SignupFormFields
        |> Uniform.append (Uniform.optional nameField)
        |> Uniform.append emailField
        |> Uniform.append passwordField
        |> Uniform.append repeatedPasswordField
        |> Uniform.append planField
        |> Uniform.append agreedToTermsField


goodSignupValues =
    { name = "John Doe"
    , email = "john@doe.com"
    , password = "VerySecure"
    , repeatedPassword = "VerySecure"
    , plan = "Basic"
    , agreedToTerms = True
    }


badSignupValues =
    { name = ""
    , email = "noway"
    , password = ""
    , repeatedPassword = "VerySecure"
    , plan = "SuperDuper"
    , agreedToTerms = False
    }


classicForm : Test
classicForm =
    describe "Typical \"classic\" signup form with static fields" <|
        [ describe "when filled with good values" <|
            [ test "produces the good output" <|
                \_ ->
                    let
                        expected : UserDetails
                        expected =
                            { name = Just "John Doe"
                            , email = Email "john@doe.com"
                            , password = Password "VerySecure"
                            , plan = Basic
                            }
                    in
                    Uniform.fill signupForm goodSignupValues
                        |> .output
                        |> Expect.equal (Just expected)
            ]
        , describe "when filled with bad values" <|
            [ test "produces nothing" <|
                \_ ->
                    Uniform.fill signupForm badSignupValues
                        |> .output
                        |> Expect.equal Nothing
            , test "fields contain the expected validation errors" <|
                \_ ->
                    Uniform.fill signupForm badSignupValues
                        |> .fields
                        |> (\f ->
                                { name = f.name.output
                                , email = f.email.output
                                , password = f.password.output
                                , repeatedPassword = f.repeatedPassword.output
                                , plan = f.plan.output
                                , agreedToTerms = f.agreedToTerms.output
                                }
                           )
                        |> Expect.equal
                            { name = Ok Nothing
                            , email = Err (Uniform.ValidationFailed "Invalid email")
                            , password = Err Uniform.RequiredFieldIsEmpty
                            , repeatedPassword = Err (Uniform.ValidationFailed "The passwords should match")
                            , plan = Err (Uniform.ValidationFailed "Wrong plan")
                            , agreedToTerms = Err (Uniform.ValidationFailed "You must accept the terms")
                            }
            ]
        ]
