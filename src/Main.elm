module Main exposing (..)

import Browser
import Browser.Events
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Random


type alias Model =
    { currentGuess : List Char
    , pastGuesses : List (List Char)
    , wordToGuess : List Char
    , error : Maybe String
    }


type CharState
    = Muted
    | Warning
    | Success


charStateToClass : CharState -> String
charStateToClass charState =
    case charState of
        Muted ->
            "bg-muted"

        Warning ->
            "bg-warning"

        Success ->
            "bg-success"


init : a -> ( Model, Cmd Msg )
init _ =
    ( { currentGuess = []
      , pastGuesses = []
      , wordToGuess = []
      , error = Nothing
      }
    , Random.generate NewRandom (Random.float 0 1)
    )


boardSize : { height : Int, width : Int }
boardSize =
    { height = 5, width = 5 }


type Msg
    = KeyDown Int
    | GotWords Float (Result Http.Error (List String))
    | NewRandom Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom float ->
            ( model, getWords float )

        GotWords randomFloat result ->
            case result of
                Ok words ->
                    ( { model
                        | wordToGuess =
                            words
                                |> List.drop (round (randomFloat * toFloat (List.length words)))
                                |> List.head
                                |> Maybe.withDefault "ERROR"
                                |> String.toUpper
                                |> String.toList
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | wordToGuess = String.toList "ERROR"
                        , error = Just "Error while loading the secret word. I replaced it with something simpler."
                      }
                    , Cmd.none
                    )

        KeyDown keyCode ->
            if keyCode >= 65 && keyCode <= 90 && List.length model.currentGuess < boardSize.width then
                --
                -- Add one character, unless the game is already won
                --
                ( { model | currentGuess = model.currentGuess ++ [ Char.fromCode keyCode ] }, Cmd.none )

            else if keyCode == 8 then
                --
                -- Delete - Remove one character
                --
                ( { model | currentGuess = List.take (List.length model.currentGuess - 1) model.currentGuess }, Cmd.none )

            else if keyCode == 13 then
                --
                -- Submit a currentGuess
                --
                if List.length model.currentGuess >= boardSize.width && List.length model.pastGuesses <= boardSize.height then
                    ( { model
                        | currentGuess = []
                        , pastGuesses = model.pastGuesses ++ [ model.currentGuess ]
                      }
                    , Cmd.none
                    )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )


isGameWon : { b | pastGuesses : List a, wordToGuess : a } -> Bool
isGameWon model =
    case List.head <| List.reverse model.pastGuesses of
        Just lastGuess ->
            lastGuess == model.wordToGuess

        Nothing ->
            False


isGameOver : { b | pastGuesses : List a, wordToGuess : a } -> Bool
isGameOver model =
    if List.length model.pastGuesses > boardSize.height then
        True

    else
        isGameWon model


calculateCharsState : List Char -> List Char -> List CharState
calculateCharsState wordToGuess guess =
    let
        addSuccesses : List Char -> List Char -> ( List Char, List CharState )
        addSuccesses wordToGuess_ guessAttempt =
            List.foldr
                (\( charToGuess, charGuessed ) ( accWordToGuess, acc ) ->
                    if charToGuess == charGuessed then
                        -- Removing the character to avoid future matches
                        ( ' ' :: accWordToGuess, Success :: acc )

                    else
                        ( charToGuess :: accWordToGuess, Muted :: acc )
                )
                ( [], [] )
                (zip wordToGuess_ guessAttempt)

        calculateWarning : List Char -> Char -> ( List Char, Bool )
        calculateWarning wordToGuess_ charGuessAttempt =
            List.foldr
                (\charToGuess ( accWordToGuess_2, metchedAlready ) ->
                    if metchedAlready then
                        -- Short circuiting
                        ( charToGuess :: accWordToGuess_2, metchedAlready )

                    else if charToGuess == charGuessAttempt then
                        -- Removing the character to avoid future matches
                        ( ' ' :: accWordToGuess_2, True )

                    else
                        ( charToGuess :: accWordToGuess_2, False )
                )
                ( [], False )
                wordToGuess_

        ( newWordToGuess, outcome ) =
            addSuccesses wordToGuess guess
    in
    List.foldl
        (\( charGuessAttempt, charState ) ( accWordToGuess, acc ) ->
            if charState == Success then
                -- This is already been taken care
                ( accWordToGuess, Success :: acc )

            else
                let
                    ( accWordToGuess_3, isWarning ) =
                        calculateWarning accWordToGuess charGuessAttempt
                in
                if isWarning then
                    ( accWordToGuess_3, Warning :: acc )

                else
                    ( accWordToGuess_3, Muted :: acc )
        )
        ( newWordToGuess, [] )
        (zip guess outcome)
        |> Tuple.second
        |> List.reverse


view : Model -> Html Msg
view model =
    let
        pastGuessesCharState : List (List CharState)
        pastGuessesCharState =
            List.map
                (\pastGuessAttempts ->
                    calculateCharsState model.wordToGuess pastGuessAttempts
                )
                model.pastGuesses

        keyboardCharState : Dict.Dict Char CharState
        keyboardCharState =
            List.foldl
                (\( guessChar, guessCharState ) acc ->
                    Dict.update
                        guessChar
                        (\maybeValue ->
                            case maybeValue of
                                Just value ->
                                    case value of
                                        Muted ->
                                            -- Everything is an upgrade
                                            Just guessCharState

                                        Warning ->
                                            if guessCharState == Success then
                                                -- Only success is an upgrade
                                                Just Success

                                            else
                                                maybeValue

                                        Success ->
                                            -- Nothing is an upgrade
                                            maybeValue

                                Nothing ->
                                    -- Everything is an upgrade
                                    Just guessCharState
                        )
                        acc
                )
                Dict.empty
                (zip (List.concat model.pastGuesses) (List.concat pastGuessesCharState))

        won : Bool
        won =
            isGameOver model
    in
    div [ class "container" ]
        [ h1 [ class "tc" ] [ text "Elmwordle" ]
        , case model.error of
            Just error ->
                p [ style "color" "red", style "text-align" "center" ] [ text error ]

            Nothing ->
                text ""

        -- The Gameboard
        , div []
            (List.concat
                -- Attempts
                [ if isGameOver model then
                    [ div
                        [ style "position" "absolute"
                        , style "left" "50%"
                        , style "width" "440px"
                        , style "height" "540px"
                        , style "transform" "translate(-50%, 0)"
                        , style "font-size" "60px"
                        , style "vertical-align" "middle"
                        , style "background-color" "rgba(255, 255, 255, 0.7)"
                        , style "letter-spacing" "12px"
                        , style "font-weight" "bold"
                        ]
                        [ div
                            [ style "position" "absolute"
                            , style "width" "100%"
                            , style "top" "50%"
                            , style "transform" "translate(0, -50%) rotate(10deg)"
                            , style "text-align" "center"
                            ]
                            (if isGameWon model then
                                [ text "CORRECT!"
                                , br [] []
                                , text "🎉"
                                ]

                             else
                                [ text "Game Over!"
                                , br [] []
                                , text "⚡"
                                ]
                            )
                        ]
                    ]

                  else
                    []
                , List.map
                    (\( pastGuessAttempts, pastGuessCharState ) ->
                        viewRowCharacters pastGuessAttempts pastGuessCharState
                    )
                    (zip model.pastGuesses pastGuessesCharState)

                -- Current guess
                , if List.length model.pastGuesses > boardSize.height then
                    []

                  else
                    [ viewRowCharacters
                        (model.currentGuess ++ List.repeat (boardSize.width - List.length model.currentGuess) ' ')
                        (List.repeat boardSize.width Muted)
                    ]

                -- Fill the rest of the space with ' ' to reach the required board size
                , List.map
                    (\empty -> viewRowCharacters empty (List.repeat boardSize.width Muted))
                    (List.repeat (boardSize.height - List.length model.pastGuesses)
                        (List.repeat boardSize.width ' ')
                    )
                ]
            )

        -- The Keyboard
        , viewRowKeyboard [ viewKeyboardKeys won keyboardCharState "QWERTYUIOP" ]
        , viewRowKeyboard [ viewKeyboardKeys won keyboardCharState "ASDFGHJKL" ]
        , viewRowKeyboard
            [ [ viewButton won "Enter" 13 "w-auto" ]
            , viewKeyboardKeys won keyboardCharState "ZXCVBNM"
            , [ viewButton won "⌫" 8 "w-auto" ]
            ]
        , p
            [ style "text-align" "center"
            , style "margin-top" "20px"
            , style "font-size" "15px"
            , style "line-height" "24px"
            ]
            [ a [ href "https://arhamjain.com/hyperwordle/" ] [ text "Inspired by Hyperwordle" ]
            , text " | "
            , a [ href "https://github.com/lucamug/elmwordle/blob/master/src/Main.elm" ] [ text "Source Code" ]
            , text " | "
            , a [ href "https://dev.to/lucamug/elm-vs-hyperscript-2m3m" ] [ text "Post" ]
            , br [] []
            , text "Reload the page to guess a new word."
            , br [] []
            , text "Open the Elm Debugger to see how it works →"
            ]
        ]


viewButton : Bool -> String -> Int -> String -> Html Msg
viewButton won keyText keyCode width =
    button
        ([ class <| "secondary ma1 pa2 " ++ width ]
            ++ (if won then
                    [ disabled True ]

                else
                    [ onClick (KeyDown keyCode) ]
               )
        )
        [ text keyText ]


viewRowCharacters : List Char -> List CharState -> Html msg
viewRowCharacters listChar listCharState =
    div [ class "contrast flex ttu tc b f3 w-100 justify-center" ]
        (List.map
            (\( char, charState ) ->
                span [ class <| "w3 h3 ma1 pa3 " ++ charStateToClass charState ]
                    [ text <| String.fromChar char ]
            )
            (zip listChar listCharState)
        )


viewRowKeyboard : List (List (Html msg)) -> Html msg
viewRowKeyboard lol =
    div [ class "flex justify-center ttu tc" ] <| List.concat lol


viewKeyboardKeys : Bool -> Dict.Dict Char CharState -> String -> List (Html Msg)
viewKeyboardKeys won keyboardCharState string =
    string
        |> String.toList
        |> List.map
            (\char ->
                viewButton
                    won
                    (String.fromChar char)
                    (Char.toCode char)
                    ("w2"
                        ++ (case Dict.get char keyboardCharState of
                                Just charState ->
                                    " " ++ charStateToClass Muted ++ " " ++ charStateToClass charState

                                Nothing ->
                                    ""
                           )
                    )
            )


getWords : Float -> Cmd Msg
getWords randomFloat =
    Http.get
        { url = "words.json"
        , expect = Http.expectJson (GotWords randomFloat) (Json.Decode.list Json.Decode.string)
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if isGameOver model then
        Sub.none

    else
        Browser.Events.onKeyDown (Json.Decode.map KeyDown Html.Events.keyCode)


zip : List a -> List b -> List ( a, b )
zip =
    -- Take two lists and returns a list of corresponding pairs
    List.map2 Tuple.pair


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
