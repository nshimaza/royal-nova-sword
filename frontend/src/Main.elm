{-
   Copyright (c) 2020 Cisco and/or its affiliates.

   This software is licensed to you under the terms of the Cisco Sample
   Code License, Version 1.1 (the "License"). You may obtain a copy of the
   License at

               https://developer.cisco.com/docs/licenses

   All use of the material herein must be in accordance with the terms of
   the License. All rights not expressly granted by the License are
   reserved. Unless required by applicable law or agreed to separately in
   writing, software distributed under the License is distributed on an "AS
   IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied.
-}


module Main exposing (main)

import Browser
import Element exposing (Attribute, Element, column, el, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Task



-- Types


type RnsState
    = Unlocked
    | Starting
    | Learning
    | Locking
    | Locked
    | Unlocking
    | BackendError


showAgentState : RnsState -> String
showAgentState state =
    case state of
        Unlocked ->
            "Unlocked"

        Starting ->
            "Starting"

        Learning ->
            "Learning"

        Locking ->
            "Locking"

        Locked ->
            "Locked"

        Unlocking ->
            "Unlocking"

        BackendError ->
            "BackendError"


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- Model


type alias Model =
    { rnsState : RnsState
    , rnsBackendError : Maybe Http.Error
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rnsState = Unlocked
      , rnsBackendError = Nothing
      }
    , Cmd.none
    )



-- Update


type Msg
    = LearnPressed
    | LearningStarted (Result Http.Error ())
    | LockPressed
    | LockDone (Result Http.Error ())
    | UnlockPressed
    | UnlockDone (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LearnPressed ->
            ( { model | rnsState = Starting }
            , Http.post
                { url = "/api"
                , body = Http.emptyBody
                , expect = Http.expectWhatever LearningStarted
                }
            )

        LearningStarted result ->
            case result of
                Ok _ ->
                    ( { model | rnsState = Learning, rnsBackendError = Nothing }, Cmd.none )

                Err err ->
                    ( { model | rnsState = BackendError, rnsBackendError = Just err }, Cmd.none )

        LockPressed ->
            ( { model | rnsState = Locking }
            , Http.request
                { method = "PUT"
                , headers = []
                , url = "/api"
                , body = Http.emptyBody
                , expect = Http.expectWhatever LockDone
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        LockDone result ->
            case result of
                Ok _ ->
                    ( { model | rnsState = Locked, rnsBackendError = Nothing }, Cmd.none )

                Err err ->
                    ( { model | rnsState = BackendError, rnsBackendError = Just err }, Cmd.none )

        UnlockPressed ->
            ( { model | rnsState = Unlocking }
            , Http.request
                { method = "DELETE"
                , headers = []
                , url = "/api"
                , body = Http.emptyBody
                , expect = Http.expectWhatever UnlockDone
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        UnlockDone result ->
            case result of
                Ok _ ->
                    ( { model | rnsState = Unlocked, rnsBackendError = Nothing }, Cmd.none )

                Err err ->
                    ( { model | rnsState = BackendError, rnsBackendError = Just err }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    Element.layout [ padding 7 ] <|
        column []
            [ row [ padding 10 ] [ text "Royal Nova Sword" ]
            , row [ padding 10, Font.color <| rgb255 255 0 0 ] [ text <| showAgentState model.rnsState ]
            , row [ padding 10, spacing 20 ]
                [ learnButton model.rnsState
                , lockButton model.rnsState
                , unlockButton model.rnsState
                ]
            ]


enabledButtonAttr : List (Attribute Msg)
enabledButtonAttr =
    [ padding 5, Border.width 1, Border.rounded 3 ]


disabledButtonAttr : List (Attribute Msg)
disabledButtonAttr =
    [ padding 5, Border.width 1, Border.rounded 3, Background.color <| rgb255 128 128 128 ]


getButtonAttr : Maybe a -> List (Attribute Msg)
getButtonAttr selected =
    case selected of
        Nothing ->
            disabledButtonAttr

        _ ->
            enabledButtonAttr


learnButton : RnsState -> Element Msg
learnButton state =
    let
        ( attr, onP ) =
            case state of
                Unlocked ->
                    ( enabledButtonAttr, Just LearnPressed )

                Starting ->
                    ( disabledButtonAttr, Nothing )

                Learning ->
                    ( disabledButtonAttr, Nothing )

                Locking ->
                    ( disabledButtonAttr, Nothing )

                Locked ->
                    ( enabledButtonAttr, Just LearnPressed )

                Unlocking ->
                    ( disabledButtonAttr, Nothing )

                BackendError ->
                    ( disabledButtonAttr, Nothing )
    in
    Input.button attr
        { onPress = onP
        , label = text "Learn"
        }


lockButton : RnsState -> Element Msg
lockButton state =
    let
        ( attr, onP ) =
            case state of
                Unlocked ->
                    ( disabledButtonAttr, Nothing )

                Starting ->
                    ( disabledButtonAttr, Nothing )

                Learning ->
                    ( enabledButtonAttr, Just LockPressed )

                Locking ->
                    ( disabledButtonAttr, Nothing )

                Locked ->
                    ( disabledButtonAttr, Nothing )

                Unlocking ->
                    ( disabledButtonAttr, Nothing )

                BackendError ->
                    ( disabledButtonAttr, Nothing )
    in
    Input.button attr
        { onPress = onP
        , label = text "Lock"
        }


unlockButton : RnsState -> Element Msg
unlockButton state =
    let
        ( attr, onP ) =
            case state of
                Unlocked ->
                    ( disabledButtonAttr, Nothing )

                Starting ->
                    ( disabledButtonAttr, Nothing )

                Learning ->
                    ( enabledButtonAttr, Just UnlockPressed )

                Locking ->
                    ( disabledButtonAttr, Nothing )

                Locked ->
                    ( enabledButtonAttr, Just UnlockPressed )

                Unlocking ->
                    ( disabledButtonAttr, Nothing )

                BackendError ->
                    ( enabledButtonAttr, Just UnlockPressed )
    in
    Input.button attr
        { onPress = onP
        , label = text "Unlock"
        }
