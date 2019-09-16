port module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.element { init = init, subscriptions = subs, update = update, view = view }



-- MODEL


type alias Model =
    { code : String
    , result : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = "", result = "" }, Cmd.none )



-- SUBSCRIPTIONS and PORTS


port toJS : String -> Cmd m


port toElm : (String -> m) -> Sub m


subs : Model -> Sub Msg
subs _ =
    toElm Receive



-- UPDATE


type Msg
    = CodeEdit String
    | Receive String
    | Evaluate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeEdit newCode ->
            ( { model | code = newCode }, Cmd.none )

        Receive res ->
            ( { model | result = res }, Cmd.none )

        Evaluate ->
            ( { model | result = "working..." }, toJS model.code )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ placeholder "bosatsu code here", value model.code, onInput CodeEdit ] []
        , button [ onClick Evaluate ] [ text "go" ]
        , div [] [ text model.result ]
        ]
