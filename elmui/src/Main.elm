port module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode



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


port toElm : (Json.Decode.Value -> m) -> Sub m


subs : Model -> Sub Msg
subs _ =
    toElm Receive



-- UPDATE


type Msg
    = CodeEdit String
    | Receive Json.Decode.Value
    | Evaluate


type BosatsuResult
    = BosatsuSuccess Json.Decode.Value
    | BosatsuError String


decodeResult : Json.Decode.Decoder BosatsuResult
decodeResult =
    let
        suc =
            Json.Decode.map BosatsuSuccess (Json.Decode.field "result" Json.Decode.value)

        err =
            Json.Decode.map BosatsuError (Json.Decode.field "error_message" Json.Decode.string)
    in
    Json.Decode.oneOf [ suc, err ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeEdit newCode ->
            ( { model | code = newCode }, Cmd.none )

        Receive res ->
            case Json.Decode.decodeValue decodeResult res of
                Ok (BosatsuSuccess good) ->
                    let
                        goodmsg =
                            Json.Encode.encode 2 good
                    in
                    ( { model | result = goodmsg }, Cmd.none )

                Ok (BosatsuError err) ->
                    ( { model | result = err }, Cmd.none )

                Err err ->
                    ( { model | result = Json.Decode.errorToString err }, Cmd.none )

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
