port module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Json.Decode
import Json.Encode



-- MAIN


main =
    Browser.element { init = init, subscriptions = subs, update = update, view = view }



-- MODEL


type Result
    = JsonValue Json.Decode.Value
    | ErrorMessage String
    | EmptyResult
    | Working


type alias Model =
    { code : String
    , result : Result
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = "", result = EmptyResult }, Cmd.none )



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
                    ( { model | result = JsonValue good }, Cmd.none )

                Ok (BosatsuError err) ->
                    ( { model | result = ErrorMessage err }, Cmd.none )

                Err err ->
                    ( { model | result = ErrorMessage (Json.Decode.errorToString err) }, Cmd.none )

        Evaluate ->
            ( { model | result = Working }, toJS model.code )



-- VIEW

textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []

view : Model -> Html Msg
view model =
    let
        res =
            case model.result of
                Working ->
                    text "working..."

                EmptyResult ->
                    text ""

                ErrorMessage msg ->
                    span [] (textHtml (String.concat ["<pre>", msg, "</pre>"]))

                JsonValue v ->
                    text (Json.Encode.encode 4 v)
    in
    div []
        [ textarea [ placeholder "bosatsu code here", value model.code, onInput CodeEdit ] []
        , button [ onClick Evaluate ] [ text "go" ]
        , div [] [ res ]
        ]
