port module Main exposing (..)

import Browser
import Element exposing (column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, labelHidden, multiline, placeholder)
import Html exposing (Html, span)
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


green =
    Element.rgb255 167 255 143


blue =
    Element.rgb 0 0 1


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
                    column []
                        [ text "error:"
                        , Element.html (span [] (textHtml (String.concat [ "<pre>", msg, "</pre>" ])))
                        ]

                JsonValue v ->
                    let
                        j =
                            Json.Encode.encode 4 v
                    in
                    column []
                        [ text "last value as json:"
                        , Element.html (span [] (textHtml (String.concat [ "<pre>", j, "</pre>" ])))
                        ]

        textArea =
            el
                [ Font.family [ Font.monospace ]
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (multiline [ Element.height Element.fill ]
                    { onChange = CodeEdit
                    , text = model.code
                    , placeholder = Just (placeholder [] (text "bosatsu code here"))
                    , label = labelHidden "code"
                    , spellcheck = False
                    }
                )

        b =
            button [ Background.color green, Element.padding 10, Border.rounded 10 ]
                { onPress = Just Evaluate
                , label = text "run"
                }

        table =
            row [ Element.width Element.fill, Element.spacing 10 ]
                [ column [ Element.width Element.fill, Element.spacing 15 ] [
                      el [ Element.width (Element.fillPortion 3) ] textArea,
                      el [ Element.centerX ] b
                    ],
                  el [ Element.width (Element.fillPortion 1) ] res
                ]

        mainElement =
            column [ Element.width Element.fill, Element.spacing 10 ]
                [ Element.paragraph []
                    [ text "Try "
                    , Element.link [ Font.color blue, Font.underline ]
                        { url = "https://github.com/johnynek/bosatsu"
                        , label = text "Bosatsu"
                        }
                    ]
                , table
                ]
    in
    Element.layout [] mainElement
