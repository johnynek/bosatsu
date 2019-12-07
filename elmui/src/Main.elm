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


type Stack a
    = EmptyStack
    | NonEmpty (List a) a (List a)


pushIfNew : comparable -> Stack comparable -> Stack comparable
pushIfNew item stack =
    case stack of
        EmptyStack ->
            NonEmpty [] item []

        NonEmpty prev cur next ->
            if cur /= item then
                NonEmpty (cur :: prev) item next

            else
                stack


type alias Model =
    { code : String
    , history : Stack String
    , result : Result
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = "", history = EmptyStack, result = EmptyResult }, Cmd.none )



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
    | BackStack
    | ForwardStack


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
            ( { model | result = Working, history = pushIfNew model.code model.history }, toJS model.code )

        BackStack ->
            case model.history of
                EmptyStack ->
                    ( model, Cmd.none )

                NonEmpty [] _ _ ->
                    ( model, Cmd.none )

                NonEmpty (p0 :: p1) c n ->
                    ( { model | history = NonEmpty p1 p0 (c :: n), code = p0 }, Cmd.none )

        ForwardStack ->
            case model.history of
                EmptyStack ->
                    ( model, Cmd.none )

                NonEmpty _ _ [] ->
                    ( model, Cmd.none )

                NonEmpty p c (n0 :: n1) ->
                    ( { model | history = NonEmpty (c :: p) n0 n1, code = n0 }, Cmd.none )



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
                        [ text "last value:"
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

        runB =
            button [ Background.color green, Element.padding 10, Border.rounded 10 ]
                { onPress = Just Evaluate
                , label = text "run"
                }

        lightBlue =
            Background.color (Element.rgb255 201 213 255)

        backB =
            button [ lightBlue, Element.padding 10, Border.rounded 10 ]
                { onPress = Just BackStack
                , label = text "back"
                }

        forB =
            button [ lightBlue, Element.padding 10, Border.rounded 10 ]
                { onPress = Just ForwardStack
                , label = text "forward"
                }

        table =
            row [ Element.width Element.fill, Element.spacing 10 ]
                [ column [ Element.width Element.fill, Element.spacing 15 ]
                    [ el [ Element.width (Element.fillPortion 3) ] textArea
                    , row [ Element.centerX ] [ backB, runB, forB ]
                    ]
                , el [ Element.width (Element.fillPortion 1) ] res
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
