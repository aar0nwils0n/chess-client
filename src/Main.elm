module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Dom.Scroll exposing (..)
import Task exposing (..)


type Msg
    = NewMessage String
    | UpdateLine String
    | Submit
    | NoOp


type alias Model =
    { messages : List String
    , line : String
    , guid : String
    }


type alias Flags =
    { guid : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { messages = []
      , line = ""
      , guid = flags.guid
      }
    , Cmd.none
    )


view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "height", "100vh" )
            , ( "justify-content", "flex-end" )
            , ( "flex-direction", "column" )
            , ( "background", "black" )
            ]
        ]
        [ div
            [ id "history"
            , style
                [ ( "color", "white" )
                , ( "max-height", "calc(100vh - 50px)" )
                , ( "overflow", "auto" )
                , ( "line-height", "0.85" )
                , ( "font-size", "20px" )
                ]
            ]
          <|
            List.map (\message -> pre [] [ text message ]) model.messages
        , div [ style [ ( "background-color", "black" ), ( "color", "white" ), ( "padding", "5px 0" ), ( "display", "flex" ), ( "justify-content", "space-between" ) ] ]
            [ div [ style [] ]
                [ input
                    [ style
                        [ ( "background", "black" )
                        , ( "border", "0" )
                        , ( "border-bottom", "1px solid white" )
                        , ( "color", "white" )
                        , ( "outline", "0" )
                        , ( "height", "30px" )
                        ]
                    , onInput
                        UpdateLine
                    , value model.line
                    ]
                    []
                , button
                    [ style
                        [ ( "color", "white" )
                        , ( "background", "black" )
                        , ( "border", "1px solid white" )
                        , ( "margin-left", "5px" )
                        , ( "padding", "5px" )
                        , ( "outline", "0" )
                        ]
                    , onClick Submit
                    ]
                    [ text "Submit" ]
                ]
            , div [] [ text "Commands: start game foo | join game bar | a2 a4" ]
            ]
        ]


serverAddr =
    "ws://localhost:9160"


scrollToBottom : Cmd Msg
scrollToBottom =
    Task.attempt (always NoOp) <| toBottom "history"


update msg model =
    case msg of
        NewMessage message ->
            ( { model | messages = model.messages ++ [ message ] }
            , scrollToBottom
            )

        UpdateLine text ->
            ( { model | line = text }, Cmd.none )

        Submit ->
            ( { model | line = "" }, WebSocket.send serverAddr <| model.guid ++ model.line )

        NoOp ->
            ( model, Cmd.none )


subscriptions model =
    Sub.batch
        [ WebSocket.listen serverAddr NewMessage
        ]


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
