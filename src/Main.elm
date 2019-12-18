port module Main exposing (..)

import Browser exposing (..)
import Browser.Events exposing (onAnimationFrame, onResize)
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html as Html
import Html.Attributes as HtmlAttr
import Scroll.Scroll exposing (defaultConfig, scrollToWithOptions)
import Task exposing (..)
import Time exposing (..)


port scroll : (Int -> msg) -> Sub msg


scrollTo =
    scrollToWithOptions
        { defaultConfig
            | target = Just "appContainer"
            , offset = 50
        }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { menuVisible : Bool
    , scrollTop : Int
    , device : Device
    , width : Int
    , height : Int
    }


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    , scrollTop : Int
    }


type Msg
    = Tick Posix
    | Scrolled Int
    | WinResize Int Int
    | SmoothScroll String
    | NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { menuVisible = True
      , scrollTop = flags.scrollTop
      , device = classifyDevice { width = flags.width, height = flags.height }
      , width = flags.width
      , height = flags.height
      }
    , Cmd.none
    )


view model =
    { title = "Basic template"
    , body =
        [ Element.layout
            [ width fill
            , height (px model.height)
            , Font.size 16
            , Background.image <|
                "https://via.placeholder.com/"
                    ++ String.fromInt model.width
                    ++ "x"
                    ++ String.fromInt model.height
            , clip
            , inFront <| menuView model

            --, if not model.menuVisible then
            --    inFront (menuView model True)
            --  else
            --    noAttr
            ]
            (column
                [ width fill
                , scrollbarY
                , htmlAttribute <| HtmlAttr.id "appContainer"
                , htmlAttribute <| HtmlAttr.style "-webkit-overflow-scrolling" "touch"
                ]
                [ el
                    [ width fill
                    , height (px <| menuHeaderHeight model + 45)
                    , htmlAttribute <| HtmlAttr.id "appTop"
                    ]
                    Element.none
                , column
                    [ width fill
                    , height (px 2000)
                    , spacing 15
                    ]
                    [ el
                        [ width fill
                        , padding 15
                        , Background.color (col white)
                        , htmlAttribute <| HtmlAttr.id "item1"
                        ]
                        (text "item 1")
                    , el
                        [ width fill
                        , height (px 500)
                        ]
                        Element.none
                    , paragraph
                        [ width fill
                        , padding 15
                        , Background.color (col white)
                        , htmlAttribute <| HtmlAttr.id "item2"
                        ]
                        [ text "item 2" ]
                    ]
                , text "footer"
                ]
            )
        ]
    }


menuHeaderHeight model =
    case model.device.class of
        Phone ->
            75

        Tablet ->
            75

        Desktop ->
            125

        BigDesktop ->
            125


menuView model =
    let
        menuItems =
            [ ( "item 1", "item1" )
            , ( "item 2", "item2" )
            , ( "item 3", "item3" )
            , ( "item 4", "item4" )
            , ( "item 5", "item5" )
            , ( "item 6", "item6" )
            ]

        itemLenght =
            200

        itemView ( label, dest ) =
            el
                [ width (px itemLenght)
                , Background.color (Element.rgba 0.8 0.6 0.7 0.4)
                , padding 15
                , onClick (SmoothScroll dest)
                , pointer
                ]
                (text label)

        phoneView =
            column
                []
                []

        tabletView =
            column
                []
                []

        desktopView =
            column
                [ width fill ]
                [ if model.menuVisible then
                    el
                        [ Background.color (Element.rgba 0.4 0.5 0.7 0.4)
                        , width fill
                        , height (px <| menuHeaderHeight model)
                        ]
                        Element.none

                  else
                    Element.none
                , row
                    [ width fill
                    , spaceEvenly
                    , Background.color (Element.rgba 0.4 0.5 0.7 0.4)
                    ]
                    (List.map itemView menuItems)
                ]
    in
    case model.device.class of
        Phone ->
            phoneView

        Tablet ->
            tabletView

        Desktop ->
            desktopView

        BigDesktop ->
            desktopView


update msg model =
    case msg of
        Tick time ->
            ( model, Cmd.none )

        Scrolled n ->
            ( { model
                | scrollTop = n
                , menuVisible = n == 0
              }
            , Cmd.none
            )

        WinResize width height ->
            ( { model
                | width = width
                , height = height
                , device = classifyDevice { width = width, height = height }
              }
            , Cmd.none
            )

        SmoothScroll id ->
            ( model, Task.attempt (always NoOp) (scrollTo id) )

        NoOp ->
            ( model, Cmd.none )


subscriptions model =
    Sub.batch
        [ scroll Scrolled
        , onResize WinResize
        ]


noAttr =
    htmlAttribute <| HtmlAttr.class ""


col : Color.Color -> Element.Color
col c =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue c_.alpha
           )
