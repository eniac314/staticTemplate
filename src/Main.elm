port module Main exposing (..)

import Browser exposing (..)
import Browser.Events exposing (onAnimationFrame, onResize)
import Browser.Navigation as Nav
import Color exposing (..)
import Dict exposing (..)
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
import Url as Url
import Url.Builder as UrlBuilder exposing (..)
import Url.Parser as UrlParser exposing (..)


port scroll : (Int -> msg) -> Sub msg


scrollTo =
    scrollToWithOptions
        { defaultConfig
            | target = Just "appContainer"
            , offset = 50
        }


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangeUrl
        }


type alias Model =
    { menuVisible : Bool
    , scrollTop : Int
    , device : Device
    , width : Int
    , height : Int
    , key : Nav.Key
    , url : Url.Url
    , sideMenuOpen : Bool
    , updateOnNextFrame : Maybe Msg
    , currentPosition : { path : Path, anchor : Maybe String }
    }


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    , scrollTop : Int
    }


type Msg
    = ChangeUrl Url.Url
    | ClickedLink UrlRequest
    | Tick Posix
    | Scrolled Int
    | WinResize Int Int
    | SmoothScroll String
    | SyncedUpdate Msg
    | NoOp


type alias Path =
    String


type alias Anchor =
    String


type alias Label =
    String


type alias ExternalUrl =
    String


type Link
    = Internal Path Label (Maybe Anchor)
    | External ExternalUrl Label


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { menuVisible = True
      , scrollTop = flags.scrollTop
      , device = classifyDevice { width = flags.width, height = flags.height }
      , width = flags.width
      , height = flags.height
      , key = key
      , url = url
      , sideMenuOpen = False
      , updateOnNextFrame = Nothing
      , currentPosition = { path = "/home", anchor = Nothing }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ChangeUrl url ->
            let
                pathParser =
                    UrlParser.oneOf
                        [ UrlParser.map
                            (\anchor -> ( "/home", anchor ))
                            (UrlParser.top </> UrlParser.fragment identity)
                        , UrlParser.map
                            (\anchor -> ( "/page1", anchor ))
                            (UrlParser.s "page1" </> UrlParser.fragment identity)
                        ]
            in
            case UrlParser.parse pathParser url of
                Just ( path, mbAnchor ) ->
                    ( { model
                        | currentPosition =
                            { path = path
                            , anchor = mbAnchor
                            }
                        , updateOnNextFrame =
                            case mbAnchor of
                                Just anchor ->
                                    Just (SmoothScroll anchor)

                                Nothing ->
                                    model.updateOnNextFrame
                        , url = url
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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

        SyncedUpdate msg_ ->
            update msg_ { model | updateOnNextFrame = Nothing }

        NoOp ->
            ( model, Cmd.none )


subscriptions model =
    Sub.batch
        [ scroll Scrolled
        , onResize WinResize
        , case model.updateOnNextFrame of
            Just msg ->
                onAnimationFrame (\_ -> SyncedUpdate msg)

            Nothing ->
                Sub.none
        ]


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
                , Dict.get model.currentPosition.path content
                    |> Maybe.withDefault Element.none
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



-------------------------------------------------------------------------------
-- View functions --


menuView model =
    let
        menuItems =
            [ Internal "/" "item 1" (Just "item1")
            , Internal "/" "item 2" (Just "item2")
            , Internal "/" "item 3" (Just "item3")
            , Internal "/page1" "item 4" (Just "item4")
            , Internal "/page1" "item 5" (Just "item5")
            , Internal "/page2" "item 6" Nothing
            , External "https://google.com" "google"
            ]

        itemLenght =
            max 100 (model.width // List.length menuItems)

        itemView itemLink =
            case itemLink of
                Internal path label mbAnchor ->
                    link
                        [ width (px itemLenght)
                        , Background.color (Element.rgba 0.8 0.6 0.7 0.4)
                        , padding 15
                        , pointer
                        ]
                        { url =
                            UrlBuilder.custom
                                Relative
                                (String.split "/" path)
                                []
                                mbAnchor
                        , label = text label
                        }

                External externalUrl label ->
                    newTabLink
                        [ width (px itemLenght)
                        , Background.color (Element.rgba 0.8 0.6 0.7 0.4)
                        , padding 15
                        , pointer
                        ]
                        { url = externalUrl
                        , label = text label
                        }

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
                [ el
                    [ Background.color (Element.rgba 0.4 0.5 0.7 0.4)
                    , width fill
                    , height <|
                        if model.menuVisible then
                            px <| menuHeaderHeight model

                        else
                            px 0
                    , htmlAttribute <| HtmlAttr.style "transition" "height 0.3s"
                    ]
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


noAttr =
    htmlAttribute <| HtmlAttr.class ""


col : Color.Color -> Element.Color
col c =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue c_.alpha
           )



-------------------------------------------------------------------------------
-- Content


content : Dict String (Element msg)
content =
    Dict.fromList
        [ ( "/home"
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
          )
        ]
