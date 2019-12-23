port module Main exposing (..)

import Animation exposing (..)
import Browser exposing (..)
import Browser.Events exposing (Visibility(..), onAnimationFrame, onResize, onVisibilityChange)
import Browser.Navigation as Nav
import Color exposing (..)
import Dict exposing (..)
import Dict.Extra exposing (filterMap)
import Ease exposing (linear)
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
import Html.Events as HtmlEvents exposing (on)
import Internal.Scroll exposing (defaultConfig, scrollToWithOptions)
import Internal.Streams exposing (..)
import Ionicon
import Ionicon.Android
import Json.Decode as Decode
import Set exposing (..)
import Task exposing (..)
import Time exposing (..)
import Url as Url
import Url.Builder as UrlBuilder exposing (..)
import Url.Parser as UrlParser exposing (..)


port scroll : (Int -> msg) -> Sub msg


subscriptions model =
    Sub.batch
        [ scroll Scrolled
        , onResize WinResize
        , onVisibilityChange VisibilityChange
        , if
            (model.currentAnimation == Nothing)
                && (not <|
                        Dict.Extra.any
                            (\k (CustomAnim anim) -> isRunning model.clock anim.animation)
                            model.customAnimations
                   )
          then
            Sub.none

          else
            onAnimationFrame Tick
        , if model.animate && (model.visibility == Visible) then
            Time.every 6000 (Animate "slideShow")
            --Dict.map (\_ (CustomAnim a) -> a.recurring) model.customAnimations
            --    |> Dict.Extra.filterMap (\_ -> identity)
            --    |> Dict.map (\k t -> Time.every t (Animate k))
            --    |> Dict.values
            --    |> Sub.batch

          else
            Sub.none
        , case model.updateOnNextFrame of
            Just msg ->
                onAnimationFrame (\_ -> SyncedUpdate msg)

            Nothing ->
                Sub.none
        ]


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
    { headerVisible : Bool
    , scrollTop : Int
    , scrollbarWidth : Int
    , device : Device
    , width : Int
    , height : Int
    , visibility : Visibility
    , animate : Bool
    , currentAnimation : Maybe Animation
    , customAnimations : Dict String CustomAnim
    , clock : Float
    , key : Nav.Key
    , url : Url.Url
    , sideMenuOpen : Bool
    , updateOnNextFrame : Maybe Msg
    , currentPosition : { path : Path, anchor : Maybe String }
    , loaded : Set String
    , images : BiStream (List Image)
    }


type CustomAnim
    = CustomAnim
        { name : String
        , animation : Animation
        , reversed : Bool
        , recurring : Maybe Float
        , startUpdate : Maybe (Model -> ( Model, Cmd Msg ))
        , endUpdate : Maybe (Model -> ( Model, Cmd Msg ))
        }


type alias Image =
    { src : String
    , id : Int
    }


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    , scrollTop : Int
    , scrollbarWidth : Int
    }


type Msg
    = ChangeUrl Url.Url
    | ClickedLink UrlRequest
    | Animate String Posix
    | Tick Posix
    | Scrolled Int
    | WinResize Int Int
    | VisibilityChange Visibility
    | SmoothScroll String
    | SyncedUpdate Msg
    | ToogleSideMenu
    | ImgLoaded String
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
    let
        imgs =
            [ "/assets/images/pic1.jpg"
            , "/assets/images/pic2.jpg"
            , "/assets/images/pic3.jpg"
            ]

        stream =
            List.indexedMap
                (\n s -> Image s n)
                imgs
                |> (\xs -> biStream xs (Image "" -1))
                |> chunkBiStream 3

        animations =
            Dict.fromList
                [ ( "slideShow"
                  , CustomAnim
                        { name = "slideShow"
                        , animation =
                            animation (toFloat flags.currentTime)
                                |> from 1
                                |> to 0
                                |> duration 1500
                                |> ease Ease.linear
                        , reversed = False
                        , recurring = Just 6000
                        , startUpdate = Nothing
                        , endUpdate = Nothing
                        }
                  )
                ]
    in
    ( { headerVisible = True
      , scrollTop = flags.scrollTop
      , scrollbarWidth = flags.scrollbarWidth
      , device = classifyDevice { width = flags.width, height = flags.height }
      , width = flags.width
      , height = flags.height
      , visibility = Visible
      , animate = True
      , currentAnimation = Nothing
      , customAnimations = animations
      , clock = toFloat flags.currentTime
      , key = key
      , url = url
      , sideMenuOpen = False
      , updateOnNextFrame = Nothing
      , currentPosition = { path = "/home", anchor = Nothing }
      , loaded = Set.empty
      , images = stream
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
                        , sideMenuOpen = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Animate name time ->
            case Dict.get name model.customAnimations of
                Just (CustomAnim anim) ->
                    let
                        newClock =
                            toFloat <| posixToMillis time

                        newAnimation =
                            let
                                f =
                                    getFrom anim.animation

                                t =
                                    getTo anim.animation

                                d =
                                    getDuration anim.animation

                                e =
                                    getEase anim.animation
                            in
                            animation newClock
                                |> from f
                                |> to t
                                |> duration d
                                |> ease e

                        newAnim =
                            case model.currentAnimation of
                                Nothing ->
                                    Just
                                        (animation newClock
                                            |> from 1
                                            |> to 0
                                            |> duration 1500
                                            |> ease Ease.linear
                                        )

                                _ ->
                                    Nothing
                    in
                    ( { model
                        | currentAnimation = newAnim
                        , customAnimations =
                            Dict.insert name
                                (CustomAnim { anim | animation = newAnimation })
                                model.customAnimations
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Tick time ->
            let
                newClock =
                    toFloat <| posixToMillis time

                --(newAnimations, updates) =
                --    Dict.foldr
                --        (\k (CustomAnim anim) (anims, upd) ->
                --            if isDone newClock anim then
                --                )
                ( newAnim, newImages ) =
                    case model.currentAnimation of
                        Just anim ->
                            if isDone newClock anim then
                                ( Nothing, left (.images model) )

                            else
                                ( model.currentAnimation, model.images )

                        _ ->
                            ( model.currentAnimation, model.images )
            in
            ( { model
                | clock = newClock
                , currentAnimation = newAnim
                , images = newImages
              }
            , Cmd.none
            )

        Scrolled n ->
            ( { model
                | scrollTop = n
                , headerVisible =
                    n == 0
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

        VisibilityChange visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        SmoothScroll id ->
            ( model, Task.attempt (always NoOp) (scrollTo model id) )

        SyncedUpdate msg_ ->
            update msg_ { model | updateOnNextFrame = Nothing }

        ToogleSideMenu ->
            ( { model | sideMenuOpen = not model.sideMenuOpen }, Cmd.none )

        ImgLoaded src ->
            ( { model | loaded = Set.insert src model.loaded }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


scrollTo model id =
    scrollToWithOptions
        { defaultConfig
            | target = Just "appContainer"
            , offset =
                if id == "appTop" then
                    headerHeight model + mainMenuHeight model

                else
                    mainMenuHeight model
        }
        id



-------------------------------------------------------------------------------
-- View functions --


view model =
    { title = "Basic template"
    , body =
        [ Element.layout
            [ width fill
            , height (px model.height)
            , Font.size 16
            , behindContent (galleryView model)
            , inFront <| mainMenuView model
            , clip
            ]
            (column
                [ width fill
                , height (minimum (model.height - (headerHeight model + mainMenuHeight model)) fill)
                , htmlAttribute <| HtmlAttr.id "appContainer"
                , htmlAttribute <| HtmlAttr.style "-webkit-overflow-scrolling" "touch"
                , if model.sideMenuOpen then
                    noAttr

                  else
                    scrollbarY
                ]
                [ el
                    [ width fill
                    , height (px <| headerHeight model + mainMenuHeight model)
                    , htmlAttribute <| HtmlAttr.id "appTop"
                    ]
                    Element.none
                , Dict.get
                    model.currentPosition.path
                    content
                    |> Maybe.withDefault Element.none
                , footerView model
                ]
            )
        ]
    }



-------------------------------------------------------------------------------
-- Background slide show


galleryView model =
    el
        [ centerX
        , clipX
        , width fill
        , Background.color (col lightGrey)
        ]
        (chunkView model (current model.images))


chunkView model chunk =
    let
        frontOpacity =
            case model.currentAnimation of
                Just anim ->
                    alpha <| animate model.clock anim

                _ ->
                    alpha 1

        backOpacity =
            case model.currentAnimation of
                Just anim ->
                    alpha <| 1 - animate model.clock anim

                _ ->
                    alpha 0
    in
    case chunk of
        l :: c :: r :: [] ->
            row
                [ moveLeft (toFloat model.width)
                ]
                [ backgroundPicView model l []
                , el
                    [ behindContent
                        (backgroundPicView model l [ backOpacity ])
                    ]
                    (backgroundPicView model c [ frontOpacity ])
                , backgroundPicView model r []
                ]

        _ ->
            Element.none


backgroundPicView model { src } attrs =
    if Set.member src model.loaded then
        el
            ([ Background.image src
             , height (px model.height)
             , width (px <| model.width)
             ]
                ++ attrs
            )
            Element.none

    else
        column
            [ width fill
            , height (px model.height)
            ]
            [ html <|
                Html.img
                    [ HtmlAttr.hidden True
                    , HtmlEvents.on "load" (Decode.succeed (ImgLoaded src))
                    , HtmlAttr.src src
                    ]
                    []
            ]



-------------------------------------------------------------------------------
-- menu functions


headerHeight model =
    case model.device.class of
        Phone ->
            0

        Tablet ->
            0

        Desktop ->
            125

        BigDesktop ->
            125


mainMenuHeight model =
    case model.device.class of
        Phone ->
            65

        Tablet ->
            65

        Desktop ->
            45

        BigDesktop ->
            45


menuItems =
    [ Internal "/" "item 1" (Just "item1")
    , Internal "/" "item 2" (Just "item2")
    , Internal "/" "item 3" (Just "item3")
    , Internal "/page1" "item 4" (Just "item4")
    , Internal "/page1" "item 5" (Just "item5")
    , Internal "/page2" "item 6" Nothing
    , External "https://google.com" "google"
    ]


mainMenuView model =
    let
        mainMenuBackgroundColor =
            Element.rgb255 181 166 189

        itemLenght =
            max 100 ((model.width - 40) // List.length menuItems)

        itemView isMobile itemLink =
            let
                itemStyle =
                    if isMobile then
                        [ width fill
                        , padding 15
                        , Font.center
                        ]

                    else
                        [ width (px itemLenght)

                        --, alignLeft
                        , padding 15
                        , pointer
                        ]
            in
            case itemLink of
                Internal path label mbAnchor ->
                    link
                        itemStyle
                        { url =
                            UrlBuilder.custom
                                Relative
                                (String.split "/" path)
                                []
                                mbAnchor
                        , label = el [ centerX ] (text label)
                        }

                External externalUrl label ->
                    newTabLink
                        itemStyle
                        { url = externalUrl
                        , label = el [ centerX ] (text label)
                        }

        sideMenuButton =
            el
                [ onClick ToogleSideMenu
                , pointer
                , paddingEach { sides | right = 15 }
                ]
                (viewIcon
                    (if model.sideMenuOpen then
                        Ionicon.Android.close

                     else
                        Ionicon.navicon
                    )
                    40
                    grey
                )

        logoView size =
            link
                [ pointer ]
                { url =
                    UrlBuilder.custom
                        Relative
                        (String.split "/" "/")
                        []
                        (Just "appTop")
                , label = viewIcon Ionicon.aperture size grey
                }

        mobileView =
            column
                [ width fill ]
                [ row
                    [ width fill
                    , Background.color mainMenuBackgroundColor
                    , inFront
                        (column
                            [ width <|
                                if model.sideMenuOpen then
                                    if model.width > 400 then
                                        px 400

                                    else
                                        px model.width

                                else
                                    px 0
                            , height fill
                            , moveDown (mainMenuHeight model)
                            , alignRight
                            , clip
                            , htmlAttribute <| HtmlAttr.style "transition" "width 0.2s ease-out"
                            , Background.color (col white)
                            ]
                            [ column
                                [ width fill
                                , height (px <| model.height - headerHeight model + mainMenuHeight model)
                                , scrollbarY
                                , spacing 100
                                ]
                                (List.map (itemView True) menuItems)
                            ]
                        )
                    ]
                    [ el
                        [ alignLeft
                        ]
                        (logoView 65)
                    , el
                        [ alignRight
                        , centerY
                        ]
                        sideMenuButton
                    ]
                ]

        desktopView =
            column
                [ width (px <| model.width - model.scrollbarWidth)
                ]
                [ el
                    [ Background.color mainMenuBackgroundColor
                    , width fill
                    , height <|
                        if model.headerVisible then
                            px <| headerHeight model

                        else
                            px 0
                    , htmlAttribute <| HtmlAttr.style "transition" "height 0.3s"
                    , clip
                    ]
                    (el [ centerX ] (logoView 120))
                , row
                    [ width fill
                    , height (px <| mainMenuHeight model)
                    , Background.color (Element.rgb255 166 174 195)
                    ]
                    (List.map (itemView False) menuItems)
                ]
    in
    case model.device.class of
        Phone ->
            mobileView

        Tablet ->
            mobileView

        Desktop ->
            desktopView

        BigDesktop ->
            desktopView



-------------------------------------------------------------------------------
--footerView


footerView model =
    column
        [ width fill ]
        [ link []
            { url =
                UrlBuilder.custom
                    Relative
                    (String.split "/" "/")
                    []
                    (Just "appTop")
            , label = text "top"
            }
        ]



-------------------------------------------------------------------------------


defaultwidth =
    width (maximum 1000 fill)


defaultContainerStyle =
    [ defaultwidth
    , centerX
    , padding 15
    , spacing 15
    ]


noAttr =
    htmlAttribute <| HtmlAttr.class ""


sides =
    { top = 0, left = 0, right = 0, bottom = 0 }



-------------------------------------------------------------------------------


col : Color.Color -> Element.Color
col c =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue c_.alpha
           )


colA : Color.Color -> Float -> Element.Color
colA c a =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue a
           )



-------------------------------------------------------------------------------


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias Icon msg =
    Int -> RGBA -> Html.Html msg


viewIcon : Icon msg -> Int -> Color.Color -> Element msg
viewIcon icon size color =
    el [ centerX, centerY ] <|
        html <|
            icon size
                (Color.toRgba color)



-------------------------------------------------------------------------------
-- Content


content : Dict String (Element msg)
content =
    Dict.fromList
        [ ( "/home"
          , column
                [ width fill
                , spacing 15
                ]
                [ el
                    [ height (px 200)
                    ]
                    Element.none
                , el
                    [ width fill
                    , padding 15
                    , Background.color (col white)
                    , htmlAttribute <| HtmlAttr.id "item1"
                    , Background.color (col white)
                    ]
                    (column
                        defaultContainerStyle
                        [ el
                            [ Font.bold
                            , Font.size 22
                            ]
                            (text "Item 1")
                        , paragraph
                            []
                            [ text "Proin vitae lobortis leo. Maecenas sed rhoncus mi, at lobortis augue. Sed sollicitudin libero non varius aliquet. Quisque eget euismod ligula, sodales tristique nunc. Maecenas diam leo, pulvinar quis lobortis at, rutrum at turpis. Curabitur ac lorem vitae tellus rhoncus finibus vitae in arcu. Maecenas eleifend diam ut interdum rutrum. Ut eget pharetra dui. Quisque eget nibh sit amet mauris tincidunt dapibus at at diam. Nunc euismod leo ligula, eget mattis mi porttitor eu. Quisque nec justo at augue cursus cursus. Sed odio turpis, laoreet nec eleifend nec, venenatis eu enim. Nullam a felis dolor. Phasellus varius ultrices dui vulputate dictum. Integer magna arcu, porta eget orci ut, rhoncus consectetur turpis. Integer sodales tortor urna, eu maximus ipsum ullamcorper et. " ]
                        ]
                    )
                , el
                    [ height (px 200)
                    ]
                    Element.none
                , el
                    [ width fill
                    , padding 15
                    , Background.color (col white)
                    , htmlAttribute <| HtmlAttr.id "item2"
                    , Background.color (col white)
                    ]
                    (column
                        defaultContainerStyle
                        [ el
                            [ Font.bold
                            , Font.size 22
                            ]
                            (text "Item 2")
                        , paragraph
                            []
                            [ text "Proin vitae lobortis leo. Maecenas sed rhoncus mi, at lobortis augue. Sed sollicitudin libero non varius aliquet. Quisque eget euismod ligula, sodales tristique nunc. Maecenas diam leo, pulvinar quis lobortis at, rutrum at turpis. Curabitur ac lorem vitae tellus rhoncus finibus vitae in arcu. Maecenas eleifend diam ut interdum rutrum. Ut eget pharetra dui. Quisque eget nibh sit amet mauris tincidunt dapibus at at diam. Nunc euismod leo ligula, eget mattis mi porttitor eu. Quisque nec justo at augue cursus cursus. Sed odio turpis, laoreet nec eleifend nec, venenatis eu enim. Nullam a felis dolor. Phasellus varius ultrices dui vulputate dictum. Integer magna arcu, porta eget orci ut, rhoncus consectetur turpis. Integer sodales tortor urna, eu maximus ipsum ullamcorper et. " ]
                        , paragraph
                            []
                            [ text "Interdum et malesuada fames ac ante ipsum primis in faucibus. Morbi convallis consectetur vulputate. Integer ut mauris mauris. Sed ornare aliquam tortor id eleifend. Integer facilisis elit id erat dapibus, eget efficitur odio elementum. Curabitur in lacus ultrices, convallis ex et, tristique ante. Morbi at pharetra lorem, tincidunt viverra ligula. Vestibulum pharetra magna a turpis fringilla finibus. Nullam nec diam nulla. Aliquam a elit ullamcorper, auctor diam in, maximus libero. "
                            ]
                        ]
                    )
                ]
          )
        ]
