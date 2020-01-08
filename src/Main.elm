port module Main exposing (..)

import Animation exposing (..)
import Browser exposing (..)
import Browser.Dom exposing (focus, setViewport, setViewportOf)
import Browser.Events exposing (Visibility(..), onAnimationFrame, onResize, onVisibilityChange)
import Browser.Navigation as Nav
import Color exposing (..)
import Countries exposing (..)
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
import Ionicon.Ios
import Json.Decode as D
import List.Extra exposing (findIndex, getAt, remove)
import Set exposing (..)
import String.Extra exposing (toSentenceCase)
import Task exposing (..)
import Time exposing (..)
import Url as Url
import Url.Builder as UrlBuilder exposing (..)
import Url.Parser as UrlParser exposing (..)


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


type alias Path =
    String


type alias Anchor =
    String


type alias Label =
    MlStr


type alias ExternalUrl =
    String


type Link
    = Internal Path Label (Maybe Anchor)
    | External ExternalUrl Label


type Widget
    = LanguagePicker



--# ********************************** #--
--# ***** Multi language strings ***** #--


type Language
    = English
    | Japanese


languages =
    [ English
    , Japanese
    ]


type alias MlStr =
    { en : String
    , jp : String
    }


textM : Language -> MlStr -> Element msg
textM lang mls =
    case lang of
        English ->
            text mls.en

        Japanese ->
            text mls.jp


mlsMap : (String -> String) -> MlStr -> MlStr
mlsMap f { en, jp } =
    MlStr (f en) (f jp)



--# ***** Multi language strings ***** #--
--# ********************************** #--
--# ************************** #--
--# ***** Initialisation ***** #--


port scroll : (Int -> msg) -> Sub msg


subscriptions model =
    Sub.batch
        [ scroll Scrolled
        , onResize WinResize
        , onVisibilityChange VisibilityChange
        , if model.currentAnimation == Nothing then
            Sub.none

          else
            onAnimationFrame Tick
        , if model.animate && (model.visibility == Visible) then
            Time.every 6000 Animate

          else
            Sub.none
        , case model.updateOnNextFrame of
            Just msg ->
                onAnimationFrame (\_ -> SyncedUpdate msg)

            Nothing ->
                Sub.none
        , case model.openedWidget of
            Just LanguagePicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "languagePicker" Close)

            _ ->
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
    , clock : Float
    , key : Nav.Key
    , url : Url.Url
    , sideMenuOpen : Bool
    , updateOnNextFrame : Maybe Msg
    , loaded : Set String
    , images : BiStream (List Image)
    , openedWidget : Maybe Widget
    , lang : Language
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        device =
            classifyDevice { width = flags.width, height = flags.height }

        backgroundFolder =
            if device.class == Phone || device.class == Tablet then
                "regular"

            else
                "background"

        imgs =
            [ "/assets/images/" ++ backgroundFolder ++ "/pic1.jpg"
            , "/assets/images/" ++ backgroundFolder ++ "/pic2.jpg"
            , "/assets/images/" ++ backgroundFolder ++ "/pic3.jpg"
            ]

        stream =
            List.indexedMap
                (\n s -> Image s n)
                imgs
                |> (\xs -> biStream xs (Image "" -1))
                |> chunkBiStream 3
    in
    ( { headerVisible = True
      , scrollTop = flags.scrollTop
      , scrollbarWidth = flags.scrollbarWidth
      , device = device
      , width = flags.width
      , height = flags.height
      , visibility = Visible
      , animate = True
      , currentAnimation = Nothing
      , clock = toFloat flags.currentTime
      , key = key
      , url =
            { url
                | path =
                    if url.path == "/index.html" then
                        "/"

                    else
                        url.path
            }
      , sideMenuOpen = False
      , updateOnNextFrame =
            case url.fragment of
                Just anchor ->
                    Just (SmoothScroll ("$$" ++ anchor))

                Nothing ->
                    Nothing
      , loaded = Set.empty
      , images = stream
      , openedWidget = Nothing
      , lang = English
      }
    , Cmd.none
      --Task.perform (\_ -> NoOp) (setViewport 0 0)
    )



--# ***** Initialisation ***** #--
--# ************************** #--
--# ****************** #--
--# ***** Update ***** #--


type Msg
    = ChangeUrl Url.Url
    | ClickedLink UrlRequest
    | Animate Posix
    | Tick Posix
    | Scrolled Int
    | WinResize Int Int
    | Focus String
    | VisibilityChange Visibility
    | SmoothScroll String
    | SyncedUpdate Msg
    | ToogleSideMenu
    | ImgLoaded String
    | PickLanguage Language
    | OpenLanguagePicker
    | Close
    | PrevLanguage
    | NextLanguage
    | NoOp


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
                            (\anchor -> ( "/", anchor ))
                            (UrlParser.top </> UrlParser.fragment identity)
                        , UrlParser.map
                            (\anchor -> ( "/about", anchor ))
                            (UrlParser.s "about" </> UrlParser.fragment identity)
                        , UrlParser.map
                            (\anchor -> ( "/contact", anchor ))
                            (UrlParser.s "contact" </> UrlParser.fragment identity)
                        ]
            in
            case UrlParser.parse pathParser url of
                Just ( path, mbAnchor ) ->
                    ( { model
                        | updateOnNextFrame =
                            case mbAnchor of
                                Just anchor ->
                                    Just (SmoothScroll ("$$" ++ anchor))

                                Nothing ->
                                    model.updateOnNextFrame
                        , url = url
                        , sideMenuOpen = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Animate time ->
            let
                newClock =
                    toFloat <| posixToMillis time

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
              }
            , Cmd.none
            )

        Tick time ->
            let
                newClock =
                    toFloat <| posixToMillis time

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

        Focus id ->
            ( model, Task.attempt (always NoOp) (focus id) )

        VisibilityChange visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        SmoothScroll id ->
            ( model, Task.attempt (always NoOp) (scrollTo model id) )

        SyncedUpdate msg_ ->
            update msg_ { model | updateOnNextFrame = Nothing }

        ToogleSideMenu ->
            ( { model
                | sideMenuOpen = not model.sideMenuOpen
                , animate = not model.animate
                , updateOnNextFrame =
                    -- probably unnecessary
                    if model.sideMenuOpen then
                        Just (Focus "appContainer")

                    else
                        Just (Focus "sideMenu")
              }
            , if not model.sideMenuOpen then
                Task.attempt (always NoOp) (setViewportOf "sideMenu" 0 0)

              else
                Cmd.none
            )

        ImgLoaded src ->
            ( { model | loaded = Set.insert src model.loaded }, Cmd.none )

        PickLanguage lang ->
            ( { model
                | lang = lang
                , openedWidget = Nothing
              }
            , Cmd.none
            )

        OpenLanguagePicker ->
            ( { model
                | openedWidget =
                    if model.openedWidget == Just LanguagePicker then
                        Nothing

                    else
                        Just LanguagePicker
              }
            , Cmd.none
            )

        Close ->
            ( { model
                | openedWidget = Nothing
              }
            , Cmd.none
            )

        PrevLanguage ->
            let
                prevLanguage =
                    List.Extra.findIndex (\l -> l == model.lang) languages
                        |> Maybe.andThen
                            (\i ->
                                List.Extra.getAt
                                    (modBy (List.length languages) (i - 1))
                                    languages
                            )
                        |> Maybe.withDefault model.lang
            in
            ( { model | lang = prevLanguage }, Cmd.none )

        NextLanguage ->
            let
                nextLanguage =
                    List.Extra.findIndex (\l -> l == model.lang) languages
                        |> Maybe.andThen
                            (\i ->
                                List.Extra.getAt
                                    (modBy (List.length languages) (i - 1))
                                    languages
                            )
                        |> Maybe.withDefault model.lang
            in
            ( { model | lang = nextLanguage }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


scrollTo model id =
    scrollToWithOptions
        { defaultConfig
            | target = Just "appContainer"
            , offset =
                if id == "top" then
                    headerHeight model + mainMenuHeight model

                else
                    mainMenuHeight model
        }
        id



--# ***** Update ***** #--
--# ****************** #--
--# ************************** #--
--# ***** View functions ***** #--


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
                    , htmlAttribute <| HtmlAttr.id "$$top"
                    ]
                    Element.none
                , Dict.get
                    model.url.path
                    (content model)
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
            [ width (px model.width)
            , height (px model.height)
            ]
            [ html <|
                Html.img
                    [ HtmlAttr.hidden True
                    , HtmlEvents.on "load" (D.succeed (ImgLoaded src))
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
            46

        BigDesktop ->
            46


mainMenuBackgroundColor =
    --錆浅葱
    Element.rgb255 106 127 122


headerBackgroundColor =
    --浅葱色
    Element.rgb255 72 146 155


menuItems =
    [ Internal "/" (MlStr "Home" "ホーム") (Just "top")
    , Internal "/about" (MlStr "About" "春日江リトリートとは？") (Just "top")
    , Internal "/contact" (MlStr "Contact" "お問い合わせ") (Just "top")
    , Internal "/member" (MlStr "Member area" "メンバー・エリア") (Just "top")
    ]


mainMenuView model =
    let
        itemLenght =
            max 100
                (round <|
                    toFloat
                        (model.width
                            - model.scrollbarWidth
                            - (if model.headerVisible then
                                0

                               else
                                105
                              )
                        )
                        / toFloat (List.length menuItems)
                )

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
                        , padding 15
                        , pointer
                        , htmlAttribute <| HtmlAttr.style "transition" "background-color 0.3s ease, width 0.3   s ease"
                        , mouseOver
                            [ Background.color
                                (if isMobile then
                                    col grey

                                 else
                                    col white
                                )
                            , Font.color (col black)
                            ]
                        ]
            in
            case itemLink of
                Internal path label mbAnchor ->
                    link
                        ((if isMobile then
                            Background.color
                                (if path == model.url.path then
                                    col grey

                                 else
                                    col white
                                )

                          else
                            Font.color
                                (if path == model.url.path then
                                    col white

                                 else
                                    col black
                                )
                         )
                            :: itemStyle
                        )
                        { url =
                            UrlBuilder.custom
                                Relative
                                (String.split "/" path)
                                []
                                mbAnchor
                        , label = el [ centerX ] (textM model.lang label)
                        }

                External externalUrl label ->
                    newTabLink
                        itemStyle
                        { url = externalUrl
                        , label = el [ centerX ] (textM model.lang label)
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

        logoView isMobile =
            let
                referenceHeight =
                    if isMobile then
                        mainMenuHeight model

                    else
                        headerHeight model
            in
            link
                [ pointer
                ]
                { url =
                    UrlBuilder.custom
                        Relative
                        (String.split "/" "/")
                        []
                        (Just "top")
                , label =
                    row
                        [ spacing 15
                        ]
                        [ el
                            [ width (px <| round ((toFloat <| referenceHeight) * 1.1696))
                            , height (px <| referenceHeight)

                            --, Background.uncropped "/assets/images/artwork/logo.svg"
                            , Background.uncropped <| "https://via.placeholder.com/" ++ String.fromInt referenceHeight
                            ]
                            Element.none
                        , column
                            [ Font.size
                                (if isMobile then
                                    22

                                 else
                                    45
                                )
                            , Font.family
                                [ Font.typeface ""
                                , Font.sansSerif
                                ]
                            ]
                            (if isMobile then
                                [ el [ centerX ] (textM model.lang (MlStr "My" ""))
                                , el [ centerX ] (textM model.lang (MlStr "static webapp" ""))
                                ]

                             else
                                [ textM model.lang (MlStr "My static webapp" "") ]
                            )
                        ]
                }

        mobileView =
            column
                [ width fill ]
                [ row
                    [ width fill
                    , height (px <| mainMenuHeight model)
                    , Background.color headerBackgroundColor
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
                            [ el
                                [ width fill
                                , height (px <| model.height - (headerHeight model + mainMenuHeight model))
                                , scrollbarY
                                , htmlAttribute <| HtmlAttr.id "sideMenu"
                                , spacing 30
                                ]
                                -- for some reason the padding can't be in the sideMenu container
                                -- or it does not work in safari ios
                                (column
                                    [ width fill
                                    , spacing 30
                                    , paddingXY 0 15
                                    ]
                                    [ mobileLanguagePickerView model.lang
                                    , column [ width fill ]
                                        (List.map (itemView True) menuItems)
                                    ]
                                )
                            ]
                        )
                    ]
                    [ el
                        [ alignLeft
                        , paddingXY 15 0
                        ]
                        (logoView True)
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
                , inFront <|
                    el [ alignRight ]
                        (languagePickerView
                            (model.openedWidget == Just LanguagePicker)
                            (mainMenuHeight model)
                            model.lang
                        )
                ]
                [ el
                    [ Background.color headerBackgroundColor
                    , width fill
                    , height <|
                        if model.headerVisible then
                            px <| headerHeight model

                        else
                            px 0
                    , htmlAttribute <| HtmlAttr.style "transition" "height 0.3s"
                    , clip
                    ]
                    (el
                        [ centerX
                        ]
                        (logoView False)
                    )
                , row
                    [ width fill
                    , height (px <| mainMenuHeight model)
                    , Background.color mainMenuBackgroundColor
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
-- Language picker View


flagIcon code =
    Countries.fromCode code
        |> Maybe.map .flag
        |> Maybe.withDefault ""


languageView width_ lang =
    case lang of
        English ->
            row
                [ width_
                , centerY
                ]
                [ text "English"
                , el
                    [ alignRight ]
                    (text <| flagIcon "GB")
                ]

        Japanese ->
            row
                [ width_
                , centerY
                ]
                [ text "日本語"
                , el
                    [ alignRight ]
                    (text <| flagIcon "JP")
                ]


mobileLanguagePickerView : Language -> Element Msg
mobileLanguagePickerView currentLang =
    row
        [ width fill ]
        [ el
            [ onClick PrevLanguage ]
            (viewIcon
                Ionicon.Ios.arrowBack
                40
                grey
            )
        , el
            [ width fill
            ]
            (el [ centerX ]
                (languageView (width (px 85)) currentLang)
            )
        , el
            [ onClick NextLanguage

            --, centerY
            --, alignRight
            ]
            (viewIcon
                Ionicon.Ios.arrowForward
                40
                grey
            )
        ]


languagePickerView : Bool -> Int -> Language -> Element Msg
languagePickerView isOpen itemHeight currentLang =
    let
        pickerWidth =
            85

        selectView =
            column
                [ Background.color mainMenuBackgroundColor
                ]
                (List.map itemView (List.Extra.remove currentLang languages))

        itemView lang =
            el
                [ width fill
                , height (px itemHeight)
                , paddingXY 15 0
                , onClick (PickLanguage lang)
                , pointer
                , htmlAttribute <| HtmlAttr.style "transition" "background-color 0.3s ease"
                , mouseOver
                    [ Background.color (col white) ]
                ]
                (languageView (width (px pickerWidth)) lang)

        buttonStyle =
            [ focused [ Border.glow (Element.rgb 1 1 1) 0 ]
            ]
    in
    el
        [ if isOpen then
            below selectView

          else
            noAttr
        , htmlAttribute <| HtmlAttr.id "languagePicker"
        ]
        (Input.button
            buttonStyle
            { onPress =
                Just OpenLanguagePicker
            , label =
                el
                    [ paddingXY 15 0
                    , height (px itemHeight)
                    , htmlAttribute <| HtmlAttr.style "transition" "background-color 0.3s ease"
                    , mouseOver
                        [ Background.color (col white) ]
                    ]
                    (languageView (width (px pickerWidth)) currentLang)
            }
        )



-------------------------------------------------------------------------------
--footerView


footerView model =
    let
        isMobile_ =
            model.device.class == Phone || model.device.class == Tablet

        itemView isMobile itemLink =
            let
                itemStyle =
                    if isMobile then
                        [ width fill
                        , padding 7
                        , Font.center
                        ]

                    else
                        [ pointer
                        , htmlAttribute <| HtmlAttr.style "transition" "background-color 0.3s ease, width 0.3   s ease"
                        , mouseOver
                            [ Font.color (col white)
                            ]
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
                        , label = el [ centerX ] (textM model.lang label)
                        }

                External externalUrl label ->
                    newTabLink
                        itemStyle
                        { url = externalUrl
                        , label = el [ centerX ] (textM model.lang label)
                        }

        referenceHeight =
            150
    in
    column
        [ width fill
        , Background.color
            --浅葱色
            (Element.rgb255 72 146 155)
        , paddingXY 0 15
        , spacing 30
        ]
        [ (if isMobile_ then
            column
                [ centerX
                ]

           else
            row
                [ spacing 30
                , centerX
                ]
          )
            (List.map (itemView isMobile_) menuItems)
        , row
            [ spacing 15
            , centerX
            ]
            [ textM model.lang (MlStr "Contact number:" "お問い合わせ電話番号:")
            , text "075497483543"
            ]
        , if isMobile_ then
            Element.none

          else
            el
                [ width (px <| round ((toFloat <| referenceHeight) * 1.1696))
                , height (px <| referenceHeight)

                --, Background.uncropped "/assets/images/artwork/logoW.svg"
                , Background.uncropped <| "https://via.placeholder.com/" ++ String.fromInt referenceHeight
                , centerX
                ]
                Element.none
        ]



--# ***** View functions ***** #--
--# ************************** #--
-------------------------------------------------------------------------------
--# ******************* #--
--# ***** Content ***** #--


homePageBlockRow model meta =
    chunkedRows
        (min model.width 1000)
        20
        (bestFit 300 20)
        (List.map (homePageBlockView model) meta)


homePageBlockView model block =
    el [ paddingXY 0 0 ]
        (link
            [ htmlAttribute <| HtmlAttr.style "transition" "background-color 0.3s ease"
            , mouseOver
                [ Background.color (colA white 0.8)
                ]
            , Border.rounded 10
            ]
            { url =
                UrlBuilder.custom
                    Relative
                    (String.split "/" block.path)
                    []
                    block.anchor
            , label =
                row
                    [ width (px 250)
                    , height (px 150)
                    , Border.rounded 10
                    , Background.color (colA white 0.6)
                    , centerX
                    , centerY
                    ]
                    [ el
                        [ width (fillPortion 1)
                        , height fill
                        , Background.image block.pic
                        , Border.roundEach
                            { topLeft = 10
                            , topRight = 0
                            , bottomLeft = 10
                            , bottomRight = 100
                            }
                        ]
                        Element.none
                    , el
                        [ width (fillPortion 2)
                        , height fill
                        ]
                        (paragraph [ centerY, Font.center ]
                            [ textM model.lang <| mlsMap toSentenceCase block.label ]
                        )
                    ]
            }
        )


content : Model -> Dict String (Element Msg)
content model =
    Dict.fromList
        [ ( "/"
          , column
                [ width fill
                ]
                [ column
                    [ width fill
                    , spacing 20
                    , paddingXY 0 20
                    , height (minimum (model.height - headerHeight model - mainMenuHeight model - 30) fill)
                    ]
                    (homePageBlockRow model
                        [ { label = MlStr "Link 1" ""
                          , path = "/about#"
                          , anchor = Nothing
                          , pic = "/assets/images/thumbs/pic1.jpg"
                          }
                        , { label = MlStr "Link 2" ""
                          , path = "/about#"
                          , anchor = Nothing
                          , pic = "/assets/images/thumbs/pic2.jpg"
                          }
                        ]
                    )
                , defaultContainer ""
                    [ el
                        [ Font.bold
                        , Font.size 22
                        ]
                        (textM model.lang (MlStr "Title" "タイトル"))
                    , paragraph
                        []
                        [ textM model.lang
                            (MlStr "Proin vitae lobortis leo. Maecenas sed rhoncus mi, at lobortis augue. Sed sollicitudin libero non varius aliquet. Quisque eget euismod ligula, sodales tristique nunc. Maecenas diam leo, pulvinar quis lobortis at, rutrum at turpis. Curabitur ac lorem vitae tellus rhoncus finibus vitae in arcu. Maecenas eleifend diam ut interdum rutrum. Ut eget pharetra dui. Quisque eget nibh sit amet mauris tincidunt dapibus at at diam. Nunc euismod leo ligula, eget mattis mi porttitor eu. Quisque nec justo at augue cursus cursus. Sed odio turpis, laoreet nec eleifend nec, venenatis eu enim. Nullam a felis dolor. Phasellus varius ultrices dui vulputate dictum. Integer magna arcu, porta eget orci ut, rhoncus consectetur turpis. Integer sodales tortor urna, eu maximus ipsum ullamcorper et."
                                "民どぐ協8回く相面ン便社びす都嘉エ発両じ線東サコキリ長井在モサセケ草改免トフ投的きかトれ大展ゅよ帰軽抗はろ。般カニヒ大的ーだは紙無ぽフ新定官62情緒ルロヒ未愛マムレ水呆クム集別ね党強ゃ刈短せ年北びン。田キコ能局コタ比竹リヌロ童事徳田化然コヲナヒ話97堂れぎへに他業ばイ問外ニハヤ原竹ニキマ使兵ぼ突社よぐまレ平4人も脳国たで告林ムイ都用府べ。"
                            )
                        ]
                    , paragraph
                        []
                        [ textM model.lang
                            (MlStr "Sed lectus arcu, pellentesque ut interdum eu, pharetra et neque. Etiam tempor neque ut nulla aliquet maximus aliquam vitae eros. Suspendisse rhoncus ipsum vitae ex suscipit, eu mollis diam imperdiet. Ut eu tortor hendrerit nisi sollicitudin auctor quis a est. Sed a volutpat elit. Donec et nunc vitae dolor consequat cursus. Cras eget ante vel tortor tempus blandit quis vel arcu. "
                                "真よル田保フシリ像87国ゆにえ面載ッーけ康話ムソオシ支続ルキヨ議護ヒイツ析条ワヱキ開側ゃぎぱ社月すうドス経協きーフ優壊筆研くフド。名セ超気かえー敗図石カヘヒモ由芸伴げッづぽ含止条ムレ約面ゃ目絶書トヲウニ減情リ葉高度づや粧断ッさ読覧ひくゆな舎督ナハ体昭ふづむね的首せくゆ機護検帯でひわー。金しま約94空提べな描馬ソ熊荷ワユ市割クホ住傳チ力4約庭なぼる速18尚徒阿わえが。"
                            )
                        ]
                    , showImages model
                        [ { url = "/assets/images/regular/pic1.jpg"
                          , caption = Nothing
                          , size = { height = 450, width = 600 }
                          }
                        , { url = "/assets/images/regular/pic2.jpg"
                          , caption = Nothing
                          , size = { height = 450, width = 600 }
                          }
                        , { url = "/assets/images/regular/pic3.jpg"
                          , caption = Nothing
                          , size = { height = 450, width = 600 }
                          }
                        ]
                    ]
                , el
                    [ height (px 200)
                    ]
                    Element.none
                ]
          )
        , ( "/about"
          , column
                [ width fill ]
                [ column
                    [ width fill
                    , spacing 20
                    , height (minimum (model.height - headerHeight model - mainMenuHeight model - 30) fill)
                    ]
                    []
                ]
          )
        , ( "/contact"
          , column
                [ width fill ]
                [ column
                    [ width fill
                    , spacing 20
                    , height (minimum (model.height - headerHeight model - mainMenuHeight model - 30) fill)
                    ]
                    []
                ]
          )
        ]



--# ***** Content ***** #--
--# ******************* #--
-------------------------------------------------------------------------------
--# ************************ #--
--# ***** Misc helpers ***** #--


defaultwidth_ =
    1000


defaultwidth =
    width (maximum defaultwidth_ fill)


defaultContainerStyle =
    [ defaultwidth
    , centerX
    , padding 15
    , spacing 15

    --, Background.tiled "/assets/images/background/namiT.png"
    ]


defaultContainer : String -> List (Element Msg) -> Element Msg
defaultContainer id contents =
    el
        [ width fill
        , padding 15
        , Background.color (col white)
        , htmlAttribute <| HtmlAttr.id ("$$" ++ id)

        --, Background.color (col white)
        ]
        (column
            defaultContainerStyle
            contents
        )


noAttr =
    htmlAttribute <| HtmlAttr.class ""


sides =
    { top = 0, left = 0, right = 0, bottom = 0 }



--# ***** Misc helpers ***** #--
--# ************************ #--
-------------------------------------------------------------------------------
--# ******************************** #--
--# ***** Icons View functions ***** #--


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



--# ***** Icons View functions ***** #--
--# ******************************** #--
-------------------------------------------------------------------------------
--# *************************** #--
--# ***** Color functions ***** #--


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



--# ***** Color functions ***** #--
--# *************************** #--
-------------------------------------------------------------------------------
--# ************************* #--
--# ***** Chunked rows  ***** #--


chunkedRows : Int -> Int -> (Int -> Int) -> List (Element msg) -> List (Element msg)
chunkedRows width spacing_ chunkBy elems =
    let
        nbrChunks =
            chunkBy width

        chunks =
            chunklist nbrChunks elems
    in
    List.map
        (row
            [ centerX
            , centerY
            , spacing spacing_
            ]
        )
        chunks


chunklist : Int -> List a -> List (List a)
chunklist n xs =
    let
        helper acc ys =
            case ys of
                [] ->
                    List.reverse acc

                _ ->
                    helper (List.take n ys :: acc) (List.drop n ys)
    in
    helper [] xs


bestFit : Int -> Int -> Int -> Int
bestFit elemWidth spacing_ width =
    let
        nbrChunks_ =
            width // elemWidth

        spacing =
            (nbrChunks_ - 1) * spacing_
    in
    if (nbrChunks_ * elemWidth + spacing) < width then
        nbrChunks_

    else
        nbrChunks_ - 1



--# ***** Chunked rows  ***** #--
--# ************************* #--
-------------------------------------------------------------------------------
--# ********************************* #--
--# ***** Same height image row ***** #--


type alias ImageMeta =
    { url : String
    , caption : Maybe String
    , size : { height : Int, width : Int }
    }


showImages model images =
    if model.width < defaultwidth_ then
        column
            [ spacing 15
            , centerX
            ]
            (List.map
                (\{ url, caption } ->
                    image
                        [ width (maximum model.width fill) ]
                        { src = url
                        , description =
                            Maybe.withDefault "" caption
                        }
                )
                images
            )

    else
        sameHeightImgRow images


sameHeightImgRow : List ImageMeta -> Element msg
sameHeightImgRow images =
    let
        images_ =
            List.map
                (\meta ->
                    { meta = meta
                    , newWidth = 0
                    , newHeight = 0
                    }
                )
                images

        imgSizes imgs =
            List.map (\i -> i.meta.size) imgs

        minHeight imgs =
            imgSizes imgs
                |> List.map .height
                |> List.sort
                |> List.head
                |> Maybe.withDefault 0

        imgsScaledToMinHeight =
            let
                mh =
                    minHeight images_

                scale { meta } =
                    { meta = meta
                    , newHeight = toFloat mh + 5
                    , newWidth =
                        toFloat mh
                            * toFloat meta.size.width
                            / toFloat meta.size.height
                    }
            in
            List.map scale images_

        totalImgWidth =
            List.foldr (\i n -> i.newWidth + n) 0 imgsScaledToMinHeight
    in
    Keyed.row
        [ spacing 15
        ]
        (List.indexedMap
            (\i im ->
                ( String.fromInt (i * List.length imgsScaledToMinHeight)
                , el
                    [ width <| fillPortion (floor <| 10000 * im.newWidth / totalImgWidth)
                    ]
                    (html <|
                        Html.img
                            [ HtmlAttr.style "width" "100%"
                            , HtmlAttr.style "height" "auto"
                            , HtmlAttr.src im.meta.url
                            ]
                            []
                    )
                )
            )
            imgsScaledToMinHeight
        )



--# ***** Same height image row ***** #--
--# ********************************* #--
-------------------------------------------------------------------------------
--# ********************************* #--
--# ***** Outside click decoder ***** #--


outsideTargetHandler : String -> msg -> D.Decoder msg
outsideTargetHandler targetId handler =
    D.field "target" (isOutsideTarget targetId)
        |> D.andThen
            (\isOutside ->
                if isOutside then
                    D.succeed handler

                else
                    D.fail "inside target"
            )


isOutsideTarget targetId =
    D.oneOf
        [ D.field "id" D.string
            |> D.andThen
                (\id ->
                    if targetId == id then
                        -- found match by id
                        D.succeed False

                    else
                        -- try next decoder
                        D.fail "continue"
                )
        , D.lazy (\_ -> D.field "parentNode" (isOutsideTarget targetId))

        -- fallback if all previous decoders failed
        , D.succeed True
        ]



--# ***** Outside click decoder ***** #--
--# ********************************* #--
