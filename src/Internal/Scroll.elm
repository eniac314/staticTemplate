module Internal.Scroll exposing
    ( Config
    , defaultConfig
    , scrollTo
    , scrollToWithOptions
    )

{-|


# Config

@docs Config
@docs defaultConfig


# Scrolling

@docs scrollTo
@docs scrollToWithOptions

-}

import Browser.Dom as Dom
import Ease
import Task exposing (Task)


{-| Configuration options for smooth scrolling. Has three options:

  - offset: The amount of space in pixels between the element to scroll to and the top of the viewport that is to remain after scrolling
  - speed: The higher this number, the faster the scrolling!
  - easing: The easing function to use. Check out the [easing functions](https://package.elm-lang.org/packages/elm-community/easing-functions/latest/) package for more information.

-}
type alias Config =
    { offset : Int
    , speed : Int
    , easing : Ease.Easing
    , target : Maybe String
    }


{-|

    import SmoothScroll

    defaultConfig : Config
    defaultConfig =
        { offset = 12
        , speed = 200
        , easing = Ease.outQuint
        }

-}
defaultConfig : Config
defaultConfig =
    { offset = 12
    , speed = 50
    , easing = Ease.outQuint
    , target = Nothing
    }


{-| Scroll to the element with the given id, using the default configuration

    import SmoothScroll

    scrollTo "article"

-}
scrollTo : String -> Task Dom.Error (List ())
scrollTo =
    scrollToWithOptions defaultConfig


{-| Scroll to the element with the given id, using a custom configuration

    import SmoothScroll exposing (defaultConfig)

    scrollToWithOptions { defaultConfig | offset = 60 } "article"

-}
scrollToWithOptions : Config -> String -> Task Dom.Error (List ())
scrollToWithOptions config id =
    let
        ( getter, setter ) =
            case config.target of
                Just targetId ->
                    ( Dom.getViewportOf targetId, Dom.setViewportOf targetId )

                Nothing ->
                    ( Dom.getViewport, Dom.setViewport )

        tasks from to =
            List.map (setter 0)
                (animationSteps config.speed config.easing from (to - toFloat config.offset))
                |> Task.sequence
    in
    Task.map2 Tuple.pair getter (Dom.getElement id)
        |> Task.andThen
            (\( { viewport }, { element } ) ->
                let
                    yOffset =
                        case config.target of
                            Nothing ->
                                0

                            Just _ ->
                                viewport.y
                in
                tasks viewport.y (yOffset + element.y)
            )


animationSteps : Int -> Ease.Easing -> Float -> Float -> List Float
animationSteps speed easing start stop =
    let
        diff =
            abs <| start - stop

        frames =
            Basics.max 1 <| round diff // speed

        framesFloat =
            toFloat frames

        weights =
            List.map (\i -> easing (toFloat i / framesFloat)) (List.range 0 frames)

        operator =
            if start > stop then
                (-)

            else
                (+)
    in
    if speed <= 0 || start == stop then
        []

    else
        List.map (\weight -> operator start (weight * diff)) weights
