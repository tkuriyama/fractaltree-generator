module FractalTreeGenerator.View exposing (emptyTree, render, view)

{-| View / render fractal trees with controls.
-}

import Color exposing (Color)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FractalTreeGenerator.Types exposing (..)
import Html exposing (Html)
import Random
import TypedSvg exposing (g, line, svg)
import TypedSvg.Attributes
    exposing
        ( class
        , stroke
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx
    exposing
        ( strokeWidth
        , x1
        , x2
        , y1
        , y2
        )
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))



--------------------------------------------------------------------------------
-- View


view : Model -> Html Msg
view model =
    E.layout
        [ Font.family [ Font.typeface "Consolas", Font.sansSerif ]
        , Font.size 18
        , E.padding 10
        ]
        (E.column
            [ E.centerX
            , E.alignTop
            , E.width <|
                E.maximum (round model.windowW) E.fill
            , E.height <|
                E.maximum (round model.windowH) E.fill
            , E.spacing 0
            ]
            [ controls model
            , E.el
                [ E.alignTop
                , E.centerX
                , E.width <| E.px <| round model.w
                , E.height <| E.px <| round model.h
                ]
                (model.tree |> E.html)
            ]
        )


controls : Model -> E.Element Msg
controls model =
    let
        rowAttrs =
            [ E.alignTop
            , E.centerX
            , E.spacing 30
            , E.padding 5
            ]
    in
    E.column
        [ E.alignTop
        , E.centerX
        , E.spacing 10
        ]
        [ E.row
            rowAttrs
            [ levelSlider model.levels
            , angleSlider UpdateLeftAngle "Left Angle" model.leftAngle
            , angleSlider UpdateRightAngle "Right Angle" model.rightAngle
            , randomizeChoice
                UpdateRandomizeAngles
                model.randomizeAngles
                "Branch Angles"
            ]
        , E.row
            rowAttrs
            [ floatSlider
                UpdateBaseWidth
                ( 20, 80 )
                model.baseWidth
                "Base Width"
            , floatSlider
                UpdateWidthNarrowing
                ( 0.5, 0.85 )
                model.widthNarrowing
                "Width Change %"
            , randomizeChoice
                UpdateRandomizeWidthNarrowing
                model.randomizeHeightShortening
                "Width Change %"
            , floatSlider
                UpdateBaseHeight
                ( model.h * 0.15, model.h * 0.25 )
                model.baseHeight
                "Base Height"
            , floatSlider
                UpdateHeightShortening
                ( 0.5, 0.85 )
                model.heightShortening
                "Height Change %"
            , randomizeChoice
                UpdateRandomizeHeightShortening
                model.randomizeHeightShortening
                "Height Change %"
            ]
        ]


levelSlider : Int -> E.Element Msg
levelSlider currentVal =
    Input.slider
        [ E.height <| E.px 30
        , E.behindContent sliderElement
        ]
        { onChange = round >> UpdateLevels
        , label = titleLabel "Tree Levels"
        , min = 2
        , max = 14
        , step = Just (toFloat 1)
        , value = toFloat currentVal
        , thumb =
            Input.defaultThumb
        }


angleSlider : (Float -> Msg) -> String -> Float -> E.Element Msg
angleSlider msg title currentVal =
    Input.slider
        [ E.height <| E.px 30
        , E.behindContent sliderElement
        ]
        { onChange = msg
        , label = titleLabel title
        , min = 5
        , max = 25
        , step = Just 1
        , value = currentVal
        , thumb =
            Input.defaultThumb
        }


randomizeChoice : (Bool -> Msg) -> Bool -> String -> E.Element Msg
randomizeChoice msg selected title =
    Input.radioRow
        [ E.padding 10
        , E.spacing 10
        ]
        { onChange = msg
        , selected = Just selected
        , label = titleLabel <| title
        , options =
            [ Input.option False (E.text "Exact")
            , Input.option True (E.text "Noise")
            ]
        }


floatSlider :
    (Float -> Msg)
    -> ( Float, Float )
    -> Float
    -> String
    -> E.Element Msg
floatSlider msg ( min, max ) currentVal title =
    Input.slider
        [ E.height <| E.px 30
        , E.behindContent sliderElement
        ]
        { onChange = msg
        , label = titleLabel title
        , min = min
        , max = max
        , step = Just ((max - min) / 20)
        , value = currentVal
        , thumb =
            Input.defaultThumb
        }


titleLabel : String -> Input.Label msg
titleLabel s =
    Input.labelAbove
        [ Font.heavy
        , E.centerX
        ]
        (E.text s)


sliderElement : E.Element msg
sliderElement =
    E.el
        [ E.width E.fill
        , E.height <| E.px 10
        , E.centerY
        , Background.color <| E.rgb255 66 135 245
        , Border.rounded 2
        ]
        E.none



--------------------------------------------------------------------------------
-- Render


type alias Renderable a =
    { a
        | leftAngle : Float
        , rightAngle : Float
        , randomizeAngles : Bool
        , baseWidth : Float
        , widthNarrowing : Float
        , randomizeWidthNarrowing : Bool
        , baseHeight : Float
        , heightShortening : Float
        , randomizeHeightShortening : Bool
    }


render : Model -> Random.Generator (Svg Msg)
render model =
    let
        ( x, y ) =
            ( model.w / 2, model.h - 10 )

        ( w, h ) =
            ( model.baseWidth, model.baseHeight )

        toSvg xs =
            svg [ viewBox 0 0 model.w model.h ] xs
    in
    renderBranches model model.levels ( x, y ) ( w, h ) 90
        |> Random.andThen (\xs -> toSvg xs |> Random.constant)


renderBranches :
    Renderable a
    -> Int
    -> ( Float, Float )
    -> ( Float, Float )
    -> Float
    -> Random.Generator (List (Svg Msg))
renderBranches model level ( x, y ) ( w, h ) angle =
    let
        ( x_, y_ ) =
            extend ( x, y ) h angle

        branch =
            renderBranch level ( x, y ) ( x_, y_ ) w
    in
    if level == 0 then
        Random.constant [ svg [] [] ]

    else
        randomFloats model
            |> Random.andThen
                (\triple ->
                    renderNextBranches model level ( x_, y_ ) ( w, h ) angle triple
                )
            |> Random.andThen
                (\branches -> Random.constant (branch :: branches))


renderBranch : Int -> ( Float, Float ) -> ( Float, Float ) -> Float -> Svg Msg
renderBranch level ( x, y ) ( x_, y_ ) w =
    line
        [ x1 x_
        , y1 y_
        , x2 x
        , y2 y
        , strokeWidth w
        , stroke <| Paint <| branchColor level
        ]
        []


renderNextBranches :
    Renderable a
    -> Int
    -> ( Float, Float )
    -> ( Float, Float )
    -> Float
    -> ( Float, Float, Float )
    -> Random.Generator (List (Svg Msg))
renderNextBranches model level ( x, y ) ( w, h ) angle ( rAngle, rWidth, rHeight ) =
    let
        w_ =
            w * model.widthNarrowing * rWidth

        h_ =
            h * model.heightShortening * rHeight

        leftAngle =
            angle - model.leftAngle * rAngle

        rightAngle =
            angle + model.rightAngle * rAngle

        f angle_ =
            renderBranches model (level - 1) ( x, y ) ( w_, h_ ) angle_
    in
    Random.map2 (++) (f leftAngle) (f rightAngle)


extend : ( Float, Float ) -> Float -> Float -> ( Float, Float )
extend ( x, y ) h angle =
    ( x + h * cos (degrees angle)
    , y - h * sin (degrees angle)
    )


branchColor : Int -> Color
branchColor x =
    Color.rgb255 0 (255 / toFloat x |> round) 0



--------------------------------------------------------------------------------
-- Helpers


emptyTree : Svg Msg
emptyTree =
    svg [] []


randomFloats : Renderable a -> Random.Generator ( Float, Float, Float )
randomFloats model =
    let
        triple x y z =
            ( x, y, z )
    in
    Random.map3
        triple
        (randomFloat model.randomizeAngles)
        (randomFloat model.randomizeWidthNarrowing)
        (randomFloat model.randomizeHeightShortening)


randomFloat : Bool -> Random.Generator Float
randomFloat b =
    if b then
        Random.float 0.9 1.1

    else
        Random.constant 1.0
