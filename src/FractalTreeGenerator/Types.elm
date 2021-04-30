module FractalTreeGenerator.Types exposing (..)

{-| Types for FractalTreeGenerator files.
-}

import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }


type alias Model =
    { windowW : Float
    , windowH : Float
    , w : Float
    , h : Float
    , levels : Int
    , leftAngle : Float
    , rightAngle : Float
    , randomizeAngles : Bool
    , baseWidth : Float
    , widthNarrowing : Float
    , randomizeWidthNarrowing : Bool
    , baseHeight : Float
    , heightShortening : Float
    , randomizeHeightShortening : Bool
    , tree : Svg Msg
    }


type Msg
    = UpdateLevels Int
    | UpdateLeftAngle Float
    | UpdateRightAngle Float
    | UpdateRandomizeAngles Bool
    | UpdateBaseWidth Float
    | UpdateWidthNarrowing Float
    | UpdateRandomizeWidthNarrowing Bool
    | UpdateBaseHeight Float
    | UpdateHeightShortening Float
    | UpdateRandomizeHeightShortening Bool
    | WindowResize ( Int, Int )
    | GenTree (Svg Msg)
