module FractalTreeGenerator.Generator exposing (main)

{-| A fractal tree generator with user controls.
-}

import Browser
import Browser.Events exposing (onResize)
import FractalTreeGenerator.Types exposing (..)
import FractalTreeGenerator.View as View
import Random



--------------------------------------------------------------------------------
-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



--------------------------------------------------------------------------------
-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        levels =
            10

        zeros =
            List.repeat levels 0

        model =
            { windowW = flags.windowWidth
            , windowH = flags.windowHeight
            , w = flags.windowWidth * 0.9
            , h = flags.windowHeight * 0.8
            , levels = levels
            , rightAngle = 15
            , leftAngle = 15
            , randomizeAngles = False
            , baseWidth = 20
            , widthNarrowing = 0.8
            , randomizeWidthNarrowing = False
            , baseHeight = flags.windowHeight * 0.8 * 0.2
            , heightShortening = 0.75
            , randomizeHeightShortening = False
            , tree = View.emptyTree
            }
    in
    ( model, genTree model )



--------------------------------------------------------------------------------
-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLevels n ->
            let
                model_ =
                    { model | levels = n }
            in
            ( model_, genTree model_ )

        UpdateLeftAngle n ->
            let
                model_ =
                    { model | leftAngle = n }
            in
            ( model_, genTree model_ )

        UpdateRightAngle n ->
            let
                model_ =
                    { model | rightAngle = n }
            in
            ( model_, genTree model_ )

        UpdateRandomizeAngles b ->
            let
                model_ =
                    { model | randomizeAngles = b }
            in
            ( model_, genTree model_ )

        UpdateBaseWidth n ->
            let
                model_ =
                    { model | baseWidth = n }
            in
            ( model_, genTree model_ )

        UpdateWidthNarrowing n ->
            let
                model_ =
                    { model | widthNarrowing = n }
            in
            ( model_, genTree model_ )

        UpdateRandomizeWidthNarrowing b ->
            let
                model_ =
                    { model | randomizeWidthNarrowing = b }
            in
            ( model_, genTree model_ )

        UpdateBaseHeight n ->
            let
                model_ =
                    { model | baseHeight = n }
            in
            ( model_, genTree model_ )

        UpdateHeightShortening n ->
            let
                model_ =
                    { model | heightShortening = n }
            in
            ( model_, genTree model_ )

        UpdateRandomizeHeightShortening b ->
            let
                model_ =
                    { model | randomizeHeightShortening = b }
            in
            ( model_, genTree model_ )

        WindowResize ( w, h ) ->
            let
                ( w_, h_ ) =
                    ( toFloat w, toFloat h )

                model_ =
                    { model
                        | windowW = w_
                        , windowH = h_
                        , w = model.w * w_ / model.windowW
                        , h = model.h * h_ / model.windowH
                        , baseHeight = model.baseHeight * h_ / model.windowH
                    }
            in
            ( model_, genTree model_ )

        GenTree t ->
            ( { model | tree = t }, Cmd.none )


genTree : Model -> Cmd Msg
genTree model =
    Random.generate GenTree (View.render model)



--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResize ( w, h ))
        ]
