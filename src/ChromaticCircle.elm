module ChromaticCircle exposing (chromaticCircle)

import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)


chromaticCircle : Float -> Svg msg
chromaticCircle size =
    let
        radiusPercent =
            80

        percent =
            size / 100

        origin =
            percent * 50

        radius =
            (percent * radiusPercent) / 2
    in
    svg
        [ width <| String.fromFloat size
        , height <| String.fromFloat size
        , viewBox <| String.join " " [ "0", "0", String.fromFloat size, String.fromFloat size ]
        ]
        [ circle
            [ cx <| String.fromFloat origin
            , cy <| String.fromFloat origin
            , r <| String.fromFloat radius
            , stroke "black"
            , strokeWidth <| String.fromFloat (0.5 * percent)
            , fill "none"
            ]
            []
        , notePoint origin radius 0 percent
        , notePoint origin radius 30 percent
        , notePoint origin radius 60 percent
        , notePoint origin radius 90 percent
        , notePoint origin radius 120 percent
        , notePoint origin radius 150 percent
        , notePoint origin radius 180 percent
        , notePoint origin radius 210 percent
        , notePoint origin radius 240 percent
        , notePoint origin radius 270 percent
        , notePoint origin radius 300 percent
        , notePoint origin radius 330 percent
        , noteLabel "D♯ / E♭" origin radius 0 percent
        , noteLabel "E" origin radius 30 percent
        , noteLabel "F" origin radius 60 percent
        , noteLabel "F♯ / D♭" origin radius 90 percent
        , noteLabel "G" origin radius 120 percent
        , noteLabel "G♯ / A♭" origin radius 150 percent
        , noteLabel "A" origin radius 180 percent
        , noteLabel "A♯ / B♭" origin radius 210 percent
        , noteLabel "B" origin radius 240 percent
        , noteLabel "C" origin radius 270 percent
        , noteLabel "C♯ / D♭" origin radius 300 percent
        , noteLabel "D" origin radius 330 percent
        ]


notePoint : Float -> Float -> Float -> Float -> Svg msg
notePoint origin radius degree percent =
    circle
        [ cx <| String.fromFloat (radius * cos (degrees degree) + origin)
        , cy <| String.fromFloat (radius * sin (degrees degree) + origin)
        , r <| String.fromFloat (1.0 * percent)
        ]
        []


noteLabel : String -> Float -> Float -> Float -> Float -> Svg msg
noteLabel note origin radius degree percent =
    let
        textRadius =
            radius + percent * 3

        pointX =
            String.fromFloat <| textRadius * cos (degrees degree) + origin

        pointY =
            String.fromFloat <| textRadius * sin (degrees degree) + origin
    in
    text_
        [ x pointX
        , y pointY
        , textAnchor "middle"
        , fontSize <| String.fromFloat (percent * 4)
        , transform <| "rotate(" ++ String.fromFloat (degree + 90) ++ "," ++ pointX ++ "," ++ pointY ++ ")"
        ]
        [ text note ]
