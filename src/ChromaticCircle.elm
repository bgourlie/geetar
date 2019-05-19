module ChromaticCircle exposing (chromaticCircle)

import Notes exposing (Note(..))
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
        , notePoint DE origin radius percent
        , noteLabel DE origin radius percent
        , notePoint E origin radius percent
        , noteLabel E origin radius percent
        , notePoint F origin radius percent
        , noteLabel F origin radius percent
        , notePoint FG origin radius percent
        , noteLabel FG origin radius percent
        , notePoint G origin radius percent
        , noteLabel G origin radius percent
        , notePoint GA origin radius percent
        , noteLabel GA origin radius percent
        , notePoint A origin radius percent
        , noteLabel A origin radius percent
        , notePoint AB origin radius percent
        , noteLabel AB origin radius percent
        , notePoint B origin radius percent
        , noteLabel B origin radius percent
        , notePoint C origin radius percent
        , noteLabel C origin radius percent
        , notePoint CD origin radius percent
        , noteLabel CD origin radius percent
        , notePoint D origin radius percent
        , noteLabel D origin radius percent
        ]


noteDegree : Note -> Float
noteDegree note =
    case note of
        A ->
            180.0

        AB ->
            210.0

        B ->
            240.0

        C ->
            270.0

        CD ->
            300.0

        D ->
            330.0

        DE ->
            0.0

        E ->
            30.0

        F ->
            60.0

        FG ->
            90.0

        G ->
            120.0

        GA ->
            150.0


notePointCoords : Note -> Float -> ( Float, Float )
notePointCoords note radius =
    let
        degree =
            noteDegree note
    in
    ( radius * cos (degrees degree), radius * sin (degrees degree) )


notePoint : Note -> Float -> Float -> Float -> Svg msg
notePoint note origin radius percent =
    let
        ( noteX, noteY ) =
            notePointCoords note radius
    in
    circle
        [ cx <| String.fromFloat (noteX + origin)
        , cy <| String.fromFloat (noteY + origin)
        , r <| String.fromFloat (1.0 * percent)
        ]
        []


noteLabel : Note -> Float -> Float -> Float -> Svg msg
noteLabel note origin radius percent =
    let
        textRadius =
            radius + percent * 3

        ( noteX, noteY ) =
            notePointCoords note textRadius

        pointX =
            String.fromFloat <| noteX + origin

        pointY =
            String.fromFloat <| noteY + origin
    in
    text_
        [ x pointX
        , y pointY
        , textAnchor "middle"
        , fontSize <| String.fromFloat (percent * 4)
        , transform <| "rotate(" ++ String.fromFloat (noteDegree note + 90) ++ "," ++ pointX ++ "," ++ pointY ++ ")"
        ]
        [ text <| Notes.toString note ]
