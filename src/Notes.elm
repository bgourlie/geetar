module Notes exposing (Note(..), Scale, majorScale, scale, step, toString)


type Note
    = A
    | AB
    | B
    | C
    | CD
    | D
    | DE
    | E
    | F
    | FG
    | G
    | GA


type Tone
    = SemiTone
    | WholeTone


type alias ScaleSteps =
    { first : Tone
    , second : Tone
    , third : Tone
    , fourth : Tone
    , fifth : Tone
    , sixth : Tone
    , seventh : Tone
    }


type alias Scale =
    { first : Note
    , second : Note
    , third : Note
    , fourth : Note
    , fifth : Note
    , sixth : Note
    , seventh : Note
    , eigth : Note
    }


toString : Note -> String
toString note =
    case note of
        A ->
            "A"

        AB ->
            "A♯ / B♭"

        B ->
            "B"

        C ->
            "C"

        CD ->
            "C♯ / D♭"

        D ->
            "D"

        DE ->
            "D♯ / E♭"

        E ->
            "E"

        F ->
            "F"

        FG ->
            "F♯ / D♭"

        G ->
            "G"

        GA ->
            "G♯ / A♭"


mapToneToSemiTones : Tone -> Int
mapToneToSemiTones tone =
    case tone of
        SemiTone ->
            1

        WholeTone ->
            2


majorScale : ScaleSteps
majorScale =
    { first = WholeTone
    , second = WholeTone
    , third = SemiTone
    , fourth = WholeTone
    , fifth = WholeTone
    , sixth = WholeTone
    , seventh = SemiTone
    }


scale : Note -> ScaleSteps -> Scale
scale rootNote { first, second, third, fourth, fifth, sixth, seventh } =
    let
        stepOne =
            mapToneToSemiTones first

        stepTwo =
            stepOne + mapToneToSemiTones second

        stepThree =
            stepTwo + mapToneToSemiTones third

        stepFour =
            stepThree + mapToneToSemiTones fourth

        stepFive =
            stepFour + mapToneToSemiTones fifth

        stepSix =
            stepFive + mapToneToSemiTones sixth

        stepSeven =
            stepSix + mapToneToSemiTones seventh
    in
    { first = rootNote
    , second = step rootNote stepOne
    , third = step rootNote stepTwo
    , fourth = step rootNote stepThree
    , fifth = step rootNote stepFour
    , sixth = step rootNote stepFive
    , seventh = step rootNote stepSix
    , eigth = step rootNote stepSeven
    }


step : Note -> Int -> Note
step note semiTones =
    let
        normalizedSemiTones =
            remainderBy 12 semiTones
    in
    if normalizedSemiTones > 0 then
        let
            nextNote =
                case note of
                    A ->
                        AB

                    AB ->
                        B

                    B ->
                        C

                    C ->
                        CD

                    CD ->
                        D

                    D ->
                        DE

                    DE ->
                        E

                    E ->
                        F

                    F ->
                        FG

                    FG ->
                        G

                    G ->
                        GA

                    GA ->
                        A
        in
        step nextNote (normalizedSemiTones - 1)

    else if normalizedSemiTones < 0 then
        let
            nextNote =
                case note of
                    AB ->
                        A

                    B ->
                        AB

                    C ->
                        B

                    CD ->
                        C

                    D ->
                        CD

                    DE ->
                        D

                    E ->
                        DE

                    F ->
                        E

                    FG ->
                        F

                    G ->
                        FG

                    GA ->
                        G

                    A ->
                        GA
        in
        step nextNote (normalizedSemiTones + 1)

    else
        note
