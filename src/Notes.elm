module Notes exposing (Note(..), step)


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


step : Note -> Int -> Note
step note halfSteps =
    let
        normalizedHalfSteps =
            remainderBy 12 halfSteps
    in
    if normalizedHalfSteps > 0 then
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
        step nextNote (normalizedHalfSteps - 1)

    else if normalizedHalfSteps < 0 then
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
        step nextNote (normalizedHalfSteps + 1)

    else
        note
