module Tests exposing (suite)

import Expect exposing (Expectation)
import Notes exposing (Note(..), majorScale, scale)
import Test exposing (..)


suite : Test
suite =
    describe "The Notes module"
        [ describe "Positive semitones"
            [ test "Semitone from A" <|
                \_ ->
                    Expect.equal AB (Notes.step A 1)
            , test "Semitone from AB" <|
                \_ ->
                    Expect.equal B (Notes.step AB 1)
            , test "Semitone from B" <|
                \_ ->
                    Expect.equal C (Notes.step B 1)
            , test "Semitone from C" <|
                \_ ->
                    Expect.equal CD (Notes.step C 1)
            , test "Semitone from CD" <|
                \_ ->
                    Expect.equal D (Notes.step CD 1)
            , test "Semitone from D" <|
                \_ ->
                    Expect.equal DE (Notes.step D 1)
            , test "Semitone from DE" <|
                \_ ->
                    Expect.equal E (Notes.step DE 1)
            , test "Semitone from E" <|
                \_ ->
                    Expect.equal F (Notes.step E 1)
            , test "Semitone from F" <|
                \_ ->
                    Expect.equal FG (Notes.step F 1)
            , test "Semitone from FG" <|
                \_ ->
                    Expect.equal G (Notes.step FG 1)
            , test "Semitone from G" <|
                \_ ->
                    Expect.equal GA (Notes.step G 1)
            , test "Semitone from GA" <|
                \_ ->
                    Expect.equal A (Notes.step GA 1)
            , test "Interval from GA" <|
                \_ ->
                    Expect.equal GA (Notes.step GA 12)
            , test "13 semitones GA" <|
                \_ ->
                    Expect.equal A (Notes.step GA 13)
            ]
        , describe "Negative steps"
            [ test "Semitone from A" <|
                \_ ->
                    Expect.equal GA (Notes.step A -1)
            , test "Semitone from GA" <|
                \_ ->
                    Expect.equal G (Notes.step GA -1)
            , test "Semitone from G" <|
                \_ ->
                    Expect.equal FG (Notes.step G -1)
            , test "Semitone from FG" <|
                \_ ->
                    Expect.equal F (Notes.step FG -1)
            , test "Semitone from F" <|
                \_ ->
                    Expect.equal E (Notes.step F -1)
            , test "Semitone from E" <|
                \_ ->
                    Expect.equal DE (Notes.step E -1)
            , test "Semitone from DE" <|
                \_ ->
                    Expect.equal D (Notes.step DE -1)
            , test "Semitone from D" <|
                \_ ->
                    Expect.equal CD (Notes.step D -1)
            , test "Semitone from CD" <|
                \_ ->
                    Expect.equal C (Notes.step CD -1)
            , test "Semitone from C" <|
                \_ ->
                    Expect.equal B (Notes.step C -1)
            , test "Semitone from B" <|
                \_ ->
                    Expect.equal AB (Notes.step B -1)
            , test "Semitone from AB" <|
                \_ ->
                    Expect.equal A (Notes.step AB -1)
            , test "Interval from A" <|
                \_ ->
                    Expect.equal A (Notes.step A -12)
            , test "13 semitones from A" <|
                \_ ->
                    Expect.equal GA (Notes.step A -13)
            ]
        , describe "Scales"
            [ test "C Major" <|
                \_ ->
                    Expect.equal { first = C, second = D, third = E, fourth = F, fifth = G, sixth = A, seventh = B, eigth = C } (scale C majorScale)
            ]
        ]
