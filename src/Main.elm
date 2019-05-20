module Main exposing (main)

import Browser exposing (Document)
import ChromaticCircle exposing (chromaticCircle)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, name, type_)
import Html.Styled.Events exposing (onClick)
import Notes exposing (Note(..))



-- MODEL


type alias Model =
    { rootNote : Note }


type Msg
    = NoteSelected Note
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteSelected note ->
            ( { model | rootNote = note }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            ]
        ]
        [ chromaticCircle model.rootNote 600
        , div [ css [ displayFlex, flexDirection column ] ]
            [ radioButton (Notes.toString C) (model.rootNote == C) (NoteSelected C)
            , radioButton (Notes.toString CD) (model.rootNote == CD) (NoteSelected CD)
            , radioButton (Notes.toString D) (model.rootNote == D) (NoteSelected D)
            , radioButton (Notes.toString DE) (model.rootNote == DE) (NoteSelected DE)
            , radioButton (Notes.toString E) (model.rootNote == E) (NoteSelected E)
            , radioButton (Notes.toString F) (model.rootNote == F) (NoteSelected F)
            , radioButton (Notes.toString FG) (model.rootNote == FG) (NoteSelected FG)
            , radioButton (Notes.toString G) (model.rootNote == G) (NoteSelected G)
            , radioButton (Notes.toString GA) (model.rootNote == GA) (NoteSelected GA)
            , radioButton (Notes.toString A) (model.rootNote == A) (NoteSelected A)
            , radioButton (Notes.toString AB) (model.rootNote == AB) (NoteSelected AB)
            , radioButton (Notes.toString B) (model.rootNote == B) (NoteSelected B)
            ]
        ]


radioButton : String -> Bool -> msg -> Html msg
radioButton label_ selected action =
    div
        [ css [ cursor pointer ]
        , onClick action
        ]
        [ text label_ ]



---- PROGRAM ----


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            { rootNote = C }
    in
    ( initialState, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { view =
            \model ->
                { title = "Procedural generation in Elm!", body = [ (view >> Html.Styled.toUnstyled) model ] }
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
