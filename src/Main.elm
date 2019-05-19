module Main exposing (main)

import Browser exposing (Document)
import ChromaticCircle exposing (chromaticCircle)
import Html.Styled exposing (Html)



-- MODEL


type alias Model =
    {}


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    chromaticCircle 800



---- PROGRAM ----


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            {}
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
