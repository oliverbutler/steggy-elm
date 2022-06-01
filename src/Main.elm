module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Image =
    { url : String
    }


type alias Model =
    { file : Maybe Image }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { file = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = GotFiles (List File)
    | GotUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            let
                file =
                    case files of
                        [] ->
                            Nothing

                        x :: _ ->
                            Just x
            in
            ( model
            , case file of
                Nothing ->
                    Cmd.none

                Just f ->
                    Task.perform GotUrl <| File.toUrl f
            )

        GotUrl url ->
            ( { file = Just { url = url } }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (D.map GotFiles filesDecoder)
            ]
            []
        , case model.file of
            Nothing ->
                div []
                    [ text "No file selected" ]

            Just file ->
                img [ src file.url ] []
        ]


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)
