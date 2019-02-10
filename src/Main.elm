module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Time exposing (Posix, Zone)



---- MODEL ----


type alias Model =
    { tracking : Tracking
    , timeZone : Zone
    }


type Tracking
    = NotStarted
    | Started StartTime
    | Ended StartTime EndTime


type alias StartTime =
    Posix


type alias EndTime =
    Posix


init : ( Model, Cmd Msg )
init =
    ( { tracking = NotStarted
      , timeZone = Time.utc
      }
    , Cmd.batch [ Task.perform TimeZoneReceived Time.here ]
    )



---- UPDATE ----


type Msg
    = StartTimeClicked
    | StopTimeClicked StartTime
    | ContinueTimeClicked StartTime
    | CurrentTimeReceived Tracking
    | TimeZoneReceived Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTimeClicked ->
            ( model, Cmd.batch [ Task.perform (CurrentTimeReceived << Started) Time.now ] )

        StopTimeClicked startTime ->
            ( model, Cmd.batch [ Task.perform (CurrentTimeReceived << Ended startTime) Time.now ] )

        ContinueTimeClicked startTime ->
            ( { model | tracking = Started startTime }, Cmd.none )

        CurrentTimeReceived tracking ->
            ( { model | tracking = tracking }, Cmd.none )

        TimeZoneReceived zone ->
            ( { model | timeZone = zone }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Time tracking" ]
        , viewTracking model
        ]


viewTracking : Model -> Html Msg
viewTracking model =
    case model.tracking of
        NotStarted ->
            button [ class "button start-time", onClick StartTimeClicked ] [ text "Start" ]

        Started startTime ->
            div []
                [ button [ class "button stop-time", onClick <| StopTimeClicked startTime ] [ text "Stop" ]
                , div [] [ h3 [] [ text <| "Started: " ++ hourAndMinute model.timeZone startTime ] ]
                ]

        Ended startTime endTime ->
            div []
                [ button [ class "button", onClick <| ContinueTimeClicked startTime ] [ text "Continue" ]
                , div [] [ h3 [] [ text <| "Started: " ++ hourAndMinute model.timeZone startTime ] ]
                , div [] [ h3 [] [ text <| "Ended: " ++ hourAndMinute model.timeZone endTime ] ]
                ]


hourAndMinute : Zone -> Posix -> String
hourAndMinute zone startTime =
    let
        toString transformer =
            transformer zone startTime
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    toString Time.toHour ++ ":" ++ toString Time.toMinute



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
