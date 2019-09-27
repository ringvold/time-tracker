module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import DateFormat
import Dict exposing (Dict)
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Quantity
import Task
import Time exposing (Posix, Zone)



---- MODEL ----


type alias Model =
    { tracking : Tracking
    , timeZone : Zone
    , currentTime : Maybe Posix
    , dates : Dates
    }


type alias Dates =
    Dict String Day


type alias Day =
    { date : Posix
    , duration : Duration
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
      , currentTime = Nothing
      , dates = Dict.empty
      }
    , Cmd.batch [ Task.perform TimeZoneReceived Time.here, Task.perform CurrentTimeReceived Time.now ]
    )



---- UPDATE ----


type Msg
    = StartTimeClicked
    | StopTimeClicked StartTime
    | ContinueTimeClicked StartTime
    | UpdatedTrackingReceived Tracking
    | CurrentTimeReceived Posix
    | TimeZoneReceived Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTimeClicked ->
            ( model, Cmd.batch [ Task.perform (UpdatedTrackingReceived << Started) Time.now ] )

        StopTimeClicked startTime ->
            ( model, Cmd.batch [ Task.perform (UpdatedTrackingReceived << Ended startTime) Time.now ] )

        ContinueTimeClicked startTime ->
            ( { model | tracking = Started startTime }, Cmd.none )

        UpdatedTrackingReceived tracking ->
            case tracking of
                Started time ->
                    ( { model | tracking = tracking }, Cmd.none )

                Ended start stop ->
                    let
                        date =
                            dateString model.timeZone start

                        duration =
                            Duration.from start stop

                        newDates =
                            updateTrackings date start model.dates duration
                    in
                    ( { model | tracking = tracking, dates = newDates }, Cmd.none )

                NotStarted ->
                    ( model, Cmd.none )

        CurrentTimeReceived ct ->
            ( { model | currentTime = Just ct }, Cmd.none )

        TimeZoneReceived zone ->
            ( { model | timeZone = zone }, Cmd.none )


updateTrackings : String -> Posix -> Dates -> Duration -> Dates
updateTrackings key today dates newDuration =
    Dict.update key
        (\value ->
            case value of
                Just ({ duration } as day) ->
                    Just
                        { day
                            | duration = Quantity.plus duration newDuration
                        }

                Nothing ->
                    Just { date = today, duration = newDuration }
        )
        dates


dateString : Zone -> Posix -> String
dateString =
    DateFormat.format
        [ DateFormat.dayOfMonthFixed
        , DateFormat.monthFixed
        , DateFormat.yearNumber
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Time tracking" ]
        , viewTracking model
        , viewDates model
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


viewDates : Model -> Html msg
viewDates model =
    Dict.toList model.dates
        |> List.map (viewDate model.timeZone)
        |> div []


viewDate : Zone -> ( String, Day ) -> Html msg
viewDate zone ( key, day ) =
    div [] [ h2 [] [ text <| displayDate zone day.date ++ " : " ++ showDuration day.duration ] ]


displayDate : Zone -> Posix -> String
displayDate =
    DateFormat.format
        [ DateFormat.dayOfMonthFixed
        , DateFormat.text "."
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        ]


showDuration duration =
    if duration |> Quantity.lessThan (Duration.minutes 60) then
        String.concat [ String.fromInt <| round <| Duration.inMinutes duration, " minutes" ]

    else
        String.concat [ String.fromInt <| round <| Duration.inHours duration, " hours" ]


hourAndMinute : Zone -> Posix -> String
hourAndMinute =
    DateFormat.format
        [ DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
