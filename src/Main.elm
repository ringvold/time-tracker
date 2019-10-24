port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import DateFormat
import Dict exposing (Dict)
import Duration exposing (Duration)
import Element exposing (..)
import Element.Background exposing (color)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region exposing (..)
import Html exposing (Html)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as E
import Quantity
import Task
import Time exposing (Posix, Zone)



---- MODEL ----


type alias Model =
    { tracking : Tracking
    , timeZone : Zone
    , currentTime : Maybe Posix
    , dates : Dates
    , session : Session
    , username : String
    , password : String
    }


type Session
    = LoggedIn User
    | Guest


type alias Dates =
    Dict String Day


type alias Day =
    { date : String
    , posix : Posix
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
      , session = Guest
      , username = ""
      , password = ""
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
    | UserReceived (Result D.Error User)
    | PortErrorReceived
    | UsernameUpdated String
    | PasswordUpdated String
    | LoginClicked


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

        UserReceived (Ok user) ->
            ( { model | session = LoggedIn user }, Cmd.none )

        UserReceived (Err error) ->
            ( model, Cmd.none )

        PortErrorReceived ->
            ( model, Cmd.none )

        UsernameUpdated username ->
            ( { model | username = username }, Cmd.none )

        PasswordUpdated password ->
            ( { model | password = password }, Cmd.none )

        LoginClicked ->
            ( model, sendToJS <| LoginUser model.username model.password )


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
                    Just { date = key, posix = today, duration = newDuration }
        )
        dates


dateString : Zone -> Posix -> String
dateString =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.monthFixed
        , DateFormat.dayOfMonthFixed
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.session of
        Guest ->
            Element.layout [ spacing 10 ] <|
                column headingStyle
                    [ text "Login required"
                    , Input.email []
                        { onChange = UsernameUpdated
                        , text = model.username
                        , placeholder = Nothing
                        , label = Input.labelAbove [] (text "Email")
                        }
                    , Input.currentPassword []
                        { onChange = PasswordUpdated
                        , text = model.password
                        , placeholder = Nothing
                        , label = Input.labelAbove [] (text "Password")
                        , show = False
                        }
                    , Input.button []
                        { onPress = Just LoginClicked
                        , label = text "Login"
                        }
                    ]

        LoggedIn user ->
            Element.layout [ spacing 0 ] <|
                column [ centerX, spacing 30 ]
                    [ el headingStyle (text "Time tracking")
                    , viewTracking model
                    , viewDays model
                    ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


headingStyle =
    [ heading 1
    , Font.size 40
    , centerX
    , paddingEach { edges | top = 30 }
    , Font.center
    ]


green =
    rgb 220 20 60


viewTracking : Model -> Element Msg
viewTracking model =
    case model.tracking of
        NotStarted ->
            Input.button buttonStyle
                { onPress = Just StartTimeClicked
                , label = text "Start"
                }

        Started startTime ->
            column [ spacing 20, centerX ]
                [ Input.button buttonStyle
                    { onPress = Just <| StopTimeClicked startTime
                    , label = text "Stop"
                    }
                , el [ heading 3, centerX ] <| text <| "Started " ++ hourAndMinute model.timeZone startTime
                ]

        Ended startTime endTime ->
            Input.button buttonStyle
                { onPress = Just StartTimeClicked
                , label = text "Start"
                }


buttonStyle =
    [ color (rgb255 0 128 0)
    , paddingXY 25 20
    , Font.size 20
    , Font.semiBold
    , Font.color (rgb255 255 255 255)
    , centerX
    , Border.rounded 5
    , Font.center
    ]


viewDays : Model -> Element Msg
viewDays model =
    table []
        { data = Dict.values model.dates
        , columns =
            [ { header = Element.text "Date"
              , width = fill
              , view =
                    \day ->
                        Element.text <| displayDate model.timeZone day.posix
              }
            , { header = Element.text "Duration"
              , width = fill
              , view =
                    \day ->
                        Element.text <| showDuration day.duration
              }
            ]
        }


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



-- PORTS --


port toJS : GenericPortData -> Cmd msg


port fromJS : (GenericPortData -> msg) -> Sub msg


type alias GenericPortData =
    { tag : String, data : E.Value }


sendToJS : ToJS -> Cmd msg
sendToJS toSend =
    case toSend of
        LoginUser username password ->
            toJS { tag = "LoginUser", data = E.object [ ( "email", E.string username ), ( "password", E.string password ) ] }


encodeDates : Dates -> E.Value
encodeDates dates =
    E.list encodeDay (Dict.values dates)


encodeDay : Day -> E.Value
encodeDay day =
    E.object
        [ ( "date", E.string day.date )
        , ( "posix", E.string (String.fromInt <| Time.posixToMillis day.posix) )
        , ( "duration", E.string (String.fromFloat <| Duration.inMilliseconds day.duration) )
        ]


type alias User =
    { displayName : String
    , email : String
    , uid : String
    }


userDecoder : Decoder User
userDecoder =
    D.succeed User
        |> JDP.required "displayName" D.string
        |> JDP.required "email" D.string
        |> JDP.required "uid" D.string


subscriptions : Model -> Sub Msg
subscriptions model =
    fromJS
        (\portData ->
            case toElmTagFromString portData.tag of
                AuthStateChanged ->
                    UserReceived <| D.decodeValue userDecoder portData.data

                Unknown ->
                    PortErrorReceived
        )


type FromJS
    = AuthStateChanged
    | Unknown


type ToJS
    = LoginUser String String


toElmTagFromString string =
    case string of
        "AuthStateChanged" ->
            AuthStateChanged

        _ ->
            Unknown



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }
