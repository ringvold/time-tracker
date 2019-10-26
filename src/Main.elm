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
import Html.Events
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
    , form : Form
    , signup : Bool
    , error : Maybe String
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


type alias Form =
    { email : String
    , password : String
    }


init : ( Model, Cmd Msg )
init =
    ( { tracking = NotStarted
      , timeZone = Time.utc
      , currentTime = Nothing
      , dates = Dict.empty
      , session = Guest
      , form = Form "" ""
      , signup = False
      , error = Nothing
      }
    , Cmd.batch
        [ Task.perform TimeZoneReceived Time.here
        , Task.perform CurrentTimeReceived Time.now
        ]
    )



---- UPDATE ----


type Msg
    = StartTimeClicked
    | StopTimeClicked StartTime
    | ContinueTimeClicked StartTime
    | UpdatedTrackingReceived Tracking
    | CurrentTimeReceived Posix
    | TimeZoneReceived Zone
    | UserReceived (Result D.Error (Maybe User))
    | UnknownTagReceivedFromJS String
    | EmailUpdated String
    | PasswordUpdated String
    | LoginTriggered
    | ErrorReceived (Result D.Error Error)
    | InitSignupTriggered
    | SignupTriggered
    | UserSignedOutReceived
    | LogoutTriggered


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

        UserReceived (Ok (Just user)) ->
            ( { model
                | session = LoggedIn user
                , form = setEmail "" <| setPassword "" model.form
                , error = Nothing
              }
            , Cmd.none
            )

        UserReceived (Ok Nothing) ->
            -- Got null from authChanged
            -- Happens at logout. Not an error per se.
            ( { model
                | form = setEmail "" <| setPassword "" model.form
                , error = Nothing
              }
            , Cmd.none
            )

        UserReceived (Err error) ->
            ( { model | error = Just (D.errorToString error) }, Cmd.none )

        UnknownTagReceivedFromJS unkownTag ->
            ( model, Cmd.none )

        EmailUpdated email ->
            ( { model | form = setEmail email model.form }, Cmd.none )

        PasswordUpdated password ->
            ( { model | form = setPassword password model.form }, Cmd.none )

        LoginTriggered ->
            ( { model | error = Nothing }, sendToJS <| LoginUser model.form )

        InitSignupTriggered ->
            ( { model | signup = True }, Cmd.none )

        SignupTriggered ->
            ( model, sendToJS <| CreateUser model.form )

        UserSignedOutReceived ->
            ( { model | signup = False, session = Guest }, Cmd.none )

        LogoutTriggered ->
            ( model, sendToJS SignOut )

        ErrorReceived (Ok error) ->
            ( { model | error = Just error.message }, Cmd.none )

        ErrorReceived (Err error) ->
            ( { model | error = Just "Error handling error -_-" }, Cmd.none )


setEmail : String -> Form -> Form
setEmail email form =
    { form | email = email }


setPassword : String -> Form -> Form
setPassword password form =
    { form | password = password }


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



-- STYLING


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


mainContentStyle =
    [ Font.size 40
    , centerX
    , paddingEach { edges | top = 30 }
    , width (fill |> maximum 400)
    ]


green =
    rgb255 220 20 60


red =
    rgb255 240 128 128


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



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ spacing 10
        ]
    <|
        column mainContentStyle <|
            [ logoutButton model
            , column
                []
                [ loginSignupView model
                ]
            ]


logoutButton model =
    case model.session of
        Guest ->
            text ""

        LoggedIn user ->
            el [ alignRight, Font.size 20, spacing 20 ] <|
                Input.button []
                    { onPress = Just LogoutTriggered
                    , label = text "Logout"
                    }


errorView : Maybe String -> Element Msg
errorView error =
    el
        [ spacing 10 ]
    <|
        case error of
            Just err ->
                el
                    [ Font.size 20
                    , padding 10
                    , Border.dashed
                    , Border.color green
                    , Border.width 1
                    , Font.color green
                    ]
                <|
                    text err

            Nothing ->
                text ""


loginSignupView : Model -> Element Msg
loginSignupView model =
    column [ spacing 20 ] <|
        case model.session of
            Guest ->
                if model.signup then
                    signupView model

                else
                    loginView model

            LoggedIn user ->
                [ column [ centerX, spacing 30 ]
                    [ el [ spacing 20 ] (text "Time tracking")
                    , errorView model.error
                    , viewTracking model
                    , viewDays model
                    ]
                ]


loginView model =
    [ text "Login required"
    , errorView model.error
    , Input.email [ onEnter LoginTriggered, Font.size 30 ]
        { onChange = EmailUpdated
        , text = model.form.email
        , placeholder = Nothing
        , label = Input.labelAbove [ alignLeft ] (text "Email")
        }
    , Input.currentPassword [ onEnter LoginTriggered, Font.size 30 ]
        { onChange = PasswordUpdated
        , text = model.form.password
        , placeholder = Nothing
        , label = Input.labelAbove [ alignLeft ] (text "Password")
        , show = False
        }
    , row [ width fill ]
        [ Input.button [ alignLeft ]
            { onPress = Just LoginTriggered
            , label = text "Login"
            }
        , Input.button [ alignRight ]
            { onPress = Just InitSignupTriggered
            , label = text "Sign up"
            }
        ]
    ]


signupView model =
    [ text "Sign up"
    , errorView model.error
    , Input.email []
        { onChange = EmailUpdated
        , text = model.form.email
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ Font.alignLeft
                , Font.size 30
                ]
                (text "Email")
        }
    , Input.currentPassword [ onEnter SignupTriggered ]
        { onChange = PasswordUpdated
        , text = model.form.password
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ Font.alignLeft
                , Font.size 30
                ]
                (text "Password")
        , show = False
        }
    , Input.button []
        { onPress = Just SignupTriggered
        , label = text "Sign Up"
        }
    ]


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
                , el [ heading 3, centerX ] <|
                    text ("Started " ++ hourAndMinute model.timeZone startTime)
                ]

        Ended startTime endTime ->
            Input.button buttonStyle
                { onPress = Just StartTimeClicked
                , label = text "Start"
                }


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
    let
        toString =
            round >> String.fromInt
    in
    if duration |> Quantity.lessThan (Duration.minutes 60) then
        String.concat
            [ Duration.inMinutes duration |> toString
            , " minutes"
            ]

    else
        String.concat
            [ Duration.inHours duration |> toString
            , " hours"
            ]


hourAndMinute : Zone -> Posix -> String
hourAndMinute =
    DateFormat.format
        [ DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed msg

                        else
                            D.fail "Not the enter key"
                    )
            )
        )



-- PORTS --


port toJS : GenericPortData -> Cmd msg


port fromJS : (GenericPortData -> msg) -> Sub msg


type alias GenericPortData =
    { tag : String, data : E.Value }


sendToJS : ToJS -> Cmd msg
sendToJS toSend =
    case toSend of
        LoginUser form ->
            toJS
                { tag = "LoginUser"
                , data = encodeEmailAndPassword form.email form.password
                }

        CreateUser form ->
            toJS
                { tag = "CreateUser"
                , data =
                    encodeEmailAndPassword form.email form.password
                }

        SignOut ->
            toJS
                { tag = "SignOut"
                , data = E.null
                }


encodeEmailAndPassword : String -> String -> E.Value
encodeEmailAndPassword email password =
    E.object
        [ ( "email", E.string email )
        , ( "password", E.string password )
        ]


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
    { displayName : Maybe String
    , email : String
    , uid : String
    }


userDecoder : Decoder User
userDecoder =
    D.succeed User
        |> JDP.optional "displayName" (D.maybe D.string) Nothing
        |> JDP.required "email" D.string
        |> JDP.required "uid" D.string


type alias Error =
    { code : String, message : String }


errorDecoder : Decoder Error
errorDecoder =
    D.succeed Error
        |> JDP.required "code" D.string
        |> JDP.required "message" D.string


subscriptions : Model -> Sub Msg
subscriptions model =
    fromJS
        (\portData ->
            case fromJsTagFromString portData.tag of
                AuthStateChanged ->
                    UserReceived <|
                        D.decodeValue
                            (D.oneOf
                                [ D.null Nothing
                                , D.map Just userDecoder
                                ]
                            )
                            portData.data

                LoginError ->
                    ErrorReceived <|
                        D.decodeValue
                            errorDecoder
                            portData.data

                SignupError ->
                    ErrorReceived <|
                        D.decodeValue
                            errorDecoder
                            portData.data

                UserSignedOut ->
                    UserSignedOutReceived

                UnknownTag tag ->
                    UnknownTagReceivedFromJS tag
        )


type FromJS
    = AuthStateChanged
    | LoginError
    | SignupError
    | UserSignedOut
    | UnknownTag String


type ToJS
    = LoginUser Form
    | CreateUser Form
    | SignOut


fromJsTagFromString : String -> FromJS
fromJsTagFromString string =
    case string of
        "AuthStateChanged" ->
            AuthStateChanged

        "LoginError" ->
            LoginError

        "SignupError" ->
            SignupError

        "UserSignedOut" ->
            UserSignedOut

        _ ->
            UnknownTag string



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }
