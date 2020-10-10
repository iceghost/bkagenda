port module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Heroicons.Outline exposing (chevronLeft, chevronRight, clock, locationMarker)
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, preload, value)
import Html.Events exposing (onClick, onInput)
import Svg.Attributes as SvgAttr
import Task exposing (Task)
import Time exposing (Month(..), Posix, Weekday(..), Zone, millisToPosix, posixToMillis)


port saveData : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Course =
    { id : String
    , name : String
    , weekday : Int
    , period : ( Int, Int )
    , time : String
    , room : String
    , weeks : List Int
    }


type State
    = InputRaw String
    | ViewCourses (List Course)


type alias Model =
    { state : State
    , currentTime : ( Posix, Zone )
    }


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init raw =
    let
        state =
            case raw |> rawToCourses of
                [] ->
                    InputRaw ""

                courses ->
                    ViewCourses courses
    in
    ( Model state ( Time.millisToPosix 0, Time.utc )
    , Task.perform GotTime Time.now
    )



---- UPDATE ----


type Msg
    = SaveData String
    | GotRaw String
    | GotTime Posix
    | GotZone Zone
    | NextWeek
    | PrevWeek
    | SetDay Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveData raw ->
            ( { model
                | state = ViewCourses (raw |> rawToCourses)
              }
            , saveData raw
            )

        GotRaw raw ->
            ( { model
                | state = InputRaw (String.trim raw)
              }
            , Cmd.none
            )

        GotTime posix ->
            ( { model | currentTime = ( posix, Tuple.second model.currentTime ) }, Cmd.none )

        GotZone zone ->
            ( { model | currentTime = ( Tuple.first model.currentTime, zone ) }, Cmd.none )

        NextWeek ->
            ( { model
                | currentTime =
                    model.currentTime
                        |> Tuple.mapFirst
                            (\posix ->
                                millisToPosix (posixToMillis posix + dayToMillis 7)
                            )
              }
            , Cmd.none
            )

        PrevWeek ->
            ( { model
                | currentTime =
                    model.currentTime
                        |> Tuple.mapFirst
                            (\posix ->
                                millisToPosix (posixToMillis posix - dayToMillis 7)
                            )
              }
            , Cmd.none
            )

        SetDay posix ->
            ( { model | currentTime = model.currentTime |> Tuple.mapFirst (\_ -> posix) }
            , Cmd.none
            )


rawToCourses : String -> List Course
rawToCourses raw =
    String.split "\n" raw
        |> List.filterMap
            (\line ->
                case String.split "\t" line of
                    [ id, name, _, _, _, rawWeekday, rawPeriod, rawTime, room, _, rawWeeks ] ->
                        let
                            defaultZero =
                                String.toInt >> Maybe.withDefault 0

                            weeks =
                                rawWeeks |> String.split "|" |> List.filterMap String.toInt

                            weekday =
                                defaultZero rawWeekday

                            period =
                                case String.split "-" rawPeriod of
                                    [ begin, end ] ->
                                        ( defaultZero begin, defaultZero end )

                                    _ ->
                                        ( 0, 0 )
                        in
                        Just (Course id name weekday period rawTime room weeks)

                    _ ->
                        Nothing
            )



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


example : String
example =
    """MI1003\tGiáo dục quốc phòng\t--\t--\tL02\t--\t0-0\t0:00 - 0:00\t------\tBK-CS1\t--|--|--|--|--|--|45|46|47|48|
..."""


view : Model -> Html Msg
view model =
    div [ class "container flex flex-col justify-between mx-auto min-h-screen w-screen text-blue-900" ]
        (case model.state of
            InputRaw raw ->
                viewInput raw

            ViewCourses courses ->
                case courses of
                    [] ->
                        [ text "Không đọc được môn nào cả. Vui lòng "
                        , button [ class "bg-blue-500 px-1 text-white rounded shadow-md", onClick (GotRaw "") ] [ text "nhập lại" ]
                        , text " bạn nhé"
                        ]

                    _ ->
                        viewCourses model.currentTime courses
        )


viewInput : String -> List (Html Msg)
viewInput raw =
    div [ class "flex flex-col" ]
        [ label [ class "w-full text-center" ]
            [ div [ class "bg-gradient-to-b from-blue-800 to-blue-700 p-2 text-white text-xl font-thin" ] [ text "Copy nguyên cái bảng TKB vào đây" ]
            , textarea
                [ value raw
                , placeholder example
                , onInput GotRaw
                , class "block w-full p-1 h-56 bg-blue-100 shadow-inset placeholder-blue-400"
                ]
                []
            ]
        , button
            [ onClick (SaveData raw)
            , class "bg-blue-500 mt-2 py-2 text-white rounded shadow-md"
            ]
            [ text "Lưu" ]
        ]
        |> List.singleton


viewCourses : ( Posix, Zone ) -> List Course -> List (Html Msg)
viewCourses currentTime courses =
    let
        thisWeek =
            posixToWeekNumber currentTime

        thisWeekday =
            dayOfWeek currentTime

        header =
            div [ class "flex flex-col bg-gradient-to-t from-blue-600 to-blue-700 text-white" ]
                [ div [ class "flex items-center justify-center gap-2 py-2" ]
                    [ button
                        [ class "rounded bg-blue-500 shadow-md"
                        , class "hover:bg-white hover:text-blue-500"
                        , onClick PrevWeek
                        ]
                        [ chevronLeft [ SvgAttr.class "h-6" ] ]
                    , span [ class "text-2xl font-bold" ]
                        [ text ("Tuần " ++ String.fromInt thisWeek) ]
                    , button
                        [ class "rounded bg-blue-500 shadow-md"
                        , class "hover:bg-white hover:text-blue-500"
                        , onClick NextWeek
                        ]
                        [ chevronRight [ SvgAttr.class "h-6" ] ]
                    ]
                , viewWeekdays currentTime
                ]

        footer =
            div [ class "flex flex-col items-center bg-blue-600 text-white p-2" ]
                [ button [ class "underline", onClick (GotRaw "") ] [ text "Nhập lại TKB tại đây" ]
                , p [ class "text-sm font-thin " ] [ text "Made with love by a K20 ❤" ]
                ]

        todayCourses =
            courses |> List.filter (\{ weekday, weeks } -> List.member thisWeek weeks && weekday == thisWeekday)
    in
    [ header
    , div [ class "flex flex-col gap-1" ] (List.map viewCourse todayCourses)
    , footer
    ]


viewCourse : Course -> Html Msg
viewCourse { name, period, weekday, room, time } =
    div
        [ class "flex flex-col gap-1 p-2 text-blue-900"
        , class "bg-gradient-to-r from-blue-400 to-blue-300"
        ]
        [ span [ class "font-semibold" ] [ text name ]
        , div [ class "flex flex-row items-center gap-1" ]
            [ locationMarker [ SvgAttr.class "h-4" ]
            , span [] [ text room ]
            ]
        , div [ class "flex flex-row items-center gap-1" ]
            [ clock [ SvgAttr.class "h-4" ]
            , span [] [ text time ]
            ]
        ]


viewWeekdays : ( Posix, Zone ) -> Html Msg
viewWeekdays ( posix, zone ) =
    let
        thisWeekday =
            dayOfWeek ( posix, zone )
    in
    div [ class "flex justify-around" ]
        (List.map
            (\weekday ->
                let
                    thisPosix =
                        Time.millisToPosix (posixToMillis posix + dayToMillis (weekday - thisWeekday))
                in
                div
                    [ class "flex flex-col items-center"
                    , onClick (SetDay thisPosix)
                    ]
                    [ button
                        [ class "flex items-center justify-center h-8 w-8 shadow-md rounded-full"
                        , if thisWeekday == weekday then
                            class "bg-blue-500 text-white"

                          else
                            class "bg-white text-blue-900"
                        ]
                        [ span []
                            [ text
                                (if weekday == 7 then
                                    "CN"

                                 else
                                    "T" ++ String.fromInt (weekday + 1)
                                )
                            ]
                        ]
                    , span [ class "font-thin text-white"] [ text (timeToString ( thisPosix, zone )) ]
                    ]
            )
            (List.range 1 7)
        )



---- HELPERS ----


timeToString : ( Posix, Zone ) -> String
timeToString ( posix, zone ) =
    let
        day =
            Time.toDay zone posix

        month =
            case Time.toMonth zone posix of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12

        toString =
            String.fromInt >> String.padLeft 2 '0'
    in
    toString day ++ "/" ++ toString month


isLeapYear : Int -> Bool
isLeapYear year =
    if remainderBy 4 year /= 0 then
        False

    else if remainderBy 100 year /= 0 then
        True

    else if remainderBy 400 year /= 0 then
        False

    else
        True


dayOfWeek : ( Posix, Zone ) -> Int
dayOfWeek ( posix, zone ) =
    case Time.toWeekday zone posix of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            7


posixToWeekNumber : ( Posix, Zone ) -> Int
posixToWeekNumber ( posix, zone ) =
    let
        year =
            Time.toYear zone posix

        p y =
            let
                yFloat =
                    toFloat y
            in
            (y + floor (yFloat / 4.0) - floor (yFloat / 100) + floor (yFloat / 400)) |> remainderBy 7

        weeks y =
            52
                + (if p y == 4 || p (y - 1) == 3 then
                    1

                   else
                    0
                  )

        leapYearOffset =
            if isLeapYear year then
                1

            else
                0

        dayOfMonth =
            Time.toDay zone posix

        dayOfYear =
            case Time.toMonth zone posix of
                Jan ->
                    dayOfMonth

                Feb ->
                    31 + dayOfMonth

                Mar ->
                    59 + leapYearOffset + dayOfMonth

                Apr ->
                    90 + leapYearOffset + dayOfMonth

                May ->
                    120 + leapYearOffset + dayOfMonth

                Jun ->
                    151 + leapYearOffset + dayOfMonth

                Jul ->
                    181 + leapYearOffset + dayOfMonth

                Aug ->
                    212 + leapYearOffset + dayOfMonth

                Sep ->
                    243 + leapYearOffset + dayOfMonth

                Oct ->
                    273 + leapYearOffset + dayOfMonth

                Nov ->
                    304 + leapYearOffset + dayOfMonth

                Dec ->
                    334 + leapYearOffset + dayOfMonth

        w =
            floor (toFloat (dayOfYear - dayOfWeek ( posix, zone ) + 10) / 7)
    in
    if w < 1 then
        weeks (year - 1)

    else if w > weeks year then
        1

    else
        w


dayToMillis : Int -> Int
dayToMillis day =
    day * 86400000
