port module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, preload, value)
import Html.Events exposing (onClick, onInput)


port saveData : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Course =
    { id : String
    , name : String
    , weekday : Int
    , period : ( Int, Int )
    , room : String
    , weeks : List Int
    }


type State
    = InputRaw String
    | ViewCourses (List Course)


type alias Model =
    { state : State
    , thisWeek : Int
    }


type alias Flags =
    { raw : String
    , thisWeek : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { raw, thisWeek } =
    let
        state =
            case raw |> rawToCourses of
                [] ->
                    InputRaw ""

                courses ->
                    ViewCourses courses
    in
    ( Model state thisWeek, Cmd.none )


type Msg
    = SaveData String
    | GotRaw String


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


rawToCourses : String -> List Course
rawToCourses raw =
    String.split "\n" raw
        |> List.filterMap
            (\line ->
                case String.split "\t" line of
                    [ id, name, _, _, _, rawWeekday, rawPeriod, _, room, _, rawWeeks ] ->
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
                        Just (Course id name weekday period room weeks)

                    _ ->
                        Nothing
            )


example : String
example =
    """MI1003\tGiáo dục quốc phòng\t--\t--\tL02\t--\t0-0\t0:00 - 0:00\t------\tBK-CS1\t--|--|--|--|--|--|45|46|47|48|
CO1023\tHệ thống số\t3\t--\tL01\t2\t2-4\t7:00 - 9:50\tH1-201\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
MT1003\tGiải tích 1\t4\t4\tL25\t3\t2-4\t7:00 - 9:50\tH1-304\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
..."""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "min-h-screen min-w-screen" ]
        [ case model.state of
            InputRaw raw ->
                div [ class "mx-16" ]
                    [ label []
                        [ text "Copy nguyên cái bảng vào đây"
                        , textarea
                            [ value raw
                            , placeholder example
                            , onInput GotRaw
                            , class "block w-full p-1 h-56 border"
                            ]
                            []
                        ]
                    , button
                        [ onClick (SaveData raw)
                        , class "bg-red-500 mt-2 p-1 text-white"
                        ]
                        [ text "Lưu" ]
                    ]

            ViewCourses courses ->
                div []
                    [ button [ onClick (GotRaw "") ] [ text "Reset" ]
                    , div
                        [ class "grid grid-cols-8 grid-rows-13 gap-2 w-auto h-full"
                        ]
                        (viewWeekdays
                            ++ viewPeriods
                            ++ List.map viewCourse courses
                        )
                    ]
        ]


viewCourse : Course -> Html Msg
viewCourse { name, period, weekday, room } =
    div
        [ class (infoToClass weekday period)
        , class "flex flex-col p-2 rounded text-white"
        , class "bg-gradient-to-r from-green-500 to-blue-500"
        ]
        [ span [ class "font-semibold"] [ text name ]
        , span [ class "text-sm"] [ text ("Tại " ++ room) ]
        ]


infoToClass : Int -> ( Int, Int ) -> String
infoToClass weekday ( begin, end ) =
    if begin <= 0 || end <= 0 || weekday < 2 then
        "hidden"

    else
        "col-start-"
            ++ String.fromInt weekday
            ++ " col-span-1"
            ++ " row-start-"
            ++ String.fromInt (begin + 1)
            ++ " row-end-"
            ++ String.fromInt (end + 2)


viewPeriods : List (Html Msg)
viewPeriods =
    let
        periodToTime period =
            case period of
                1 ->
                    "06:00 - 06:50"

                2 ->
                    "07:00 - 07:50"

                3 ->
                    "08:00 - 08:50"

                4 ->
                    "09:00 - 09:50"

                5 ->
                    "10:00 - 10:50"

                6 ->
                    "11:00 - 11:50"

                7 ->
                    "12:00 - 12:50"

                8 ->
                    "13:00 - 13:50"

                9 ->
                    "14:00 - 14:50"

                10 ->
                    "15:00 - 15:50"

                11 ->
                    "16:00 - 16:50"

                _ ->
                    "Tiết này ngộ à nha..."
    in
    List.range 1 11
        |> List.map
            (\period ->
                [ div
                    [ class "col-start-1 col-span-8"
                    , if remainderBy 2 period == 1 then
                        class "bg-blue-100"

                      else
                        class ""
                    , class (periodToClass ( period, period ))
                    ]
                    []
                , div
                    [ class "col-start-1"
                    , class (periodToClass ( period, period ))
                    , class "flex flex-col items-center"
                    ]
                    [ span [ class "uppercase font-semibold tracking-wide" ]
                        [ text
                            ("TIẾT "
                                ++ String.fromInt period
                            )
                        ]
                    , span [ class "text-blue-500" ] [ text (periodToTime period) ]
                    ]
                ]
            )
        |> List.concat


viewWeekdays : List (Html Msg)
viewWeekdays =
    let
        weekdayToText weekday =
            case weekday of
                2 ->
                    "Thứ 2"

                3 ->
                    "Thứ 3"

                4 ->
                    "Thứ 4"

                5 ->
                    "Thứ 5"

                6 ->
                    "Thứ 6"

                7 ->
                    "Thứ 7"

                8 ->
                    "Chủ nhật"

                _ ->
                    "Thứ gì ngộ dị"
    in
    List.range 2 8
        |> List.map
            (\weekday ->
                div
                    [ class "row-start-1"
                    , class (weekdayToClass weekday)
                    , class "text-center font-semibold uppercase tracking-wide"
                    ]
                    [ text (weekdayToText weekday) ]
            )


weekdayToClass : Int -> String
weekdayToClass weekday =
    case weekday of
        2 ->
            "col-start-2 col-span-1"

        3 ->
            "col-start-3 col-span-1"

        4 ->
            "col-start-4 col-span-1"

        5 ->
            "col-start-5 col-span-1"

        6 ->
            "col-start-6 col-span-1"

        7 ->
            "col-start-7 col-span-1"

        8 ->
            "col-start-8 col-span-1"

        _ ->
            "hidden"


periodToClass : ( Int, Int ) -> String
periodToClass ( begin, end ) =
    let
        beginClass =
            case begin of
                1 ->
                    "row-start-2"

                2 ->
                    "row-start-3"

                3 ->
                    "row-start-4"

                4 ->
                    "row-start-5"

                5 ->
                    "row-start-6"

                6 ->
                    "row-start-7"

                7 ->
                    "row-start-8"

                8 ->
                    "row-start-9"

                9 ->
                    "row-start-10"

                10 ->
                    "row-start-11"

                11 ->
                    "row-start-12"

                _ ->
                    "hidden"

        endClass =
            case end of
                1 ->
                    "row-end-3"

                2 ->
                    "row-end-4"

                3 ->
                    "row-end-5"

                4 ->
                    "row-end-6"

                5 ->
                    "row-end-7"

                6 ->
                    "row-end-8"

                7 ->
                    "row-end-9"

                8 ->
                    "row-end-10"

                9 ->
                    "row-end-11"

                10 ->
                    "row-end-12"

                11 ->
                    "row-end-13"

                _ ->
                    "hidden"
    in
    beginClass ++ " " ++ endClass
