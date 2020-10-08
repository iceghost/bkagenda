module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (default)


main : Program () Model Msg
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
    , weekday : String
    , time : String
    , room : String
    , weeks : List Int
    }


type alias Model =
    { courses : List Course }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ("""MI1003\tGiáo dục quốc phòng\t--\t--\tL02\t--\t0-0\t0:00 - 0:00\t------\tBK-CS1\t--|--|--|--|--|--|45|46|47|48|
CO1023\tHệ thống số\t3\t--\tL01\t2\t2-4\t7:00 - 9:50\tH1-201\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
MT1003\tGiải tích 1\t4\t4\tL25\t3\t2-4\t7:00 - 9:50\tH1-304\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
CO1024\tHệ thống số (thí nghiệm)\t--\t--\tL03\t3\t7-9\t12:00 - 14:50\tH6-605\tBK-CS2\t--|--|--|--|--|44|--|--|--|--|49|50|51|52|53|01|02|
PH1003\tVật lý 1\t4\t4\tL24\t4\t8-10\t13:00 - 15:50\tH1-301\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
CO1006\tNhập môn điện toán (TH)\t--\t--\tL02\t5\t2-4\t7:00 - 9:50\tH6-707\tBK-CS2\t--|--|--|--|--|44|--|--|--|--|49|50|51|52|53|01|02|
PH1004\tVật lý 1 (bt)\t--\t--\tL44\t5\t8-9\t13:00 - 14:50\tH1-104\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
MT1004\tGiải tích 1 (bài tập)\t--\t--\tL44\t5\t10-11\t15:00 - 16:50\tH1-103\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|53|01|02|
CO1005\tNhập môn điện toán\t3\t3\tL01\t6\t2-4\t7:00 - 9:50\tH6-GDH6\tBK-CS2\t--|--|--|42|43|44|--|--|--|--|49|50|51|52|""" |> rawToCourses), Cmd.none )


type Msg
    = RawToCourses String
    | Msg2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RawToCourses raw ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )


rawToCourses : String -> List Course
rawToCourses raw =
    String.split "\n" raw
        |> List.filterMap
            (\line ->
                case String.split "\t" line of
                    [ id, name, _, _, _, weekday, _, time, room, _, rawWeeks ] ->
                        let
                            weeks =
                                rawWeeks |> String.split "|" |> List.filterMap String.toInt
                        in
                        Just (Course id name weekday time room weeks)

                    _ ->
                        Nothing
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        (List.map viewCourse model.courses)


viewCourse : Course -> Html Msg
viewCourse { name, weekday } =
    p [] [ text ("Học " ++ name ++ " vào thứ " ++ weekday) ]
