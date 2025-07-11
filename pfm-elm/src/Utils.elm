module Utils exposing
    ( amountFmt
    , amountFmt2
    , dateFmt
    , dateFmtUnix
    , formatDate
    , formatDateForInput
    , formatDateTimeLocal
    )

import Decimal exposing (Decimal)
import Iso8601
import Time


amountFmt : Decimal -> String
amountFmt amount =
    let
        str =
            case Decimal.toString amount |> String.split "." of
                [ euros, cents ] ->
                    euros ++ "." ++ String.padRight 2 '0' cents

                [ euros ] ->
                    euros ++ ".00"

                _ ->
                    "IMPOSSIBLE"
    in
    str ++ "\u{00A0}€"


amountFmt2 : { intPart : Int, decPart : Int } -> String
amountFmt2 { intPart, decPart } =
    String.fromInt intPart
        ++ "."
        ++ String.padRight 2 '0' (String.fromInt decPart)
        ++ "\u{00A0}€"


formatDate : Time.Posix -> String
formatDate posix =
    let
        month =
            case Time.toMonth Time.utc posix of
                Time.Jan ->
                    "Jan"

                Time.Feb ->
                    "Feb"

                Time.Mar ->
                    "Mar"

                Time.Apr ->
                    "Apr"

                Time.May ->
                    "May"

                Time.Jun ->
                    "Jun"

                Time.Jul ->
                    "Jul"

                Time.Aug ->
                    "Aug"

                Time.Sep ->
                    "Sep"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"

        day =
            Time.toDay Time.utc posix |> String.fromInt

        year =
            Time.toYear Time.utc posix |> String.fromInt
    in
    month ++ " " ++ day ++ ", " ++ year


formatDateForInput : Time.Posix -> Bool -> String
formatDateForInput time showTime =
    let
        year =
            String.fromInt (Time.toYear Time.utc time)

        month =
            monthToInt (Time.toMonth Time.utc time)
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'

        hour =
            Time.toHour Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    if showTime then
        year ++ "-" ++ month ++ "-" ++ day ++ "T" ++ hour ++ ":" ++ minute

    else
        year ++ "-" ++ month ++ "-" ++ day


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


formatDateTimeLocal : Time.Zone -> Time.Posix -> String
formatDateTimeLocal zone time =
    let
        year =
            String.fromInt (Time.toYear zone time)

        month =
            String.padLeft 2 '0' (String.fromInt (monthToInt (Time.toMonth zone time)))

        day =
            String.padLeft 2 '0' (String.fromInt (Time.toDay zone time))

        hour =
            String.padLeft 2 '0' (String.fromInt (Time.toHour zone time))

        minute =
            String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))

        seconds =
            String.padLeft 2 '0' (String.fromInt (Time.toSecond zone time))
    in
    year ++ "-" ++ month ++ "-" ++ day ++ "T" ++ hour ++ ":" ++ minute ++ ":" ++ seconds


dateFmt : Time.Posix -> String
dateFmt =
    String.left 10
        << Iso8601.fromTime


dateFmtUnix : Int -> String
dateFmtUnix dateUnix =
    dateFmt <| Time.millisToPosix <| dateUnix * 1000



-- uniqueBy : (a -> comparable) -> List a -> List a
-- uniqueBy toComparable =
--     Tuple.first
--         << List.foldl
--             (\item ( items, keys ) ->
--                 if Set.member (toComparable item) keys then
--                     ( items, keys )
--                 else
--                     ( item :: items, Set.insert (toComparable item) keys )
--             )
--             ( [], Set.empty )
