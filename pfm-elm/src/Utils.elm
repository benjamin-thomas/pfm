module Utils exposing
    ( formatAmount
    , formatDate
    , formatDateForInput
    )

import Decimal exposing (Decimal)
import Time


formatAmount : Decimal -> String
formatAmount amount =
    let
        absAmount =
            Decimal.abs amount

        str =
            case Decimal.toString absAmount |> String.split "." of
                [ euros, cents ] ->
                    euros ++ "." ++ String.padRight 2 '0' cents

                [ euros ] ->
                    euros ++ ".00"

                _ ->
                    "IMPOSSIBLE"
    in
    str ++ " €"


formatDate : Time.Posix -> String
formatDate posix =
    let
        month =
            case Time.toMonth Time.utc posix of
                Time.Jan -> "Jan"
                Time.Feb -> "Feb"
                Time.Mar -> "Mar"
                Time.Apr -> "Apr"
                Time.May -> "May"
                Time.Jun -> "Jun"
                Time.Jul -> "Jul"
                Time.Aug -> "Aug"
                Time.Sep -> "Sep"
                Time.Oct -> "Oct"
                Time.Nov -> "Nov"
                Time.Dec -> "Dec"

        day =
            Time.toDay Time.utc posix |> String.fromInt

        year =
            Time.toYear Time.utc posix |> String.fromInt
    in
    month ++ " " ++ day ++ ", " ++ year


formatDateForInput : Time.Posix -> Bool -> String
formatDateForInput time showTime =
    let
        year = String.fromInt (Time.toYear Time.utc time)
        
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
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12
