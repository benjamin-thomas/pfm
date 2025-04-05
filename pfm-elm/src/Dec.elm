module Dec exposing (..)


type Sign a
    = Pos a
    | Neg a


type Dec a
    = Zero
    | MkSign
        (Sign
            { whole : a
            , fract : a
            , precision : Int -- the number of decimal places
            }
        )



{-

   doctest-setup> import Dec exposing (..)

-}


{-| Convert a string to a `Dec`.

doctest> fromString "wat"
Nothing : Maybe (Dec Int)

doctest> fromString "00"
Just Zero : Maybe (Dec Int)

doctest> fromString "00.000"
Just Zero : Maybe (Dec Int)

doctest> fromString "1.23"
Just (MkSign (Pos { fract = 23, precision = 2, whole = 1 })) : Maybe (Dec Int)

doctest> fromString "-1.23"
Just (MkSign (Neg { fract = 23, precision = 2, whole = 1 })) : Maybe (Dec Int)

doctest> fromString "-0.23"
Just (MkSign (Neg { fract = 23, precision = 2, whole = 0 })) : Maybe (Dec Int)

-}
fromString : String -> Maybe (Dec Int)
fromString str =
    case String.split "." str of
        [ whole, fract ] ->
            case ( String.toInt whole, String.toInt fract ) of
                ( Just w, Just f ) ->
                    let
                        precision =
                            String.length fract
                    in
                    if f < 0 then
                        -- bad data: "0.-0"
                        Nothing

                    else if w < 0 then
                        Just <|
                            MkSign
                                (Neg
                                    { whole = abs w
                                    , fract = f
                                    , precision = precision
                                    }
                                )

                    else if w > 0 then
                        Just <|
                            MkSign
                                (Pos
                                    { whole = w
                                    , fract = f
                                    , precision = precision
                                    }
                                )

                    else if f == 0 then
                        Just Zero

                    else if String.left 1 str == "-" then
                        Just <| MkSign (Neg { whole = 0, fract = f, precision = precision })

                    else
                        Just <| MkSign (Pos { whole = 0, fract = f, precision = precision })

                _ ->
                    Nothing

        [ _ ] ->
            fromString (str ++ "." ++ "0")

        _ ->
            Nothing


{-| --

doctest> toString Zero
"0.00" : String

doctest> fromString "1" |> Maybe.map toString
Just "1.00" : Maybe String

doctest> fromString "1.23" |> Maybe.map toString
Just "1.23" : Maybe String

doctest> fromString "-1.23" |> Maybe.map toString
Just "-1.23" : Maybe String

doctest> fromString "-0.23" |> Maybe.map toString
Just "-0.23" : Maybe String

doctest> fromString "0" |> Maybe.map toString
Just "0.00" : Maybe String

doctest> fromString "0.023" |> Maybe.map toString
Just "0.023" : Maybe String

doctest> fromString "42.0007" |> Maybe.map toString
Just "42.0007" : Maybe String

doctest> fromString "-42.0007" |> Maybe.map toString
Just "-42.0007" : Maybe String

doctest> fromString "0.3" |> Maybe.map toString
Just "0.30" : Maybe String

-}
toString : Dec Int -> String
toString dec =
    let
        padFract fract precision =
            String.padRight 2 '0' <|
                String.padLeft
                    precision
                    '0'
                    (String.fromInt fract)
    in
    case dec of
        Zero ->
            "0.00"

        MkSign (Pos { whole, fract, precision }) ->
            String.fromInt whole ++ "." ++ padFract fract precision

        MkSign (Neg { whole, fract, precision }) ->
            "-" ++ String.fromInt whole ++ "." ++ padFract fract precision


{-| --

doctest> add Zero Zero
Zero : Dec Int

doctest> Maybe.map toString <| Maybe.map2 add (fromString "0") (fromString "0")
Just "0.00" : Maybe String

doctest> Maybe.map toString <| Maybe.map2 add (fromString "0") (fromString "1")
Just "1.00" : Maybe String

doctest> Maybe.map toString <| Maybe.map2 add (fromString "1") (fromString "0")
Just "1.00" : Maybe String

doctest> Maybe.map toString <| Maybe.map2 add (fromString "1") (fromString "1")
Just "2.00" : Maybe String

doctest> Maybe.map toString <| Maybe.map2 add (fromString "1.23") (fromString "1.23")
Just "2.46" : Maybe String

doctest> Maybe.map toString <| Maybe.map2 add (fromString "1.23") (fromString "1.20")
Just "2.43" : Maybe String

doctest> Maybe.map toString <| Maybe.map2 add (fromString "1.23") (fromString "1.2")
Just "2.43" : Maybe String

-}
add : Dec Int -> Dec Int -> Dec Int
add a b =
    case ( a, b ) of
        ( Zero, b_ ) ->
            b_

        ( a_, Zero ) ->
            a_

        ( MkSign a2, MkSign b2 ) ->
            let
                maxPrecision =
                    max (getPrecision a2) (getPrecision b2)

                valueA =
                    toSmallestUnits maxPrecision a2

                valueB =
                    toSmallestUnits maxPrecision b2

                sumValue =
                    valueA + valueB
            in
            fromSmallestUnits maxPrecision sumValue


getPrecision : Sign { whole : a, fract : a, precision : Int } -> Int
getPrecision sign =
    case sign of
        Pos { precision } ->
            precision

        Neg { precision } ->
            precision


toSmallestUnits : Int -> Sign { whole : Int, fract : Int, precision : Int } -> Int
toSmallestUnits targetPrecision sign =
    case sign of
        Pos { whole, fract, precision } ->
            let
                scaleFactor =
                    10 ^ targetPrecision

                scaledWhole =
                    whole * scaleFactor

                scaledFract =
                    fract * (10 ^ (targetPrecision - precision))
            in
            scaledWhole + scaledFract

        Neg { whole, fract, precision } ->
            let
                scaleFactor =
                    10 ^ targetPrecision

                scaledWhole =
                    whole * scaleFactor

                scaledFract =
                    fract * (10 ^ (targetPrecision - precision))
            in
            -(scaledWhole + scaledFract)


fromSmallestUnits : Int -> Int -> Dec Int
fromSmallestUnits precision value =
    if value == 0 then
        Zero

    else
        let
            scaleFactor =
                10 ^ precision

            absValue =
                abs value

            whole =
                absValue // scaleFactor

            fract =
                modBy scaleFactor absValue
        in
        if value < 0 then
            MkSign (Neg { whole = whole, fract = fract, precision = precision })

        else
            MkSign (Pos { whole = whole, fract = fract, precision = precision })
