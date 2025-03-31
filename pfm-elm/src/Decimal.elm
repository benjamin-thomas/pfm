module Decimal exposing
    ( Decimal(..)
    , DecimalFull(..)
    , add
    , decToString
    , normalize
    )


type Decimal
    = MkDecimal Int Int


type DecimalFull
    = MkDecimalFull
        { whole : Int
        , fract : Int
        , precision : Int
        }


{-| Normalizes a fraction (a over b) to a simplified ratio (only for base 10 values).

    normalize ( 10, 230 )
    -- => ( 1, 23 )

    normalize ( 123, 0 )
    -- => ( 123, 0 )

-}
normalize : ( Int, Int ) -> ( Int, Int )
normalize ( a, b ) =
    if a == 0 || b == 0 then
        ( a, b )

    else
        case ( modBy 10 a, modBy 10 b ) of
            ( 0, 0 ) ->
                normalize ( a // 10, b // 10 )

            _ ->
                ( a, b )


precisionFor : Int -> Int
precisionFor n =
    1 + (truncate <| logBase 10 <| toFloat <| abs n)


scale : Int -> Int -> Int
scale n p =
    if n == 0 || precisionFor n >= p then
        n

    else
        scale (n * 10) p


add : Decimal -> Decimal -> DecimalFull
add (MkDecimal aWhole aFract) (MkDecimal bWhole bFract) =
    let
        ( aFractN, bFractN ) =
            normalize ( aFract, bFract )
    in
    addStep1 (MkDecimal aWhole aFractN) (MkDecimal bWhole bFractN)


addStep1 : Decimal -> Decimal -> DecimalFull
addStep1 (MkDecimal aWhole aFract) (MkDecimal bWhole bFract) =
    let
        precision =
            max
                (precisionFor aFract)
                (precisionFor bFract)
    in
    addStep2 precision
        (MkDecimal aWhole (scale aFract precision))
        (MkDecimal bWhole (scale bFract precision))


addStep2 : Int -> Decimal -> Decimal -> DecimalFull
addStep2 precision (MkDecimal aWhole aFract) (MkDecimal bWhole bFract) =
    let
        ref =
            10 ^ precision

        whole =
            aWhole + bWhole

        sign whole_ fract_ =
            if whole_ < 0 then
                fract_ * -1

            else
                fract_

        overflow =
            (sign aWhole aFract + sign bWhole bFract) // ref

        rest =
            modBy ref (aFract + bFract)
    in
    MkDecimalFull
        { whole = whole + overflow
        , fract = rest
        , precision = precision
        }


decToString : DecimalFull -> String
decToString (MkDecimalFull { whole, fract, precision }) =
    String.concat
        [ String.fromInt whole
        , "."
        , String.padLeft precision '0' (String.fromInt fract)
        ]
