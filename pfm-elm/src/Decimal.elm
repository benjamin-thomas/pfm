module Decimal exposing
    ( Decimal(..)
    , DecimalFull(..)
    , add
    , decToString
    , normalize
    , precisionFor
    )


type Decimal
    = MkDecimal Int Int


type DecimalFull
    = MkDecimalFull
        { whole : Int
        , fract : Int
        , precision : Int
        }



{-

   doctest-setup> import Decimal exposing (..)
   _doctest-interact>

-}


{-| Normalizes a fraction (a over b) to a simplified ratio, but only by factors of 10.

doctest> normalize (10, 230)
(1,23) : ( Int, Int )

doctest> normalize (123, 0)
(123,0) : ( Int, Int )

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


{-| Returns the "base 10 width" of a number. Equivalent to (String.length << String.fromInt).

doctest> precisionFor 1
1 : Int

doctest> precisionFor 12
2 : Int

doctest> precisionFor 123
3 : Int

doctest> precisionFor 1234567890
10 : Int

-}
precisionFor : Int -> Int
precisionFor n =
    1 + (truncate <| logBase 10 <| toFloat <| abs n)


scale : Int -> Int -> Int
scale n p =
    if n == 0 || precisionFor n >= p then
        n

    else
        scale (n * 10) p


{-| Adds two decimal numbers.

doctest> decToString <| add (MkDecimal 0 1) (MkDecimal 0 2)
"0.3" : String

doctest> decToString <| add (MkDecimal 3 3) (MkDecimal 3 33)
"6.63" : String

doctest> decToString <| add (MkDecimal 3 30) (MkDecimal 3 33)
"6.63" : String

doctest> decToString <| add (MkDecimal 3 300) (MkDecimal 3 330)
"6.63" : String

doctest> decToString <| add (MkDecimal 5 5) (MkDecimal 5 5)
"11.0" : String

doctest> decToString <| add (MkDecimal 5 5) (MkDecimal -5 5)
"0.0" : String

doctest> decToString <| add (MkDecimal -5 5) (MkDecimal 5 5)
"0.0" : String

doctest> decToString <| add (MkDecimal -5 25) (MkDecimal 5 25)
"0.00" : String

doctest> decToString <| add (MkDecimal -5 255) (MkDecimal 5 255)
"0.000" : String

doctest> decToString <| add (MkDecimal -5 250) (MkDecimal 5 255)
"0.005" : String

-}
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
addStep2 precision (MkDecimal aWhole aRawFract) (MkDecimal bWhole bRawFract) =
    let
        ref =
            10 ^ precision

        whole =
            Debug.log "whole"
                (Debug.log "aWhole" aWhole + Debug.log "bWhole" bWhole)

        sign whole_ fract_ =
            if whole_ < 0 then
                fract_ * -1

            else
                fract_

        aFract =
            Debug.log "aFract"
                (sign aWhole aRawFract)

        bFract =
            Debug.log "bFract"
                (sign bWhole bRawFract)

        overflow =
            Debug.log "overflow"
                ((aFract + bFract) // ref)

        newFract =
            -- Debug.log "newFract" <|
            modBy ref <|
                abs (aFract + bFract)
    in
    MkDecimalFull
        { whole = whole + overflow
        , fract = newFract
        , precision = precision
        }


decToString : DecimalFull -> String
decToString (MkDecimalFull { whole, fract, precision }) =
    String.concat
        [ String.fromInt whole
        , "."
        , String.padLeft precision '0' (String.fromInt fract)
        ]
