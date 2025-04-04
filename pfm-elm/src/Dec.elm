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



{- Simplifies fractions whose elements are multiples of 10

   doctest-setup> import Dec exposing (..)

   \_doctest> normalize (Zero, Zero)
   (Zero,Zero) : ( Dec, Dec )

   \_doctest> normalize (Pos 10 230, Pos 10 230)
   (Pos 1 23,Pos 1 23) : ( Dec, Dec )

-}
-- normalize : ( Dec, Dec ) -> ( Dec, Dec )
-- normalize ( a, b ) =
--     case ( a, b ) of
--         ( Zero, Zero ) ->
--             ( Zero, Zero )
--         _ ->
--             Debug.todo "x"
