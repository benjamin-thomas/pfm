module Dec2 exposing (..)


type Dec
    = Dec Int Int -- mantissa, exponent


fromString : String -> Maybe Dec
fromString str =
    case String.split "." str of
        [ whole, fract ] ->
            let
                mantissa =
                    String.toInt (whole ++ fract)

                exponent =
                    -(String.length fract)
            in
            Maybe.map
                (\m -> Dec m exponent)
                mantissa

        [ whole ] ->
            Maybe.map (\m -> Dec m 0) (String.toInt whole)

        _ ->
            Nothing


toString : Dec -> String
toString (Dec m e) =
    let
        s =
            String.fromInt (abs m)

        sign =
            if m < 0 then
                "-"

            else
                ""
    in
    if e == 0 then
        sign ++ s

    else if e > 0 then
        sign ++ s ++ String.repeat e "0"

    else
        sign ++ String.dropRight -e s ++ "." ++ String.right -e s


add : Dec -> Dec -> Dec
add (Dec ma ea) (Dec mb eb) =
    let
        commonExp =
            min ea eb

        maScaled =
            ma * 10 ^ (ea - commonExp)

        mbScaled =
            mb * 10 ^ (eb - commonExp)
    in
    Dec (maScaled + mbScaled) commonExp
