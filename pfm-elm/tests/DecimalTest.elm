module DecimalTest exposing (..)

import Decimal exposing (..)
import Expect
import Test exposing (..)



-- TODO: add some property tests


normalizeTest : Test
normalizeTest =
    describe "normalize fracts returns a simplified ratio a over b, but only by factors of 10"
        [ test "(10, 100) simplifies to (1,10)" <|
            \_ ->
                normalize ( 10, 100 ) |> Expect.equal ( 1, 10 )
        , test "(500, 550) simplifies to (50, 55)" <|
            \_ -> normalize ( 500, 550 ) |> Expect.equal ( 50, 55 )
        , test "(0,0) simplifies to (0,0)" <|
            \_ -> normalize ( 0, 0 ) |> Expect.equal ( 0, 0 )
        , test "(0,1) simplifies to (0,1)" <|
            \_ -> normalize ( 0, 1 ) |> Expect.equal ( 0, 1 )
        , test "(1,0) simplifies to (1,0)" <|
            \_ -> normalize ( 1, 0 ) |> Expect.equal ( 1, 0 )
        , test "(1,1) simplifies to (1,1)" <|
            \_ -> normalize ( 1, 1 ) |> Expect.equal ( 1, 1 )
        , test "(10,230) simplifies to (1,23)" <|
            \_ -> normalize ( 10, 230 ) |> Expect.equal ( 1, 23 )
        , test "(100,2300) simplifies to (1,23)" <|
            \_ -> normalize ( 100, 2300 ) |> Expect.equal ( 1, 23 )
        , test "(123,0) simplifies to (123,0)" <|
            \_ -> normalize ( 123, 0 ) |> Expect.equal ( 123, 0 )
        , test "(123,10) simplifies to (123,10)" <|
            \_ -> normalize ( 123, 10 ) |> Expect.equal ( 123, 10 )
        ]


addTest : Test
addTest =
    let
        t { title, expected } f =
            test title <|
                \_ ->
                    f
                        |> (\result -> ( decToString result, result ))
                        |> Expect.equal expected
    in
    describe "Adding"
        [ t
            { title = "1.0 + 2.0 = 3.0 "
            , expected =
                ( "3.0"
                , MkDecimalFull { whole = 3, fract = 0, precision = 1 }
                )
            }
            (add (MkDecimal 1 0) (MkDecimal 2 0))
        , t
            { title = "2.5 + 2.5 = 5.0"
            , expected =
                ( "5.0"
                , MkDecimalFull { whole = 5, fract = 0, precision = 1 }
                )
            }
            (add (MkDecimal 2 5) (MkDecimal 2 5))
        , t
            { title = "2.5 + 2.55 = 5.05"
            , expected =
                ( "5.05"
                , MkDecimalFull { whole = 5, fract = 5, precision = 2 }
                )
            }
            (add (MkDecimal 2 5) (MkDecimal 2 55))
        , t
            { title = "2.50 + 2.55 = 5.05"
            , expected =
                ( "5.05"
                , MkDecimalFull { whole = 5, fract = 5, precision = 2 }
                )
            }
            (add (MkDecimal 2 50) (MkDecimal 2 55))
        , t
            { title = "2.500 + 2.550 = 5.05"
            , expected =
                ( "5.05"
                , MkDecimalFull { whole = 5, fract = 5, precision = 2 }
                )
            }
            (add (MkDecimal 2 500) (MkDecimal 2 550))
        , t
            { title = "0.1 + 0.222 = 0.322"
            , expected =
                ( "0.322"
                , MkDecimalFull { whole = 0, fract = 322, precision = 3 }
                )
            }
            (add (MkDecimal 0 1) (MkDecimal 0 222))
        , t
            { title = "2.5 + -2.5 = 0.0"
            , expected =
                ( "0.0"
                , MkDecimalFull { whole = 0, fract = 0, precision = 1 }
                )
            }
            (add (MkDecimal 2 5) (MkDecimal -2 5))
        , t
            { title = "-2.5 + 2.5 = 0.0"
            , expected =
                ( "0.0"
                , MkDecimalFull { whole = 0, fract = 0, precision = 1 }
                )
            }
            (add (MkDecimal -2 5) (MkDecimal 2 5))
        , t
            { title = "-5.25 + 5.25 = 0.0"
            , expected =
                ( "0.00"
                , MkDecimalFull { whole = 0, fract = 0, precision = 2 }
                )
            }
            (add (MkDecimal -5 25) (MkDecimal 5 25))
        ]
