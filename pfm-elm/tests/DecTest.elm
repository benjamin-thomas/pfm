module DecTest exposing (..)

import Dec exposing (Dec(..))
import Expect
import Test exposing (..)


addTest : Test
addTest =
    let
        t a b =
            Maybe.map Dec.toString <|
                Maybe.map2 Dec.add
                    (Dec.fromString a)
                    (Dec.fromString b)
    in
    describe "addTest"
        [ test "0 + 0 = 0" <|
            \_ ->
                t "0" "0" |> Expect.equal (Just "0.00")
        , test "0 + 1 = 1" <|
            \_ ->
                t "0" "1" |> Expect.equal (Just "1.00")
        , test "1 + 0 = 1" <|
            \_ ->
                t "1" "0" |> Expect.equal (Just "1.00")
        , test "1 + 1 = 2" <|
            \_ ->
                t "1" "1" |> Expect.equal (Just "2.00")
        , test "0.1 + 0.2 = 0.3" <|
            \_ ->
                t "0.1" "0.2"
                    |> Expect.equal
                        (Just "0.30")
        , test "1.23 + 1.23 = 2.46" <|
            \_ ->
                t "1.23" "1.23" |> Expect.equal (Just "2.46")
        , test "10 + -8 = 2" <|
            \_ ->
                t "10" "-8" |> Expect.equal (Just "2.00")
        , test "8 + -10 = -2" <|
            \_ ->
                t "8" "-10" |> Expect.equal (Just "-2.00")
        , test "-8 + 10 = 2" <|
            \_ ->
                t "-8" "10" |> Expect.equal (Just "2.00")
        , test "-10 + 8 = -2" <|
            \_ ->
                t "-10" "8" |> Expect.equal (Just "-2.00")
        , test "1.23 + 1.20 = 2.43" <|
            \_ ->
                t "1.23" "1.20" |> Expect.equal (Just "2.43")
        , test "1.23 + 1.2 = 2.43" <|
            \_ ->
                t "1.23" "1.2" |> Expect.equal (Just "2.43")
        , test "-1 + -1 = -2" <|
            \_ ->
                t "-1" "-1" |> Expect.equal (Just "-2.00")
        , test "-1 + 1 = 0" <|
            \_ ->
                t "-1" "1" |> Expect.equal (Just "0.00")
        , test "1 + -1 = 0" <|
            \_ ->
                t "1" "-1" |> Expect.equal (Just "0.00")
        , test "-0.23 + 0.23 = 0" <|
            \_ ->
                t "-0.23" "0.23" |> Expect.equal (Just "0.00")
        , test "0.23 + -0.23 = 0" <|
            \_ ->
                t "0.23" "-0.23" |> Expect.equal (Just "0.00")
        , test "-0.1 + -0.2 = -0.3" <|
            \_ ->
                t "-0.1" "-0.2" |> Expect.equal (Just "-0.30")
        , test "1 + -2 = -1" <|
            \_ ->
                t "1" "-2" |> Expect.equal (Just "-1.00")
        , test "-2 + 1 = -1" <|
            \_ ->
                t "-2" "1" |> Expect.equal (Just "-1.00")
        , test "0.5 + -0.75 = -0.25" <|
            \_ ->
                t "0.5" "-0.75" |> Expect.equal (Just "-0.25")
        ]
