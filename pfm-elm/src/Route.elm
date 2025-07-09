module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , parser
    )

import Html exposing (Attribute)
import Html.Attributes as HA
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)


type Route
    = NotFound
    | Home
    | UI
    | BudgetList
    | BudgetEdit Int


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Home P.top
        , P.map BudgetEdit (P.s "budgets" </> P.int) </> P.s "edit"
        , P.map BudgetList (P.s "budgets")
        , P.map UI (P.s "ui")
        ]


href : Route -> Attribute msg
href targetRoute =
    HA.href (routeToString targetRoute)


fromUrl : Url -> Route
fromUrl url =
    case P.parse parser url of
        Just route ->
            route

        Nothing ->
            NotFound


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "/"

        BudgetList ->
            "/budgets"

        BudgetEdit id ->
            "/budgets/" ++ String.fromInt id ++ "/edit"

        UI ->
            "/ui"

        NotFound ->
            "/not-found"
