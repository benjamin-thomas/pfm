module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , parser
    , pushUrl
    , replaceUrl
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as HA
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)



-- ROUTING


type Route
    = Home
    | UI
    | NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home top
        , Parser.map UI (s "ui")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    HA.href (routeToString targetRoute)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Route
fromUrl url =
    -- The RealWorld spec treats the fragment as a path.
    -- This makes it easier for people to share URLs.
    -- However the fragment is not part of the URL path, so we
    -- need to handle that ourselves by looking at the fragment and
    -- creating a new URL with the fragment as the path.
    case Parser.parse parser url of
        Just route ->
            route

        Nothing ->
            NotFound



-- INTERNAL


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                UI ->
                    [ "ui" ]

                NotFound ->
                    [ "not-found" ]
    in
    "/" ++ String.join "/" pieces
