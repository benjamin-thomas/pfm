module Page.UI exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA



-- You'll need to import any types used in the view function
-- For now, this is minimal since your UI page is simple


view : Html msg
view =
    H.div []
        [ H.p [] [ H.text "UI" ]
        , H.a [ HA.href "/" ] [ H.text "Go back to root" ]
        ]
