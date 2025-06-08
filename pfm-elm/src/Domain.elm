module Domain exposing
    ( Account
    , CategoryOld2
    , TransactionViewWithBalance
    )

import Decimal exposing (Decimal)
import Time


type alias CategoryOld2 =
    { name : String
    }


type alias Account =
    { name : String
    , category : CategoryOld2
    }


type alias TransactionViewWithBalance =
    { date : Time.Posix
    , descr : String
    , from : Account
    , to : Account
    , amount : Decimal
    , balanceMovement : { from : Decimal, to : Decimal }
    }
