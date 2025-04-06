module Domain exposing
    ( Account
    , Category
    , TransactionViewWithBalance
    )

import Decimal exposing (Decimal)
import Time


type alias Category =
    { name : String
    }


type alias Account =
    { name : String
    , category : Category
    }


type alias TransactionViewWithBalance =
    { date : Time.Posix
    , descr : String
    , from : Account
    , to : Account
    , amount : Decimal
    , balanceMovement : { from : Decimal, to : Decimal }
    }
