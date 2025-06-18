module Domain exposing
    ( AccountOld
    , CategoryOld2
    , TransactionViewWithBalance
    )

import Decimal exposing (Decimal)
import Time


type alias CategoryOld2 =
    { name : String
    }


type alias AccountOld =
    { name : String
    , category : CategoryOld2
    }


type alias TransactionViewWithBalance =
    { date : Time.Posix
    , descr : String
    , from : AccountOld
    , to : AccountOld
    , amount : Decimal
    , balanceMovement : { from : Decimal, to : Decimal }
    }
