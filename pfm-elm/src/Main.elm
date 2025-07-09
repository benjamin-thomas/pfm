port module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Generated.Decoder
    exposing
        ( decodeAccountBalanceRead
        , decodeAccountRead
        , decodeCategory
        , decodeLedgerLine
        , decodeSuggestion
        )
import Generated.Encoder exposing (encodeSuggestionWrite, encodeTransactionWrite)
import Generated.Types exposing (AccountBalanceRead, AccountRead, Category, LedgerLine, SuggestedAccount, Suggestion, SuggestionWrite, TransactionWrite)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Page.Budget.Edit as BudgetEditPage
import Page.Budget.List as BudgetListPage
import Page.UI as UI_Page
import Process
import Route exposing (Route)
import Task exposing (Task)
import Time
import Url exposing (Url)
import Utils


port enterPressed : (() -> msg) -> Sub msg


port escapePressed : (() -> msg) -> Sub msg


port rcvScrollY : (Int -> msg) -> Sub msg


port consoleLogRaw : E.Value -> Cmd msg


consoleLog : String -> E.Value -> Cmd msg
consoleLog str value =
    consoleLogRaw <|
        E.object
            [ ( "title", E.string str )
            , ( "data", value )
            ]


port toggleTheme : () -> Cmd msg


port showDialog : () -> Cmd msg


port closeDialog : () -> Cmd msg


port restoreScrollY : Int -> Cmd msg


{-| http -v localhost:8080/categories
-}
fetchCategoriesTask : Task Http.Error (List Category)
fetchCategoriesTask =
    Http.task
        { method = "GET"
        , headers = []
        , url = baseUrl ++ "categories"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| D.list decodeCategory
        , timeout = Nothing
        }


baseUrl : String
baseUrl =
    "http://localhost:8080/"


handleJsonResponse : D.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


{-| http -v localhost:8080/transactions/ accountId==2
-}
fetchLedgerLinesTask : Task Http.Error (List LedgerLine)
fetchLedgerLinesTask =
    Http.task
        { method = "GET"
        , headers = []
        , url = baseUrl ++ "transactions?accountId=2"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| D.list decodeLedgerLine
        , timeout = Nothing
        }


{-| http -v localhost:8080/transactions/suggestions fromAccountId==2 toAccountId==6

FIXME: hard coded values

-}
fetchSuggestionsTask : Task Http.Error (List Suggestion)
fetchSuggestionsTask =
    -- FIXME: hard coded values
    Http.task
        { method = "GET"
        , headers = []
        , url = baseUrl ++ "transactions/suggestions?fromAccountId=2&toAccountId=6"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| D.list decodeSuggestion
        , timeout = Nothing
        }


fetchAccountsTask : Task Http.Error (List AccountRead)
fetchAccountsTask =
    Http.task
        { method = "GET"
        , headers = []
        , url = baseUrl ++ "accounts"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| D.list decodeAccountRead
        , timeout = Nothing
        }


fetchBalancesTask : Task Http.Error (List AccountBalanceRead)
fetchBalancesTask =
    Http.task
        { method = "GET"
        , headers = []
        , url = baseUrl ++ "accounts/balances?accountIds=2,3" -- FIXME: find a way this param
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| D.list decodeAccountBalanceRead
        , timeout = Nothing
        }


postTransaction : TransactionWrite -> Cmd HomeMsg
postTransaction transaction =
    Http.post
        { url = "http://localhost:8080/transactions"
        , body = Http.jsonBody (encodeTransactionWrite transaction)
        , expect =
            Http.expectWhatever
                (CreateDialogChanged << GotCreateSaveResponse)
        }


putTransaction : ( Int, TransactionWrite ) -> (Result Http.Error () -> msg) -> Cmd msg
putTransaction ( transactionId, transaction ) nextMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8080/transactions/" ++ String.fromInt transactionId
        , body = Http.jsonBody (encodeTransactionWrite transaction)
        , expect =
            Http.expectWhatever nextMsg
        , timeout = Nothing
        , tracker = Nothing
        }


applyAllSuggestions : List ( Int, SuggestedAccount ) -> Cmd HomeMsg
applyAllSuggestions toUpdate =
    let
        toSuggestionWrite : ( Int, SuggestedAccount ) -> SuggestionWrite
        toSuggestionWrite ( transactionId, suggestedAccount ) =
            { transactionId = transactionId
            , toAccountId = suggestedAccount.accountId
            }

        bodyVal : E.Value
        bodyVal =
            toUpdate
                |> List.map toSuggestionWrite
                |> E.list encodeSuggestionWrite
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "http://localhost:8080/transactions/apply-all-suggestions"
        , body = Http.jsonBody bodyVal
        , expect =
            Http.expectWhatever GotApplyAllSuggestionsResponse
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTransaction : { transactionId : Int } -> Cmd HomeMsg
deleteTransaction { transactionId } =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:8080/transactions/" ++ String.fromInt transactionId
        , body = Http.emptyBody
        , expect = Http.expectWhatever (EditDialogChanged << GotDeleteResponse)
        , timeout = Nothing
        , tracker = Nothing
        }


type alias MkEditDialog =
    { transactionId : Int
    , budgetId : Int
    , fromAccountId : Int
    , toAccountId : Int
    , amount : String
    , date : Time.Posix
    , descr : String
    , showTime : Bool
    }


type alias MkCreateDialog =
    { fromAccountId : Int
    , toAccountId : Int
    , date : Time.Posix
    , descr : String
    , amount : String
    , showTime : Bool
    }


type Dialog
    = EditDialog MkEditDialog
    | CreateDialog MkCreateDialog


type Status a
    = Loading
    | Failed
    | Loaded a


type SearchBy
    = ByDescr String
    | BySimilarTo ContextMenuData


type alias SearchForm =
    { searchBy : SearchBy
    , minAmount : String
    , maxAmount : String
    , filterUnknownExpenses : Bool
    }


type alias Data =
    { categories : List Category
    , ledgerLines : List LedgerLine
    , accounts : List AccountRead
    , balances : List AccountBalanceRead
    , suggestions : List Suggestion
    }


type alias ContextMenuPos =
    { x : Int
    , y : Int
    }


type alias ContextMenuData =
    { soundexDescr : String
    , descr : String
    }


type alias ContextMenu =
    { pos : ContextMenuPos
    , data : ContextMenuData
    }


type alias Model =
    { key : Nav.Key
    , page : Page
    , now : Time.Posix
    , zone : Time.Zone
    , isDarkTheme : Bool
    }


type MkEditDialogChanged
    = EditDescrChanged String
    | EditFromChanged Int
    | EditToChanged Int
    | EditAmountChanged String
    | EditDateChanged Time.Posix
    | EditToggleTimeDisplay
    | EditDialogSave
    | GotEditSaveResponse (Result Http.Error ())
    | GotDeleteResponse (Result Http.Error ())
    | DeleteTransactionBtnPressed { transactionId : Int }


type MkCreateDialogChanged
    = CreateDescrChanged String
    | CreateFromChanged Int
    | CreateToChanged Int
    | CreateAmountChanged String
    | CreateDateChanged Time.Posix
    | CreateToggleTimeDisplay
    | CreateDialogSave
    | GotCreateSaveResponse (Result Http.Error ())


type HomeMsg
    = NoOp
    | ShowContextMenu ContextMenu
    | HideContextMenu
    | FindSimilarSelected ContextMenuData
    | RequestCursorRestore { scrollY : Int }
    | Fetching { scrollY : Int } (List (Cmd HomeMsg))
    | GotData (Result Http.Error Data)
    | EditTransactionClicked ( Int, TransactionWrite )
    | ApplySuggestedExpenseClicked { ledgerLine : LedgerLine, toAccountId : Int }
    | ApplyAllSuggestionsBtnClicked (List ( Int, SuggestedAccount ))
    | GotApplyExpenseResponse (Result Http.Error ())
    | GotApplyAllSuggestionsResponse (Result Http.Error ())
    | EditDialogChanged MkEditDialogChanged
    | CreateDialogChanged MkCreateDialogChanged
    | AddTransactionClicked
    | OpenCreateDialog Time.Posix
    | CloseDialogPressed
    | EnterPressed
    | EscapePressed
    | RcvScrollY Int
    | GotSearchFormMsg SearchFormMsg


type Msg
    = GotHomeMsg HomeMsg
    | GotBudgetListPageMsg BudgetListPage.Msg
    | GotBudgetEditPageMsg BudgetEditPage.Msg
    | ToggleTheme
    | UrlRequested UrlRequest
    | UrlChanged Url
    | GotZone Time.Zone


type SearchFormMsg
    = SearchDescrChanged String
    | SearchMinAmountChanged String
    | SearchMaxAmountChanged String
    | SearchUnknownExpensesClicked
    | ClearSearchFormBtnPressed


viewContextMenu : Maybe ContextMenu -> Html HomeMsg
viewContextMenu contextMenu =
    case contextMenu of
        Just { pos, data } ->
            H.div
                [ HA.class "context-menu"
                , HA.style "top" (String.fromInt pos.y ++ "px")
                , HA.style "left" (String.fromInt pos.x ++ "px")
                ]
                [ H.div [ HA.class "context-menu-debug" ]
                    [ H.text <| "SOUNDEX: " ++ data.soundexDescr ]
                , H.ul []
                    [ H.li
                        [ HE.onClick
                            (FindSimilarSelected data)
                        ]
                        [ H.text "Find similar transactions" ]
                    , H.li [ HE.onClick HideContextMenu ] [ H.text "Something else..." ]
                    ]
                ]

        Nothing ->
            H.text ""


type Page
    = MkHomePage HomeModel
    | MkUI_Page ()
    | MkBudgetListPage BudgetListPage.Model
    | MkBudgetEditPage BudgetEditPage.Model
    | MkNotFoundPage


view : Model -> Browser.Document Msg
view model =
    let
        viewPage : Html Msg
        viewPage =
            case model.page of
                MkNotFoundPage ->
                    H.div [] [ H.text "Not Found" ]

                MkHomePage pageModel ->
                    H.map
                        GotHomeMsg
                        (viewHome pageModel)

                MkUI_Page () ->
                    UI_Page.view

                MkBudgetListPage pageModel ->
                    H.map
                        GotBudgetListPageMsg
                        (BudgetListPage.view pageModel)

                MkBudgetEditPage pageModel ->
                    H.map
                        GotBudgetEditPageMsg
                        (BudgetEditPage.view pageModel)
    in
    { title = "Personal Finance Manager"
    , body =
        [ H.div
            [ HA.classList
                [ ( "app", True )
                , ( "dark-theme", model.isDarkTheme )
                ]
            ]
            [ themeToggleButton model.isDarkTheme
            , viewPage
            ]
        ]
    }


themeToggleButton : Bool -> Html Msg
themeToggleButton isDarkTheme =
    H.button
        [ HA.class "theme-toggle"
        , HE.onClick ToggleTheme
        , HA.title
            (if isDarkTheme then
                "Switch to Light Mode"

             else
                "Switch to Dark Mode"
            )
        ]
        [ H.span []
            [ H.text
                (if isDarkTheme then
                    "☀️"

                 else
                    "🌙"
                )
            ]
        ]


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog =
    H.node "dialog"


balanceCard : AccountBalanceRead -> Html HomeMsg
balanceCard { categoryName, accountName, accountBalance } =
    let
        colorAccent =
            if categoryName == "Assets" then
                "#3498db"

            else if categoryName == "Expenses" then
                "#e74c3c"

            else
                "#9b59b6"
    in
    H.div
        [ HA.class "balance-card", HA.style "border-left-color" colorAccent ]
        [ H.div [ HA.class "balance-card__category" ] [ H.text categoryName ]
        , H.div [ HA.class "balance-card__account" ] [ H.text accountName ]
        , H.div [ HA.class "balance-card__amount" ]
            [ H.text <|
                Utils.amountFmt2 <|
                    { intPart = accountBalance // 100
                    , decPart = modBy 100 accountBalance
                    }
            ]
        ]


attachPriorBalance : List LedgerLine -> List ( LedgerLine, ( Int, String ) )
attachPriorBalance transactions =
    case transactions of
        [] ->
            []

        first :: rest ->
            ( first, ( 0, "0.00" ) )
                :: List.map2
                    (\tx prevTx ->
                        ( tx
                        , ( prevTx.runningBalanceCents
                          , prevTx.runningBalance
                          )
                        )
                    )
                    rest
                    transactions


toTransactionWrite : LedgerLine -> ( Int, TransactionWrite )
toTransactionWrite { budgetId, transactionId, fromAccountId, toAccountId, dateUnix, descr, flowCents } =
    ( transactionId
    , { budgetId = Just budgetId
      , fromAccountId = fromAccountId
      , toAccountId = toAccountId
      , dateUnix = dateUnix
      , descr = descr
      , cents = abs flowCents
      }
    )


viewOneTransaction :
    ( LedgerLine, ( Int, String ), List SuggestedAccount )
    -> Html HomeMsg
viewOneTransaction ( ledgerLine, ( _, priorBalance ), suggestedAccounts ) =
    let
        isPositive =
            --Decimal.gt tx.amount Decimal.zero && tx.from /= checkingAccount
            ledgerLine.flowCents > 0

        amountClass =
            if isPositive then
                "transaction-item__amount transaction-item__amount--positive"

            else
                "transaction-item__amount transaction-item__amount--negative"

        amountSign =
            if isPositive then
                "+"

            else
                ""
    in
    H.li
        [ HA.class "transaction-item"
        , HE.onClick (EditTransactionClicked <| toTransactionWrite ledgerLine)
        , HE.preventDefaultOn "contextmenu"
            (D.map2
                (\x y ->
                    ( ShowContextMenu
                        { pos =
                            { x = x
                            , y = y
                            }
                        , data =
                            { soundexDescr = ledgerLine.soundexDescr
                            , descr = ledgerLine.descr
                            }
                        }
                    , True
                    )
                )
                (D.field "pageX" D.int)
                (D.field "pageY" D.int)
            )
        ]
        [ H.div [ HA.class "transaction-item__row" ]
            [ H.div [ HA.class "transaction-item__main-content" ]
                [ H.div [ HA.class "transaction-item__details" ]
                    [ H.div [ HA.class "transaction-item__description" ]
                        [ H.text ledgerLine.descr ]
                    , H.div [ HA.class "transaction-item__accounts" ]
                        [ H.text (ledgerLine.fromAccountName ++ " → " ++ ledgerLine.toAccountName) ]
                    ]
                , H.div [ HA.class "transaction-item__date" ]
                    [ H.text (Utils.dateFmtUnix ledgerLine.dateUnix) ]
                , H.div [ HA.class amountClass ]
                    [ H.text (amountSign ++ ledgerLine.flow ++ "\u{00A0}€") ]
                ]
            , H.div [ HA.class "transaction-item__balance-column" ]
                [ H.div [ HA.class "transaction-item__balance-movement" ]
                    [ H.span [ HA.class "balance-before" ] [ H.text <| priorBalance ++ "\u{00A0}€" ]
                    , H.span [ HA.class "arrow-icon" ] [ H.text " → " ]
                    , H.span [ HA.class "balance-after" ] [ H.text <| ledgerLine.runningBalance ++ "\u{00A0}€" ]
                    ]
                ]
            ]
        , viewIf (shouldSuggest ledgerLine) <|
            \() ->
                case suggestedAccounts of
                    [] ->
                        H.text ""

                    mostLikely :: _ ->
                        -- Suggestion UI shown for unknown expenses
                        H.div
                            [ HA.class "suggestion-container"

                            -- Don't open the edit dialog
                            , HE.stopPropagationOn "click" (D.succeed ( NoOp, True ))
                            ]
                            [ H.div [ HA.class "suggestion-text" ]
                                [ H.span [ HA.class "suggestion-icon" ] [ H.text "💡" ]
                                , H.span []
                                    [ H.text "Suggested category: "
                                    , H.strong [] [ H.text mostLikely.accountName ]
                                    ]
                                ]
                            , H.div [ HA.class "suggestion-actions" ]
                                [ H.button
                                    [ HA.class "suggestion-btn suggestion-btn-apply"

                                    -- Don't open the edit dialog
                                    , HE.stopPropagationOn "click"
                                        (D.succeed
                                            ( ApplySuggestedExpenseClicked
                                                { ledgerLine = ledgerLine, toAccountId = mostLikely.accountId }
                                            , True
                                            )
                                        )
                                    ]
                                    [ H.text "Apply" ]
                                , H.button
                                    [ HA.class "suggestion-btn suggestion-btn-ignore" ]
                                    [ H.text "Ignore" ]
                                , H.button
                                    [ HA.class "suggestion-btn suggestion-btn-ai" ]
                                    [ H.text "Ask AI "
                                    , H.span [ HA.class "ai-icon" ] [ H.text "✨" ]
                                    ]
                                ]
                            ]
        ]


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf condition fn =
    if condition then
        fn ()

    else
        H.text ""


shouldSuggest : LedgerLine -> Bool
shouldSuggest tx =
    tx.toAccountName == "Unknown_EXPENSE"


viewLedgerLines : Maybe ContextMenu -> List Suggestion -> SearchForm -> List LedgerLine -> Html HomeMsg
viewLedgerLines contextMenu allSuggestions searchForm withRunningBalanceEntity =
    let
        filteredWithPriorBalance : List ( LedgerLine, ( Int, String ) )
        filteredWithPriorBalance =
            withRunningBalanceEntity
                |> List.filter (transactionMatchesFilters searchForm)
                |> attachPriorBalance

        filteredData : ( List ( LedgerLine, ( Int, String ), List SuggestedAccount ), Int )
        filteredData =
            let
                soundDescrToSuggestedAccounts : Dict String (List SuggestedAccount)
                soundDescrToSuggestedAccounts =
                    allSuggestions
                        |> List.map (\s -> ( s.soundexDescr, s.suggestedAccounts ))
                        |> Dict.fromList
            in
            filteredWithPriorBalance
                |> List.foldl
                    (\( ledgerLine, tup2balance ) ( accList, accCnt ) ->
                        let
                            suggestedAccounts =
                                if shouldSuggest ledgerLine then
                                    Maybe.withDefault [] <|
                                        Dict.get
                                            ledgerLine.soundexDescr
                                            soundDescrToSuggestedAccounts

                                else
                                    []
                        in
                        ( ( ledgerLine, tup2balance, suggestedAccounts ) :: accList
                        , accCnt + List.length suggestedAccounts
                        )
                    )
                    ( [], 0 )

        ( filteredWithPriorBalanceWithSuggestions, totalSuggestedCount ) =
            filteredData

        -- FIXME: I could avoid a second loop here!
        totalCount =
            List.length withRunningBalanceEntity

        filteredCount =
            List.length filteredWithPriorBalance

        countText =
            if filteredCount == totalCount then
                String.fromInt totalCount ++ " transactions"

            else
                String.fromInt filteredCount ++ " of " ++ String.fromInt totalCount ++ " transactions"
    in
    H.div [ HA.class "section" ]
        [ H.div [ HA.class "transaction-list" ]
            [ H.div [ HA.class "transaction-list__header" ]
                [ H.div [ HA.class "transaction-list__header-title" ]
                    [ H.h3 [] [ H.text "Transactions" ]
                    , H.span [ HA.class "transaction-count" ] [ H.text countText ]
                    ]
                , H.div [ HA.class "transaction-list__header-buttons" ]
                    [ viewIf (totalSuggestedCount > 0) <|
                        \() ->
                            H.button
                                [ HA.class "apply-all-suggestions-button"
                                , let
                                    toUpdate : List ( Int, SuggestedAccount )
                                    toUpdate =
                                        filteredWithPriorBalanceWithSuggestions
                                            |> List.filterMap
                                                (\( ledgerLine, _, suggestedAccounts ) ->
                                                    Maybe.map
                                                        (\mostLikely ->
                                                            ( ledgerLine.transactionId
                                                            , mostLikely
                                                            )
                                                        )
                                                        (List.head suggestedAccounts)
                                                )
                                  in
                                  HE.onClick (ApplyAllSuggestionsBtnClicked toUpdate)
                                ]
                                [ H.span [ HA.class "suggestion-icon" ] [ H.text "💡" ]
                                , H.text "Apply All Suggestions"
                                ]
                    , H.button
                        [ HA.class "button button--primary"
                        , HE.onClick AddTransactionClicked
                        ]
                        [ H.text "Add Transaction" ]
                    ]
                ]
            , H.map GotSearchFormMsg (viewSearchForm searchForm)
            , H.ul [ HA.class "transaction-list__items" ]
                -- FIXME: Compute the prior balance at the DB level.
                -- FIXME: That'll enable more flexibility reversing the rows order.
                -- FIXME: Or start the first row form the current balance, rather than 0€.
                (List.map
                    viewOneTransaction
                    filteredWithPriorBalanceWithSuggestions
                )
            , viewContextMenu contextMenu
            ]
        ]


transactionMatchesFilters : SearchForm -> LedgerLine -> Bool
transactionMatchesFilters searchForm tx =
    let
        matchesUnknownExpenses : Bool
        matchesUnknownExpenses =
            not searchForm.filterUnknownExpenses
                || (tx.toAccountName == "Unknown_EXPENSE")

        matchesSearchBy : Bool
        matchesSearchBy =
            let
                matchesSearchText : { descr : String } -> Bool
                matchesSearchText { descr } =
                    String.isEmpty descr
                        || String.contains
                            (String.toLower descr)
                            (String.toLower tx.descr)
            in
            case searchForm.searchBy of
                ByDescr descr ->
                    matchesSearchText { descr = descr }

                BySimilarTo data ->
                    data.soundexDescr == tx.soundexDescr

        matchesMin : Bool
        matchesMin =
            case String.toInt searchForm.minAmount of
                Nothing ->
                    True

                Just minAmount ->
                    abs tx.flowCents >= minAmount

        matchesMax : Bool
        matchesMax =
            case String.toInt searchForm.maxAmount of
                Nothing ->
                    True

                Just maxAmount ->
                    abs tx.flowCents <= maxAmount
    in
    List.foldl (&&)
        True
        [ matchesUnknownExpenses
        , matchesSearchBy
        , matchesMin
        , matchesMax
        ]


viewSearchForm : SearchForm -> Html SearchFormMsg
viewSearchForm searchForm =
    H.div [ HA.class "transaction-search" ]
        [ H.div [ HA.class "transaction-search__row" ]
            [ H.div [ HA.class "transaction-search__field" ]
                [ H.label [ HA.for "search-description" ] [ H.text "Description" ]
                , H.input
                    [ HA.type_ "text"
                    , HA.id "search-description"
                    , HE.onInput SearchDescrChanged
                    , HA.value <|
                        case searchForm.searchBy of
                            ByDescr descr ->
                                descr

                            _ ->
                                ""
                    , HA.placeholder "Search by description"
                    , HA.autocomplete False
                    , HA.class "transaction-search__input"
                    ]
                    []
                ]
            , H.div [ HA.class "transaction-search__field" ]
                [ H.label [ HA.for "search-amount-min" ] [ H.text "Min Amount" ]
                , H.input
                    [ HA.type_ "number"
                    , HA.id "search-amount-min"
                    , HA.placeholder "Min"
                    , HE.onInput SearchMinAmountChanged
                    , HA.value searchForm.minAmount
                    , HA.class "transaction-search__input"
                    ]
                    []
                ]
            , H.div [ HA.class "transaction-search__field" ]
                [ H.label [ HA.for "search-amount-max" ] [ H.text "Max Amount" ]
                , H.input
                    [ HA.type_ "number"
                    , HA.id "search-amount-max"
                    , HA.placeholder "Max"
                    , HE.onInput SearchMaxAmountChanged
                    , HA.value searchForm.maxAmount
                    , HA.class "transaction-search__input"
                    ]
                    []
                ]
            ]
        , H.div [ HA.class "transaction-search__row transaction-search__row--bottom" ]
            [ H.div [ HA.class "transaction-search__field transaction-search__field--checkbox" ]
                [ H.label [ HA.class "checkbox-container" ]
                    [ H.input
                        [ HA.type_ "checkbox"
                        , HA.checked <|
                            searchForm.filterUnknownExpenses
                        , HE.onClick SearchUnknownExpensesClicked
                        ]
                        []
                    , H.span
                        [ HA.class "checkbox-label"
                        , HA.style "margin-left" "3px"
                        ]
                        [ H.text "Unknown expenses" ]
                    ]
                ]
            , H.div [ HA.class "transaction-search__field--button" ]
                [ H.button
                    [ HA.class "search-clear-button"
                    , HA.type_ "button"
                    , HA.id "form-clear-button"
                    , HE.onClick ClearSearchFormBtnPressed
                    ]
                    [ H.text "Clear" ]
                ]
            ]
        , case searchForm.searchBy of
            BySimilarTo data ->
                H.div [ HA.class "similar-transactions-info" ]
                    [ H.span [ HA.class "similar-transactions-label" ] [ H.text "Displaying transactions similar to:" ]
                    , H.span [ HA.class "similar-transactions-value" ] [ H.text data.descr ]
                    ]

            _ ->
                H.text ""
        ]


viewLoaded :
    Maybe ContextMenu
    -> SearchForm
    -> Maybe Dialog
    -> List AccountRead
    -> List AccountBalanceRead
    -> List LedgerLine
    -> List Suggestion
    -> Html HomeMsg
viewLoaded contextMenu searchMode dialog_ accounts balances ledgerLines suggestions =
    let
        link : Route -> String -> Html msg
        link route text =
            H.li [] [ H.a [ Route.href route ] [ H.text text ] ]
    in
    H.div [ HA.class "container" ]
        [ H.div [ HA.class "section" ]
            [ H.ul []
                [ link Route.BudgetList "Budgets"
                ]
            ]
        , H.h1 [ HA.style "margin-bottom" "0" ] [ H.text "PFM" ]
        , H.h4 [ HA.style "margin-top" "3px", HA.style "margin-bottom" "8px" ] [ H.text "In Elm" ]
        , H.div [ HA.class "section" ]
            [ H.h2 [ HA.class "section-title" ] [ H.text "Balances" ]
            , H.div [ HA.class "balances" ]
                (List.map
                    (\balance_ ->
                        balanceCard balance_
                    )
                    balances
                )
            ]
        , viewLedgerLines contextMenu suggestions searchMode ledgerLines
        , case dialog_ of
            Nothing ->
                H.text ""

            Just (EditDialog data) ->
                viewEditDialog accounts data

            Just (CreateDialog data) ->
                viewCreateDialog accounts data
        ]


viewHome : HomeModel -> Html HomeMsg
viewHome homeModel =
    case homeModel.data of
        Loaded loadedData ->
            viewLoaded
                homeModel.contextMenu
                homeModel.searchForm
                homeModel.dialog
                loadedData.accounts
                loadedData.balances
                loadedData.ledgerLines
                loadedData.suggestions

        Failed ->
            H.text "Failed to load data."

        Loading ->
            H.text "Loading..."


viewEditDialog : List AccountRead -> MkEditDialog -> Html HomeMsg
viewEditDialog allAccounts2 data =
    dialog
        [ HA.id "transaction-dialog"
        , HA.class "transaction"
        ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.div [ HA.class "dialog-header" ]
                [ H.h3 [ HA.class "dialog-title" ] [ H.text "Edit Transaction" ]
                , H.div [ HA.class "dialog-menu" ]
                    [ H.button
                        [ HA.class "menu-button"
                        , HA.attribute "aria-label" "More options"
                        ]
                        [ H.text "⋮" ]
                    , H.div [ HA.class "menu-dropdown" ]
                        [ H.button
                            [ HA.class "menu-item"
                            , HE.onClick (EditDialogChanged <| DeleteTransactionBtnPressed { transactionId = data.transactionId })
                            ]
                            [ H.text "Delete" ]
                        , H.button [ HA.class "menu-item menu-item--disabled" ] [ H.text "Duplicate" ]
                        ]
                    ]
                ]
            , makeTextField
                { text = "Description"
                , value = data.descr
                , onInput = EditDialogChanged << EditDescrChanged
                , autofocus = False
                }
            , accountSelect
                { text = "From"
                , value = String.fromInt data.fromAccountId
                , onInput = EditDialogChanged << EditFromChanged
                , accounts = allAccounts2
                , excludeAccountId = Just data.toAccountId
                }
            , accountSelect
                { text = "To"
                , value = String.fromInt data.toAccountId
                , onInput = EditDialogChanged << EditToChanged
                , accounts = allAccounts2
                , excludeAccountId = Just data.fromAccountId
                }
            , makeTextField
                { text = "Amount"
                , value = data.amount
                , onInput = EditDialogChanged << EditAmountChanged
                , autofocus = True
                }
            , dateField
                { text = "Date"
                , date = data.date
                , showTime = data.showTime
                , onDateInput = EditDialogChanged << EditDateChanged
                , onToggleTime = EditDialogChanged EditToggleTimeDisplay
                }
            , H.div [ HA.class "dialog-actions" ]
                [ H.button
                    [ HA.class "button button--secondary"
                    , HE.onClick CloseDialogPressed
                    ]
                    [ H.text "Cancel" ]
                , H.button
                    [ HA.class "button button--primary"
                    , HE.onClick (EditDialogChanged EditDialogSave)
                    ]
                    [ H.text "Save" ]
                ]
            ]
        ]


viewCreateDialog : List AccountRead -> MkCreateDialog -> Html HomeMsg
viewCreateDialog allAccounts2 data =
    dialog
        [ HA.id "transaction-dialog"
        , HA.class "transaction"
        ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.h3 [ HA.class "dialog-title" ] [ H.text "Add Transaction" ]
            , makeTextField
                { text = "Description"
                , value = data.descr
                , onInput = CreateDialogChanged << CreateDescrChanged
                , autofocus = False
                }
            , accountSelect
                { text = "From"
                , value = String.fromInt data.fromAccountId
                , onInput = CreateDialogChanged << CreateFromChanged
                , accounts = allAccounts2
                , excludeAccountId = Just data.toAccountId
                }
            , accountSelect
                { text = "To"
                , value = String.fromInt data.toAccountId
                , onInput = CreateDialogChanged << CreateToChanged
                , accounts = allAccounts2
                , excludeAccountId = Just data.fromAccountId
                }
            , makeTextField
                { text = "Amount"
                , value = data.amount
                , onInput = CreateDialogChanged << CreateAmountChanged
                , autofocus = False
                }
            , dateField
                { text = "Date"
                , date = data.date
                , showTime = data.showTime
                , onDateInput = CreateDialogChanged << CreateDateChanged
                , onToggleTime = CreateDialogChanged CreateToggleTimeDisplay
                }
            , H.div [ HA.class "dialog-actions" ]
                [ H.button
                    [ HA.class "button button--secondary"
                    , HE.onClick CloseDialogPressed
                    ]
                    [ H.text "Cancel" ]
                , H.button
                    [ HA.class "button button--primary"
                    , HE.onClick (CreateDialogChanged CreateDialogSave)
                    ]
                    [ H.text "Add" ]
                ]
            ]
        ]


makeTextField : { text : String, value : String, onInput : String -> msg, autofocus : Bool } -> Html msg
makeTextField { text, value, onInput, autofocus } =
    let
        fieldId =
            makeFieldId text
    in
    H.div [ HA.class "field" ]
        [ H.label [ HA.class "field__label", HA.for fieldId ]
            [ H.text text ]
        , H.input
            (List.filterMap identity
                [ Just <| HA.type_ "text"
                , Just <| HA.autocomplete False
                , Just <| HA.id fieldId
                , Just <| HA.class "field__input"
                , Just <| HA.value value
                , Just <| HE.onInput onInput

                -- Don't use Elm's autofocus, we need to trigger HTML-native functionality for the dialog handling.
                , if autofocus then
                    Just <| HA.attribute "autofocus" ""

                  else
                    Nothing
                ]
            )
            []
        ]


makeFieldId : String -> String
makeFieldId =
    (\s -> s ++ "-field")
        << String.replace " " "-"
        << String.toLower


dateField :
    { text : String
    , date : Time.Posix
    , showTime : Bool
    , onDateInput : Time.Posix -> msg
    , onToggleTime : msg
    }
    -> Html msg
dateField { text, date, showTime, onDateInput, onToggleTime } =
    let
        fieldId =
            makeFieldId text

        inputType =
            -- If the time is not shown, we'll still store an "instant" (so a posix time)
            -- So the time portion will implicitly be equal to the current time, from the user's browser
            -- at the time of the input.
            if showTime then
                "datetime-local"

            else
                "date"
    in
    H.div [ HA.class "field" ]
        [ H.div [ HA.class "field__header" ]
            [ H.label
                [ HA.class "field__label"
                , HA.for fieldId
                ]
                [ H.text text ]
            , H.div [ HA.class "field__toggle" ]
                [ H.label [ HA.class "toggle" ]
                    [ H.input
                        [ HA.type_ "checkbox"
                        , HA.checked showTime
                        , HE.onClick onToggleTime
                        , HA.class "toggle__input"
                        ]
                        []
                    , H.span [ HA.class "toggle__label" ] [ H.text "Include time" ]
                    ]
                ]
            ]
        , H.input
            [ HA.class "field__input"
            , HA.id fieldId
            , HA.type_ inputType
            , HA.value <| Utils.formatDateForInput date showTime
            , HE.onInput
                (onDateInput
                    << Debug.log "wat"
                    << Result.withDefault (Time.millisToPosix 0)
                    << Iso8601.toTime
                )
            ]
            []
        ]


accountSelect :
    { a
        | onInput : Int -> msg
        , text : String
        , value : String
        , accounts : List AccountRead
        , excludeAccountId : Maybe Int
    }
    -> Html msg
accountSelect { onInput, text, value, accounts, excludeAccountId } =
    let
        fieldId =
            makeFieldId text

        filteredAccounts : List AccountRead
        filteredAccounts =
            case excludeAccountId of
                Just excludeId ->
                    List.filter ((/=) excludeId << .accountId) accounts

                Nothing ->
                    accounts
    in
    H.div [ HA.class "field" ]
        [ H.label
            [ HA.class "field__label"
            , HA.for fieldId
            ]
            [ H.text text ]
        , H.select
            [ HA.class "field__select"
            , HA.id fieldId
            , HE.onInput (onInput << Maybe.withDefault 0 << String.toInt)
            , HA.value value
            ]
            (H.option [ HA.value "0" ] [ H.text "-- Select an account --" ]
                :: List.map
                    (\account ->
                        H.option
                            [ HA.value (String.fromInt account.accountId)
                            , HA.selected (String.fromInt account.accountId == value)
                            ]
                            [ H.text (account.categoryName ++ ": " ++ account.name) ]
                    )
                    filteredAccounts
            )
        ]


type alias Flags =
    { language : String
    }


fetchData : Cmd HomeMsg
fetchData =
    let
        fetchTask : Task Http.Error Data
        fetchTask =
            let
                apply =
                    Task.map2 (\x f -> f x)
            in
            Task.succeed Data
                |> apply fetchCategoriesTask
                |> apply fetchLedgerLinesTask
                |> apply fetchAccountsTask
                |> apply fetchBalancesTask
                |> apply fetchSuggestionsTask
    in
    Task.attempt GotData fetchTask


initSearchForm : SearchForm
initSearchForm =
    { searchBy = ByDescr ""
    , minAmount = ""
    , maxAmount = ""
    , filterUnknownExpenses = False
    }


type alias HomeModel =
    { data : Status Data
    , scrollY : Maybe Int
    , contextMenu : Maybe ContextMenu
    , searchForm : SearchForm
    , dialog : Maybe Dialog
    }


homeInit : HomeModel
homeInit =
    { data = Loading
    , scrollY = Nothing
    , contextMenu = Nothing
    , searchForm = initSearchForm
    , dialog = Nothing
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        build ( page, pageCmd ) =
            ( { key = key
              , page = page
              , now = Time.millisToPosix 0
              , zone = Time.utc
              , isDarkTheme = False
              }
            , pageCmd
            )
    in
    build <| initPage url


initPage : Url -> ( Page, Cmd Msg )
initPage url =
    case Route.fromUrl url of
        Route.NotFound ->
            ( MkNotFoundPage, Cmd.none )

        Route.Home ->
            ( MkHomePage homeInit
            , Cmd.batch
                [ Task.perform GotZone Time.here
                , consoleLog "Booting up..." E.null
                , Cmd.map GotHomeMsg fetchData
                ]
            )

        Route.UI ->
            ( MkUI_Page (), Cmd.none )

        Route.BudgetList ->
            BudgetListPage.init
                |> Tuple.mapBoth
                    MkBudgetListPage
                    (Cmd.map GotBudgetListPageMsg)

        Route.BudgetEdit id ->
            BudgetEditPage.init { id = id }
                |> Tuple.mapBoth
                    MkBudgetEditPage
                    (Cmd.map GotBudgetEditPageMsg)



--simulateResponse : Cmd Msg
--simulateResponse =
--    Process.sleep 0
--        |> Task.andThen (\() -> Task.succeed ())
--        -- |> Task.andThen (\() -> Task.fail <| Http.BadStatus 500)
--        |> Task.attempt (CreateDialogChanged << GotCreateSaveResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mapPage : (m -> Page) -> m -> Model
        mapPage constructor =
            \m -> { model | page = constructor m }

        setPage : Page -> Model
        setPage =
            \p -> { model | page = p }
    in
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            initPage url
                |> Tuple.mapBoth
                    setPage
                    identity

        GotZone zone ->
            ( { model | zone = zone }
            , Cmd.none
            )

        ToggleTheme ->
            ( { model | isDarkTheme = not model.isDarkTheme }
            , toggleTheme ()
            )

        GotHomeMsg homeMsg ->
            case model.page of
                MkHomePage homeModel ->
                    updateHome homeMsg homeModel
                        |> Tuple.mapBoth
                            (mapPage MkHomePage)
                            (Cmd.map GotHomeMsg)

                _ ->
                    ( model, Cmd.none )

        GotBudgetListPageMsg pageMsg ->
            case model.page of
                MkBudgetListPage pageModel ->
                    BudgetListPage.update pageMsg pageModel
                        |> Tuple.mapBoth
                            (mapPage MkBudgetListPage)
                            (Cmd.map GotBudgetListPageMsg)

                _ ->
                    ( model, Cmd.none )

        GotBudgetEditPageMsg pageMsg ->
            case model.page of
                MkBudgetEditPage pageModel ->
                    BudgetEditPage.update pageMsg pageModel
                        |> Tuple.mapBoth
                            (mapPage MkBudgetEditPage)
                            (Cmd.map GotBudgetEditPageMsg)

                _ ->
                    ( model, Cmd.none )


updateHome : HomeMsg -> HomeModel -> ( HomeModel, Cmd HomeMsg )
updateHome msg model =
    let
        updateSearchForm : (SearchForm -> SearchForm) -> SearchForm
        updateSearchForm f =
            f model.searchForm
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowContextMenu menu ->
            ( { model | contextMenu = Just menu }, Cmd.none )

        HideContextMenu ->
            ( { model | contextMenu = Nothing }, Cmd.none )

        FindSimilarSelected data ->
            ( { model
                | searchForm = updateSearchForm (\sf -> { sf | searchBy = BySimilarTo data })
                , contextMenu = Nothing
              }
            , Cmd.none
            )

        RcvScrollY scrollY ->
            ( { model | scrollY = Just scrollY }
            , Cmd.none
            )

        GotData result ->
            case result of
                Err _ ->
                    ( { model | data = Failed }
                    , Cmd.none
                    )

                Ok data ->
                    ( { model | data = Loaded data }
                    , Maybe.withDefault
                        Cmd.none
                        (Maybe.map restoreScrollY model.scrollY)
                    )

        RequestCursorRestore { scrollY } ->
            let
                _ =
                    Debug.log "Actually restoring scrollY" scrollY
            in
            ( model
            , restoreScrollY scrollY
            )

        Fetching { scrollY } [] ->
            let
                _ =
                    Debug.log "Fetching done! Restoring scrollY" scrollY
            in
            ( model
            , Process.sleep 0 |> Task.perform (\() -> RequestCursorRestore { scrollY = scrollY })
            )

        Fetching params (cmd :: rest) ->
            let
                _ =
                    Debug.log "Fetching..." (List.length rest)
            in
            ( model
            , Cmd.batch
                [ cmd
                , Task.perform (Fetching params) (Task.succeed rest)
                ]
            )

        GotSearchFormMsg subMsg ->
            case subMsg of
                SearchDescrChanged str ->
                    ( { model | searchForm = updateSearchForm (\sf -> { sf | searchBy = ByDescr str }) }
                    , Cmd.none
                    )

                SearchMinAmountChanged str ->
                    ( { model | searchForm = updateSearchForm (\sf -> { sf | minAmount = str }) }
                    , Cmd.none
                    )

                SearchMaxAmountChanged str ->
                    ( { model | searchForm = updateSearchForm (\sf -> { sf | maxAmount = str }) }
                    , Cmd.none
                    )

                SearchUnknownExpensesClicked ->
                    ( { model | searchForm = updateSearchForm (\sf -> { sf | filterUnknownExpenses = not sf.filterUnknownExpenses }) }
                    , Cmd.none
                    )

                ClearSearchFormBtnPressed ->
                    ( { model | searchForm = updateSearchForm (\_ -> initSearchForm) }
                    , Cmd.none
                    )

        EditTransactionClicked params ->
            handleEditDialog params model

        ApplySuggestedExpenseClicked { ledgerLine, toAccountId } ->
            let
                transactionWrite : TransactionWrite
                transactionWrite =
                    { budgetId = Just ledgerLine.budgetId
                    , fromAccountId = ledgerLine.fromAccountId
                    , toAccountId = toAccountId
                    , dateUnix = ledgerLine.dateUnix
                    , descr = ledgerLine.descr
                    , cents = abs ledgerLine.flowCents
                    }
            in
            ( model
            , putTransaction
                ( ledgerLine.transactionId
                , transactionWrite
                )
                GotApplyExpenseResponse
            )

        ApplyAllSuggestionsBtnClicked toUpdate ->
            ( model
            , applyAllSuggestions toUpdate
            )

        GotApplyExpenseResponse result ->
            case result of
                Err _ ->
                    ( model
                      -- TODO: display toast error or similar
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , fetchData
                    )

        GotApplyAllSuggestionsResponse result ->
            case result of
                Err _ ->
                    ( model
                      -- TODO: display toast error or similar
                    , Cmd.none
                    )

                Ok () ->
                    ( model
                    , fetchData
                    )

        EditDialogChanged subMsg ->
            case model.dialog of
                Just (EditDialog data) ->
                    case subMsg of
                        EditDescrChanged str ->
                            ( { model
                                | dialog = Just <| EditDialog { data | descr = str }
                              }
                            , Cmd.none
                            )

                        EditFromChanged n ->
                            ( { model
                                | dialog = Just <| EditDialog { data | fromAccountId = n }
                              }
                            , Cmd.none
                            )

                        EditToChanged n ->
                            ( { model
                                | dialog =
                                    Just <|
                                        EditDialog { data | toAccountId = n }
                              }
                            , Cmd.none
                            )

                        EditAmountChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | amount = str } }
                            , Cmd.none
                            )

                        EditDateChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | date = str } }
                            , Cmd.none
                            )

                        EditToggleTimeDisplay ->
                            ( { model | dialog = Just <| EditDialog { data | showTime = not data.showTime } }
                            , Cmd.none
                            )

                        EditDialogSave ->
                            ( model
                            , Cmd.batch
                                [ let
                                    transactionWrite : TransactionWrite
                                    transactionWrite =
                                        { budgetId = Just data.budgetId
                                        , fromAccountId = data.fromAccountId
                                        , toAccountId = data.toAccountId
                                        , dateUnix = Time.posixToMillis data.date // 1000
                                        , descr = data.descr
                                        , cents =
                                            String.toInt data.amount
                                                -- (-1) will make the update fail
                                                |> Maybe.withDefault -1
                                        }
                                  in
                                  putTransaction
                                    ( data.transactionId
                                    , transactionWrite
                                    )
                                    (EditDialogChanged << GotEditSaveResponse)
                                ]
                            )

                        GotEditSaveResponse result ->
                            case result of
                                Err _ ->
                                    ( model
                                      -- TODO: display toast error or similar
                                    , Cmd.none
                                    )

                                Ok _ ->
                                    ( { model
                                        | dialog = Nothing

                                        --, data = Loading -- FIXME: does it flicker?
                                      }
                                    , Cmd.batch
                                        [ closeDialog ()
                                        , fetchData
                                        ]
                                    )

                        DeleteTransactionBtnPressed params ->
                            ( model
                            , deleteTransaction params
                            )

                        GotDeleteResponse result ->
                            case result of
                                Err _ ->
                                    ( model
                                      -- TODO: display toast error or similar
                                    , Cmd.none
                                    )

                                Ok _ ->
                                    ( { model
                                        | dialog = Nothing

                                        --, data = Loading -- FIXME: does it flicker?
                                      }
                                    , Cmd.batch
                                        [ closeDialog ()
                                        , fetchData
                                        ]
                                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        AddTransactionClicked ->
            ( model
            , Task.perform OpenCreateDialog Time.now
            )

        OpenCreateDialog now ->
            let
                dialog_ =
                    CreateDialog
                        { descr = ""
                        , fromAccountId = 0
                        , toAccountId = 0
                        , amount = ""
                        , date = now
                        , showTime = True
                        }
            in
            ( { model
                | dialog = Just dialog_
                , contextMenu = Nothing
              }
            , showDialog ()
            )

        CreateDialogChanged subMsg ->
            case model.dialog of
                Just (CreateDialog data) ->
                    case subMsg of
                        CreateDescrChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | descr = str } }
                            , Cmd.none
                            )

                        CreateFromChanged n ->
                            ( { model
                                | dialog =
                                    Just <|
                                        CreateDialog { data | fromAccountId = n }
                              }
                            , Cmd.none
                            )

                        CreateToChanged n ->
                            ( { model | dialog = Just <| CreateDialog { data | toAccountId = n } }
                            , Cmd.none
                            )

                        CreateAmountChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | amount = str } }
                            , Cmd.none
                            )

                        CreateDateChanged posix ->
                            ( { model | dialog = Just <| CreateDialog { data | date = posix } }
                            , Cmd.none
                            )

                        CreateToggleTimeDisplay ->
                            ( { model | dialog = Just <| CreateDialog { data | showTime = not data.showTime } }
                            , Cmd.none
                            )

                        CreateDialogSave ->
                            let
                                newTransaction : TransactionWrite
                                newTransaction =
                                    Debug.log "newTransaction" <|
                                        { budgetId = Nothing
                                        , fromAccountId = data.fromAccountId
                                        , toAccountId = data.toAccountId

                                        -- We remove sub-second values, as only JS does that and we don't need it!
                                        , dateUnix = Time.posixToMillis data.date // 1000
                                        , descr = data.descr
                                        , cents =
                                            data.amount
                                                |> String.replace "." ""
                                                |> Debug.log "TMP"
                                                |> String.toInt
                                                |> Maybe.withDefault 0
                                        }
                            in
                            ( model
                              -- , simulateResponse
                            , postTransaction newTransaction
                            )

                        GotCreateSaveResponse result ->
                            case result of
                                Err _ ->
                                    ( model
                                      -- TODO: display toast error or similar
                                    , Cmd.none
                                    )

                                Ok _ ->
                                    ( { model
                                        | dialog = Nothing

                                        --, data = Loading -- FIXME: does it flicker?
                                      }
                                    , Cmd.batch
                                        [ closeDialog ()
                                        , fetchData
                                        ]
                                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CloseDialogPressed ->
            ( { model | dialog = Nothing }
            , closeDialog ()
            )

        EnterPressed ->
            case model.dialog of
                Just (EditDialog _) ->
                    updateHome (EditDialogChanged EditDialogSave) model

                Just (CreateDialog _) ->
                    updateHome (CreateDialogChanged CreateDialogSave) model

                _ ->
                    ( model
                    , Cmd.none
                    )

        EscapePressed ->
            ( { model | contextMenu = Nothing }
            , Cmd.none
            )


handleEditDialog : ( Int, TransactionWrite ) -> HomeModel -> ( HomeModel, Cmd HomeMsg )
handleEditDialog ( transactionId, tw ) model =
    -- let
    --     dateString =
    --         Iso8601.fromTime tx.date
    --     hasTimeInfo =
    --         String.contains "T" dateString
    --             && not (String.endsWith "T00:00:00.000Z" dateString)
    --             && not (String.endsWith "T00:00:00Z" dateString)
    -- in
    let
        editDialogModel : MkEditDialog
        editDialogModel =
            { budgetId =
                Maybe.withDefault
                    -- FIXME: yuck!
                    0
                    tw.budgetId
            , transactionId = transactionId
            , fromAccountId = tw.fromAccountId
            , toAccountId = tw.toAccountId
            , amount = String.fromInt tw.cents
            , date = Time.millisToPosix (tw.dateUnix * 1000)
            , descr = tw.descr
            , showTime = True -- FIXME: observe the unix ts trailing info
            }
    in
    ( { model
        | dialog =
            Just
                (EditDialog editDialogModel)
        , contextMenu = Nothing
      }
    , showDialog ()
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ enterPressed (\() -> GotHomeMsg EnterPressed)
        , escapePressed (\() -> GotHomeMsg EscapePressed)
        , rcvScrollY (GotHomeMsg << RcvScrollY)
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
