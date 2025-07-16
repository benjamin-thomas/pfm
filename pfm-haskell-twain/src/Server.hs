{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (runServer) where

import Control.Arrow ((>>>))
import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Accounts.Queries qualified as AccountQueries
import DB.Budgets.JSON qualified as BudgetJSON
import DB.Budgets.Queries (getBudgetIdForDateTry, insertForDateExn)
import DB.Budgets.Queries qualified as BudgetQueries
import DB.Categories.Queries (CategoryRow, getNonStaleCategories)
import DB.LedgerView.Queries (AccountId (MkAccountId), LedgerViewRow, getLedgerViewRows)
import DB.Transactions.Queries
    ( TransactionNewRow (MkTransactionNewRow, date)
    , UniqueFitId (MkUniqueFitId)
    , budgetId
    , deleteAllTransactions
    , deleteTransaction
    , getAllSuggestions
    , insertSuggestions
    , insertTransaction
    , updateTransaction
    )
import DB.Transactions.Queries qualified as TransactionQueries
import DB.User.Queries
import DTO.AccountRead qualified as AccountRead
import DTO.Category (Category, fromCategoryRow)
import DTO.Ledger (LedgerLine, fromLedgerViewRow, toSuggestionDTO)
import DTO.TransactionWrite (toSuggestionsInsert, toTransactionNewRow)
import DTO.User (fromUserRow)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Decimal (decimalMantissa, decimalPlaces)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (Day, UTCTime (..), secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.SQLite.Simple (Connection, execute_, open, withTransaction)
import Network.HTTP.Types (status200, status201, status204)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import OfxParser
    ( OfxBatch (MkOfxBatch)
    , StatementTransaction
        ( stAmount
        , stName
        , stPosted
        )
    , TimeStamp (FullDate, ShortDate)
    , ofxParser
    , stFitId
    )
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.Process (system)
import Text.Megaparsec qualified as P
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)
import Web.Twain qualified as Twain

mkApp :: Connection -> Twain.Application
mkApp conn =
    foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
        (logStdoutDev : corsMiddleware : routes conn)

corsMiddleware :: Twain.Middleware
corsMiddleware app req respond = do
    liftIO $ putStrLn $ "Request: " <> show req
    app
        req
        $ respond
            . Twain.withHeader
                ("Access-Control-Allow-Origin", "http://localhost:4000")
            . Twain.withHeader
                ("Access-Control-Allow-Methods", "OPTIONS, GET, POST, PUT, DELETE, PATCH")
            . Twain.withHeader
                ("Access-Control-Allow-Headers", "Content-Type")

echoName :: Twain.ResponderM ()
echoName = do
    name <- Twain.param "name"
    Twain.send $ Twain.html $ "Hello, " <> name

greeting :: ByteString -> Twain.Response
greeting name = Twain.html $ "Hello, " <> name

{- FOURMOLU_DISABLE -}
-- http -v localhost:8080/categories
handleCategories :: Connection -> Twain.ResponderM ()
handleCategories conn = do
    categoriesDb <- liftIO $ getNonStaleCategories conn :: Twain.ResponderM [CategoryRow]
    let categories = map fromCategoryRow categoriesDb  :: [Category]
    Twain.send $ Twain.json categories
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- http -v localhost:8080/transactions accountId==2
handleGetTransactions :: Connection -> Twain.ResponderM ()
handleGetTransactions conn = do
    accountId <- Twain.queryParam "accountId"
    transactions <- liftIO $ getLedgerViewRows (MkAccountId accountId) conn :: Twain.ResponderM [LedgerViewRow]
    let ledgerLineSummaries = map fromLedgerViewRow transactions            :: [LedgerLine]
    Twain.send $ Twain.json ledgerLineSummaries
{- FOURMOLU_ENABLE -}

handlePostTransactions :: Connection -> Twain.ResponderM ()
handlePostTransactions conn = do
    toInsert :: TransactionNewRow <- toTransactionNewRow <$> Twain.fromBody
    let dateUnix :: Int = date toInsert
    let transactionDay :: Day = utctDay $ posixSecondsToUTCTime $ fromIntegral dateUnix
    budgetId' <- do
        mBudgetId <- liftIO $ getBudgetIdForDateTry conn transactionDay
        maybe
            (liftIO $ insertForDateExn conn transactionDay)
            pure
            mBudgetId
    liftIO $ insertTransaction conn toInsert{budgetId = Just budgetId'}
    Twain.send $ Twain.raw status201 [] BSL.empty

handleApplyAllSuggestions :: Connection -> Twain.ResponderM ()
handleApplyAllSuggestions conn = do
    toInserts <- map toSuggestionsInsert <$> Twain.fromBody
    liftIO $ insertSuggestions conn toInserts
    Twain.send $ Twain.raw status204 [] BSL.empty

handlePutTransactions :: Connection -> Twain.ResponderM ()
handlePutTransactions conn = do
    transactionId :: Int <- Twain.param "id"
    toUpdate <- toTransactionNewRow <$> Twain.fromBody
    liftIO $ updateTransaction conn (transactionId, toUpdate)
    Twain.send $ Twain.raw status204 [] BSL.empty

handleDeleteTransactions :: Connection -> Twain.ResponderM ()
handleDeleteTransactions conn = do
    transactionId :: Int <- Twain.param "id"
    liftIO $ deleteTransaction conn transactionId
    Twain.send $ Twain.raw status204 [] BSL.empty

-- http -v localhost:8080/transactions/suggestions fromAccountId==2 toAccountId==10
handleGetAllSuggestions :: Connection -> Twain.ResponderM ()
handleGetAllSuggestions conn = do
    fromAccountId <- Twain.queryParam "fromAccountId"
    toAccountId <- Twain.queryParam "toAccountId"
    suggestionRows <- liftIO $ getAllSuggestions fromAccountId toAccountId conn
    let suggestions = map toSuggestionDTO suggestionRows
    Twain.send $ Twain.json suggestions

-- http -v localhost:8080/users all==1
handleUsers :: Connection -> Twain.ResponderM ()
handleUsers conn = do
    allP <-
        fmap
            (maybe False (("0" :: String) /=))
            (Twain.queryParamMaybe "all")

    liftIO $ putStrLn $ "\x1b[33mallP: " <> show allP <> "\x1b[0m"
    userRows <-
        let getUsers = if allP then getAllUserRows else getNewPlatformUserRows
         in liftIO $ getUsers conn
    let users = map fromUserRow userRows
    Twain.send $ Twain.json users

handleAccounts :: Connection -> Twain.ResponderM ()
handleAccounts conn = do
    accountRows <- liftIO $ AccountQueries.getAll conn
    let accounts_ = map AccountRead.fromAccountRow accountRows
    Twain.send $ Twain.json accounts_

-- http :8080/accounts/2/balance
handleAccountsBalance :: Connection -> Twain.ResponderM ()
handleAccountsBalance conn = do
    accountId :: Int <- Twain.param "id"
    balance <- liftIO $ AccountQueries.getBalance accountId conn
    Twain.send $ Twain.json balance

-- http :8080/accounts/balances accountIds==2,3,4
handleAccountsBalances :: Connection -> Twain.ResponderM ()
handleAccountsBalances conn = do
    accountIds :: [Int] <- Twain.queryParam "accountIds"
    balanceRows <- liftIO $ AccountQueries.getBalances accountIds conn
    let balances = map AccountRead.toAccountBalanceRead balanceRows
    Twain.send $ Twain.json balances

-- handleBudgets1 :: Connection -> Twain.ResponderM ()
-- handleBudgets1 conn = do
--     budgetRows <- liftIO $ BudgetQueries.getAll conn
--     let budgets = map BudgetJSON.fromBudgetDB budgetRows
--     Twain.send
--         . Twain.json
--         $ budgets

-- http :8080/budgets
handleBudgets :: Connection -> Twain.ResponderM ()
handleBudgets =
    BudgetQueries.getAll
        >>> fmap (map BudgetJSON.fromBudgetDB)
        >>> fmap Twain.json
        >>> liftIO
        >=> Twain.send

-- http :8080/budgets/1
handleBudget :: Connection -> Twain.ResponderM ()
handleBudget conn = do
    budgetId' :: Int <- Twain.param "id"
    budgetWithLines <-
        fmap
            BudgetJSON.fromBudgetWithLinesDB
            (liftIO $ BudgetQueries.getOne budgetId' conn)
    Twain.send $
        Twain.json budgetWithLines

routes :: Connection -> [Twain.Middleware]
routes conn =
    [ Twain.get "/" $ Twain.send $ Twain.text "hi (from pfm-haskell-twain)"
    , Twain.get "/accounts" $ handleAccounts conn
    , Twain.get "/accounts/:id/balance" $ handleAccountsBalance conn
    , Twain.get "/accounts/balances" $ handleAccountsBalances conn
    , Twain.get "/budgets/:id" $ handleBudget conn
    , Twain.get "/budgets" $ handleBudgets conn
    , Twain.get "/categories" $ handleCategories conn
    , Twain.get "/transactions" $ handleGetTransactions conn
    , Twain.post "/transactions" $ handlePostTransactions conn
    , Twain.patch "/transactions/apply-all-suggestions" $ handleApplyAllSuggestions conn
    , Twain.put "/transactions/:id" $ handlePutTransactions conn
    , Twain.get "/transactions/suggestions" $ handleGetAllSuggestions conn
    , Twain.delete "/transactions/:id" $ handleDeleteTransactions conn
    , Twain.route (Just "OPTIONS") "/transactions" $ Twain.send $ Twain.status status200 $ Twain.json ()
    , Twain.route (Just "OPTIONS") "/transactions/:id" $ Twain.send $ Twain.status status200 $ Twain.json ()
    , Twain.get "/users" $ handleUsers conn
    , Twain.get "/echo/:name" echoName
    , Twain.get "/greet/:name" $ do
        name <- Twain.param "name"
        Twain.send $ greeting name
    , Twain.get "/div/:num" $ do
        n :: Int <- read <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n `div` 2)
    , Twain.get "/inc/:num" $ do
        n :: Int <- maybe (error "oops") id . readMaybe <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n + 1)
    , Twain.get "/dec/:num" $ do
        n :: Int <- Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n - 1)
    , Twain.get "/dbl/:num" $ do
        num :: Int <- Twain.param "num"

        -- num :: Maybe Int <- readMaybe <$> Twain.param "num"
        -- num :: Int <- maybe (error "oops") id <$> paramMaybe "num"
        -- neg :: Maybe Bool <- queryParamMaybe "neg"
        -- liftIO $ putStrLn $ "Calling hello1: " <> hello1
        -- liftIO $ putStrLn $ "Calling hello2: " <> hello2
        -- void $ error "error!!"
        -- case num of
        --     Nothing -> Twain.send $ Twain.status status500 $ Twain.text "Invalid number format"
        --     Just n ->

        Twain.send
            . Twain.text
            . T.pack
            . show
            $ num * 2
    ]

runServer :: Port -> IO ()
runServer port = do
    conn <- open "./db.sqlite3"
    putStrLn $
        unwords
            [ "Running twain app at"
            , "http://localhost:" <> show port
            ]
    run port $ mkApp conn

{-

Temporary, for GHCi exploration.

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m +Database.Category Domain.Category
ghci> categories <- getCategories =<< _newConn
ghci> map fmtCategory categories
ghci> mapM isStale categories

 -}
_newConn :: IO Connection
_newConn = do
    conn <- open "./db.sqlite3"
    execute_ conn "PRAGMA foreign_keys = ON"
    pure conn

yellow :: String
yellow = "\x1b[33m"

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

newtype AccountNumber = MkAccountNumber Text deriving (Show)

fromOfxTransactionExn :: Map Day Int -> AccountNumber -> StatementTransaction -> TransactionNewRow
fromOfxTransactionExn postedToBudgetIdMap (MkAccountNumber accountNumber) tx
    | stAmount tx == 0 = error "Amount is 0" -- programming error, DB constraint will prevent this row from being inserted.
    | decimalPlaces (stAmount tx) /= 2 = error "Amount has more than 2 decimal places"
    | otherwise =
        let cents = decimalMantissa $ stAmount tx
            -- FIXME: hard coded account IDs
            checkingAccountId = 2
            unknownIncomeAccountId = 4
            unknownExpenseAccountId = 6
            budgetId' =
                Map.findWithDefault
                    (error "ABNORMAL: budgetId not found")
                    (timestampToDay $ stPosted tx)
                    postedToBudgetIdMap
         in MkTransactionNewRow
                { budgetId = Just budgetId'
                , fromAccountId = if stAmount tx < 0 then checkingAccountId else unknownIncomeAccountId
                , toAccountId = if stAmount tx > 0 then checkingAccountId else unknownExpenseAccountId
                , uniqueFitId =
                    Just . MkUniqueFitId $ accountNumber <> ":" <> stFitId tx
                , date = case stPosted tx of
                    FullDate tsDate -> truncate $ utcTimeToPOSIXSeconds tsDate
                    ShortDate day -> truncate $ utcTimeToPOSIXSeconds $ UTCTime day (secondsToDiffTime 0)
                , descr = T.unpack $ stName tx
                , cents = fromIntegral $ abs cents
                }

timestampToDay :: TimeStamp -> Day
timestampToDay ts = case ts of
    FullDate tsDate -> utctDay tsDate
    ShortDate d -> d

_wip :: IO ()
_wip = do
    -- sqlite-simple doesn't appear to handle multiple instructions so let's
    -- just shell out for now.
    putStrLn $ yellow <> "== Resetting the database" <> reset
    let cmd = "cat sql/init.sql | sqlite3 ./db.sqlite3"
    exitCode <- system cmd
    case exitCode of
        ExitSuccess -> putStrLn $ green <> "== Database reset" <> reset
        ExitFailure code -> do
            putStrLn $ "\x1b[31m== Database reset failed with code " <> show code <> reset
            exitFailure
    putStrLn ""

    let fileName = "CA20250630_124433.ofx" :: String
    ofxBS <- BSL.readFile (".tmp/" <> fileName)
    let ofxText = TE.decodeUtf8 $ BSL.toStrict ofxBS -- FIXME: handle decoding better
    let result = P.parse ofxParser fileName ofxText
    case result of
        Left e -> putStrLn $ P.errorBundlePretty e
        Right (MkOfxBatch accountNumber transactionsAsc) -> do
            let transactions = reverse transactionsAsc -- to have budget ids in a logical order
            let n = 3
            let counter = show n <> "/" <> show (length transactions)
            putStrLn $ yellow <> "== Showing the shape of " <> counter <> " parsed transactions ==" <> reset
            mapM_ pPrint (take n transactions)
            putStrLn ""
            conn <- _newConn
            let
                stPostedToBudgetId :: IO (Map Day Int)
                stPostedToBudgetId = do
                    foldM step Map.empty transactions
                  where
                    step :: Map Day Int -> StatementTransaction -> IO (Map Day Int)
                    step acc tx = do
                        let
                            day :: Day
                            day = timestampToDay $ stPosted tx

                        case Map.lookup day acc of
                            Just _ ->
                                pure acc
                            Nothing -> do
                                mBid <- getBudgetIdForDateTry conn day
                                budgetId' <- maybe (insertForDateExn conn day) pure mBid
                                pure $ Map.insert day budgetId' acc

            withTransaction conn $ do
                putStrLn $ yellow <> "== Truncating prior transactions" <> reset
                deleteAllTransactions conn
                postedToBudgetIdMap <- stPostedToBudgetId
                mapM_
                    ( insertTransaction conn
                        . fromOfxTransactionExn
                            postedToBudgetIdMap
                            (MkAccountNumber accountNumber)
                    )
                    transactions
                putStrLn $ green <> "== Transactions inserted" <> reset