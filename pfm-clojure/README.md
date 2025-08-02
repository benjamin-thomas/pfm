## Start the server

```sh
PORT=9001 APP_ENV=test clojure -M -m pfm.core

# Then observe this API request
http :9001/api/transactions
```

## Run all tests

```sh
clojure -M:test
clojure -M:test --reporter kaocha.report/documentation --watch 
```