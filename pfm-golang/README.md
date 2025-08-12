## Setup

```sh
go install github.com/a-h/templ/cmd/templ@latest
```

## Basic run

```sh
# Terminal 1
PORT=2000 bash -c 'templ generate --watch --proxy="http://localhost:$PORT" -proxyport=2001 --open-browser=false --cmd="go run ."'

# Terminal 2
# This will restart the server in terminal 1 (if succeeds)
rg --files -t sql | entr -c bash -c 'sqlc generate && echo OK || echo KO'
```