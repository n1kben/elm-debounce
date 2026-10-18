# elm-debounce

Debounce Elm tasks while keeping application events, state updates, and
commands explicit.

```elm
type alias Model =
    { value : Value
    , debounce : Debounce.Debounce Error Output
    }


type Msg
    = ValueReceived Value
    | Debounced (Debounce.Msg Value Error Output)


update msg model =
    case msg of
        ValueReceived value ->
            let
                ( debounce, cmd ) =
                    Debounce.schedule 300 value model.debounce
            in
            ( { model
                | value = value
                , debounce = debounce
              }
            , Cmd.map Debounced cmd
            )

        Debounced subMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update performWork subMsg model.debounce
            in
            ( { model | debounce = debounce }
            , Cmd.map Debounced cmd
            )
```

When the delay should be skipped, run the same task immediately:

```elm
Debounce.runNow performWork model.value model.debounce
```

The input remains application-owned. The debounce state contains only the
revision and task status needed to ignore superseded work.

## License

BSD 3-Clause
