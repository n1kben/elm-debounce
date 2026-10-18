# elm-debounce

Debounce Elm tasks while keeping application events, state updates, and
commands explicit.

```elm
type alias Model =
    { username : String
    , availability : Debounce.Debounce Http.Error Availability
    }


type Msg
    = UsernameChanged String
    | UsernameAvailability (Debounce.Msg String Http.Error Availability)


update msg model =
    case msg of
        UsernameChanged username ->
            let
                ( availability, cmd ) =
                    Debounce.schedule 300 username model.availability
            in
            ( { model
                | username = username
                , availability = availability
              }
            , Cmd.map UsernameAvailability cmd
            )

        UsernameAvailability subMsg ->
            let
                ( availability, cmd ) =
                    Debounce.update checkUsername subMsg model.availability
            in
            ( { model | availability = availability }
            , Cmd.map UsernameAvailability cmd
            )
```

On submit, run the same task immediately:

```elm
Debounce.runNow checkUsername model.username model.availability
```

The input remains application-owned. The debounce state contains only the
revision and task status needed to ignore superseded work.

## License

BSD 3-Clause
