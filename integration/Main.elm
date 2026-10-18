port module Main exposing (main)

import Debounce
import Platform
import Task


port observed : String -> Cmd msg


type alias Model =
    { scheduled : Debounce.Debounce Never String
    , immediate : Debounce.Debounce Never String
    }


type Msg
    = Scheduled (Debounce.Msg String Never String)
    | Immediate (Debounce.Msg String Never String)


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( first, firstCmd ) =
            Debounce.schedule 20 "first" Debounce.init

        ( scheduled, secondCmd ) =
            Debounce.schedule 1 "second" first

        ( waiting, waitingCmd ) =
            Debounce.schedule 20 "old" Debounce.init

        ( immediate, immediateCmd ) =
            Debounce.runNow Task.succeed "now" waiting
    in
    ( { scheduled = scheduled, immediate = immediate }
    , Cmd.batch
        [ Cmd.map Scheduled firstCmd
        , Cmd.map Scheduled secondCmd
        , Cmd.map Immediate waitingCmd
        , Cmd.map Immediate immediateCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scheduled subMsg ->
            let
                previous =
                    model.scheduled

                ( scheduled, cmd ) =
                    Debounce.update Task.succeed subMsg model.scheduled
            in
            ( { model | scheduled = scheduled }
            , Cmd.batch
                [ Cmd.map Scheduled cmd
                , reportTransition "scheduled" previous scheduled
                ]
            )

        Immediate subMsg ->
            let
                previous =
                    model.immediate

                ( immediate, cmd ) =
                    Debounce.update Task.succeed subMsg model.immediate
            in
            ( { model | immediate = immediate }
            , Cmd.batch
                [ Cmd.map Immediate cmd
                , reportTransition "immediate" previous immediate
                ]
            )


reportTransition : String -> Debounce.Debounce Never String -> Debounce.Debounce Never String -> Cmd msg
reportTransition label previous current =
    case ( Debounce.status previous, Debounce.status current ) of
        ( Debounce.Running, Debounce.Completed (Ok value) ) ->
            observed (label ++ ":" ++ value)

        _ ->
            Cmd.none
