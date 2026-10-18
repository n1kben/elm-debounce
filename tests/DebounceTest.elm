module DebounceTest exposing (suite)

import Debounce
import Expect
import Task
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Debounce"
        [ test "starts idle" <|
            \_ ->
                Debounce.init
                    |> Debounce.status
                    |> Expect.equal Debounce.Idle
        , test "schedule waits" <|
            \_ ->
                Debounce.init
                    |> Debounce.schedule 300 "value"
                    |> Tuple.first
                    |> Debounce.status
                    |> Expect.equal Debounce.Waiting
        , test "runNow starts immediately" <|
            \_ ->
                Debounce.init
                    |> Debounce.runNow (\input -> Task.succeed input) "value"
                    |> Tuple.first
                    |> Debounce.status
                    |> Expect.equal Debounce.Running
        , test "cancel returns to idle" <|
            \_ ->
                Debounce.init
                    |> Debounce.schedule 300 "value"
                    |> Tuple.first
                    |> Debounce.cancel
                    |> Debounce.status
                    |> Expect.equal Debounce.Idle
        ]
