module Debounce exposing
    ( Debounce, Msg, Status(..)
    , init, schedule, runNow, update
    , cancel, status
    )

{-| Debounce tasks while keeping their lifecycle explicit in `update`.

The input remains in your application model. `Debounce` stores only the
revision and execution status needed to ignore superseded timers and tasks.


# Types

@docs Debounce, Msg, Status


# Lifecycle

@docs init, schedule, runNow, update


# State

@docs cancel, status

-}

import Process
import Task exposing (Task)


{-| State for one debounced task.

The type is opaque so revision bookkeeping cannot get out of sync.

-}
type Debounce error output
    = Debounce
        { revision : Int
        , status : Status error output
        }


{-| Internal messages produced by a debounced task.

Wrap this in one constructor of your application's `Msg` type and pass it to
[`update`](#update).

-}
type Msg input error output
    = DelayElapsed Int input
    | TaskCompleted Int (Result error output)


{-| The observable lifecycle of a debounced task.

`Waiting` means the delay has not elapsed. `Running` means the task has
started. `Completed` retains its latest result until another operation starts
or the state is cancelled.

-}
type Status error output
    = Idle
    | Waiting
    | Running
    | Completed (Result error output)


{-| Create idle debounce state.
-}
init : Debounce error output
init =
    Debounce
        { revision = 0
        , status = Idle
        }


{-| Schedule an input to run after the given number of milliseconds.

Scheduling again supersedes the previous timer. Superseded timers may still
wake, but `update` ignores them and never starts their tasks.

-}
schedule : Float -> input -> Debounce error output -> ( Debounce error output, Cmd (Msg input error output) )
schedule delay input (Debounce state) =
    let
        revision =
            state.revision + 1
    in
    ( Debounce
        { revision = revision
        , status = Waiting
        }
    , Process.sleep delay
        |> Task.perform (\_ -> DelayElapsed revision input)
    )


{-| Start a task immediately, superseding any scheduled or running task.

Use this when pending work should start without the debounce delay.

-}
runNow :
    (input -> Task error output)
    -> input
    -> Debounce error output
    -> ( Debounce error output, Cmd (Msg input error output) )
runNow task input (Debounce state) =
    let
        revision =
            state.revision + 1
    in
    ( Debounce
        { revision = revision
        , status = Running
        }
    , task input
        |> Task.attempt (TaskCompleted revision)
    )


{-| Handle a debounce sub-message.

When the current delay elapses, this starts the supplied task. When the task
finishes, its result is stored in the status. Messages from superseded
revisions are ignored.

-}
update :
    (input -> Task error output)
    -> Msg input error output
    -> Debounce error output
    -> ( Debounce error output, Cmd (Msg input error output) )
update task msg debounceState =
    case ( msg, debounceState ) of
        ( DelayElapsed revision input, Debounce state ) ->
            if revision == state.revision then
                ( Debounce { state | status = Running }
                , task input
                    |> Task.attempt (TaskCompleted revision)
                )

            else
                ( debounceState, Cmd.none )

        ( TaskCompleted revision result, Debounce state ) ->
            if revision == state.revision then
                ( Debounce { state | status = Completed result }
                , Cmd.none
                )

            else
                ( debounceState, Cmd.none )


{-| Cancel scheduled or running work and return to `Idle`.

Elm does not expose the process behind `Task.perform`, so this is logical
cancellation: later messages from the cancelled revision are ignored.

-}
cancel : Debounce error output -> Debounce error output
cancel (Debounce state) =
    Debounce
        { revision = state.revision + 1
        , status = Idle
        }


{-| Inspect the current lifecycle status.
-}
status : Debounce error output -> Status error output
status (Debounce state) =
    state.status
