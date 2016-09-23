module AnimatedShowHide exposing (..)

import Task
import Process
import Html exposing (Html)
import Time exposing (Time)


type ShowState
    = Hidden
    | Showing
    | Shown
    | Hiding


nextState : ShowState -> ( Time, Time ) -> ( ShowState, Cmd ShowState )
nextState currentState ( showTransitionTime, hideTransitionTime ) =
    let
        transitionCmd errorMsg nextState delay =
            Task.perform (\_ -> Debug.log errorMsg nextState)
                (\_ -> nextState)
                (Process.sleep delay)
    in
        case currentState of
            Hidden ->
                ( Showing
                , transitionCmd "Failure to perform showing transition" Showing showTransitionTime
                )

            Showing ->
                ( Shown
                , Cmd.none
                )

            Shown ->
                ( Hiding
                , transitionCmd "Failure to perform hiding transition" Hiding hideTransitionTime
                )

            Hiding ->
                ( Hidden
                , Cmd.none
                )


view : ShowState -> (ShowState -> Html msg) -> Html msg
view showState htmlView =
    case showState of
        Hidden ->
            Html.text ""

        _ ->
            htmlView showState
