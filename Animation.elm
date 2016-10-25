module Animation exposing (..)

import Task
import Process
import Time exposing (Time, millisecond)


type Animation state
    = Setup state
    | Animate state
    | Done state


type alias AnimationDelays =
    { setup : Time
    , animate : Time
    , done : Time
    }


type ShowHideState
    = Show
    | Hide


type alias ShowHideAnimation =
    Animation ShowHideState


type alias ShowHideStateAnimationDelays =
    { show : AnimationDelays
    , hide : AnimationDelays
    }


standardShortDelays : AnimationDelays
standardShortDelays =
    { setup = 10
    , animate = 150
    , done = 0
    }


standardMediumDelays : AnimationDelays
standardMediumDelays =
    { setup = 10
    , animate = 300
    , done = 0
    }


standardLongDelays : AnimationDelays
standardLongDelays =
    { setup = 10
    , animate = 1000
    , done = 0
    }


delaysForShowHideState : ShowHideStateAnimationDelays -> ShowHideState -> AnimationDelays
delaysForShowHideState delays state =
    case state of
        Hide ->
            delays.hide

        Show ->
            delays.show


transitionTask : Animation state -> Time -> Cmd (Animation state)
transitionTask animation delay =
    Task.perform identity (always animation) (Process.sleep <| delay * millisecond)


startShowHideAnimation : ShowHideStateAnimationDelays -> ShowHideAnimation -> Cmd ShowHideAnimation
startShowHideAnimation delays animation =
    let
        delaysRecord =
            delaysForShowHideState delays
    in
        case animation of
            Done Hide ->
                transitionTask (Setup Show) (delaysRecord Show).setup

            Done Show ->
                transitionTask (Setup Hide) (delaysRecord Hide).setup

            _ ->
                Cmd.none


transitionShowHideAnimationState : ShowHideStateAnimationDelays -> ShowHideAnimation -> Cmd ShowHideAnimation
transitionShowHideAnimationState delays currentAnimation =
    let
        delaysRecord =
            delaysForShowHideState delays
    in
        case currentAnimation of
            Setup state ->
                transitionTask (Animate state) (delaysRecord state).setup

            Animate state ->
                transitionTask (Done state) (delaysRecord state).animate

            Done _ ->
                Cmd.none


classesForShowHideStateAnimations : ShowHideAnimation -> String
classesForShowHideStateAnimations animation =
    case animation of
        Setup Show ->
            "animating"

        Animate Show ->
            "animating shown"

        Done Show ->
            "shown"

        Setup Hide ->
            "animating shown"

        Animate Hide ->
            "animating"

        Done Hide ->
            ""


invertShowHideState : ShowHideState -> ShowHideState
invertShowHideState state =
    case state of
        Show ->
            Hide

        Hide ->
            Show


invertShowHideStateForAnimation : ShowHideAnimation -> ShowHideAnimation
invertShowHideStateForAnimation animation =
    case animation of
        Setup a ->
            Setup <| invertShowHideState a

        Animate a ->
            Animate <| invertShowHideState a

        Done a ->
            Done <| invertShowHideState a


classesForShowHideStateAnimationInverted : ShowHideAnimation -> String
classesForShowHideStateAnimationInverted =
    invertShowHideStateForAnimation >> classesForShowHideStateAnimations


showHideIsInTransitTo : ShowHideState -> ShowHideAnimation -> Bool
showHideIsInTransitTo state animation =
    case animation of
        Setup a ->
            a == state

        Animate a ->
            a == state

        Done a ->
            a == state


showHideIsInTransitToShow : ShowHideAnimation -> Bool
showHideIsInTransitToShow =
    showHideIsInTransitTo Show


showHideIsInTransitToHide : ShowHideAnimation -> Bool
showHideIsInTransitToHide =
    showHideIsInTransitTo Hide
