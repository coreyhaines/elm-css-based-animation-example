module Start exposing (..)

import Animation
import Html.App as App
import Html exposing (Html, text, div, h1)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


type alias Model =
    { name : String
    , faderAnimation : Animation.ShowHideAnimation
    , fader2Animation : Animation.ShowHideAnimation
    }


type Msg
    = NoOp
    | ShowFader
    | TransitionFaderAnimation Animation.ShowHideAnimation
    | ShowFader2
    | TransitionFader2Animation Animation.ShowHideAnimation


faderDelay : Animation.ShowHideStateAnimationDelays
faderDelay =
    { show = Animation.standardLongDelays
    , hide = Animation.standardLongDelays
    }


init : ( Model, Cmd Msg )
init =
    ( { name = "Elm Remote Conf"
      , faderAnimation = Animation.Done Animation.Hide
      , fader2Animation = Animation.Done Animation.Hide
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowFader ->
            let
                transitionCmd =
                    Animation.startShowHideAnimation faderDelay model.faderAnimation
            in
                ( model
                , Cmd.map TransitionFaderAnimation transitionCmd
                )

        TransitionFaderAnimation animation ->
            let
                _ =
                    Debug.log "Transition" animation

                transitionCmd =
                    Animation.transitionShowHideAnimationState faderDelay animation
            in
                ( { model | faderAnimation = animation }
                , Cmd.map TransitionFaderAnimation transitionCmd
                )

        ShowFader2 ->
            let
                transitionCmd =
                    Animation.startShowHideAnimation faderDelay model.fader2Animation
            in
                ( model
                , Cmd.map TransitionFader2Animation transitionCmd
                )

        TransitionFader2Animation animation ->
            let
                _ =
                    Debug.log "Transition2" animation

                transitionCmd =
                    Animation.transitionShowHideAnimationState faderDelay animation
            in
                ( { model | fader2Animation = animation }
                , Cmd.map TransitionFader2Animation transitionCmd
                )


fader : Model -> Html Msg
fader model =
    let
        contentHtml =
            case model.faderAnimation of
                Animation.Done (Animation.Hide) ->
                    text ""

                _ ->
                    div
                        [ classList
                            [ ( "fader", True )
                            , ( Animation.classesForShowHideStateAnimations model.faderAnimation, True )
                            ]
                        ]
                        [ text "This fades in and out" ]
    in
        div []
            [ div
                [ onClick ShowFader ]
                [ text "Fader" ]
            , contentHtml
            ]


twoFaders : Model -> Html Msg
twoFaders model =
    let
        contentHtml =
            div [ class "twoFader" ]
                [ div
                    [ classList
                        [ ( "fader", True )
                        , ( Animation.classesForShowHideStateAnimationInverted model.fader2Animation, True )
                        ]
                    ]
                    [ text <| "Hello, " ++ model.name ]
                , div
                    [ classList
                        [ ( "fader", True )
                        , ( Animation.classesForShowHideStateAnimations model.fader2Animation, True )
                        ]
                    ]
                    [ text <| "Goodbye, " ++ model.name ]
                ]
    in
        div []
            [ div [ onClick ShowFader2 ]
                [ text "Two Faders" ]
            , contentHtml
            ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "OMG! CSS TRANSITIONS" ]
        , div [ class "left" ]
            [ fader model ]
        , div [ class "right" ]
            [ twoFaders model ]
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
