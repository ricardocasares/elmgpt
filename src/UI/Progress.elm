module UI.Progress exposing
    ( Progress
    , new
    , label, value, formatValue
    , toHtml
    )

{-| Displays an indicator showing the completion progress of a task.

A progress bar is built with `new`, given a percentage, and rendered with `toHtml`:

    Progress.new 33
        |> Progress.toHtml []

A label and the value can be shown above the track. The value is formatted as a percentage:

    Progress.new 56
        |> Progress.label [ text "Upload progress" ]
        |> Progress.value
        |> Progress.toHtml [ class "w-full max-w-sm" ]

The percentage is exposed to CSS as `--progress-value` on the root, so the indicator width,
and anything else that should follow it, can be driven from a class:

    Progress.new 80
        |> Progress.toHtml [ class "[&_[data-slot=progress-indicator]]:bg-green-600" ]

@docs Progress


# Constructor

@docs new


# Setters

@docs label, value, formatValue


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


{-| A progress bar being built.
-}
type Progress msg
    = Progress
        { percentage : Float
        , label : Maybe (List (Html msg))
        , value : Maybe (Float -> String)
        }



-- CONSTRUCTOR


{-| A progress bar at the given percentage, from 0 to 100. Values outside that range are clamped.
-}
new : Float -> Progress msg
new percentage =
    Progress
        { percentage = clamp 0 100 percentage
        , label = Nothing
        , value = Nothing
        }



-- SETTERS


{-| A label shown above the track, at the start.
-}
label : List (Html msg) -> Progress msg -> Progress msg
label children (Progress config) =
    Progress { config | label = Just children }


{-| Shows the percentage above the track, at the end, as `56%`.
-}
value : Progress msg -> Progress msg
value =
    formatValue (\percentage -> String.fromFloat percentage ++ "%")


{-| Shows the percentage above the track, formatted by the given function.

    Progress.new 42
        |> Progress.formatValue (\p -> String.fromFloat p ++ " of 100 files")
        |> Progress.toHtml []

-}
formatValue : (Float -> String) -> Progress msg -> Progress msg
formatValue format (Progress config) =
    Progress { config | value = Just format }



-- RENDERING


{-| Renders the progress bar. The attributes go on the root, after the styles, so classes can override them.

The root carries `--progress-value` as a percentage and the `progressbar` role with its `aria-value*` attributes.

-}
toHtml : List (Attribute msg) -> Progress msg -> Html msg
toHtml attrs (Progress config) =
    let
        percentage =
            String.fromFloat config.percentage ++ "%"
    in
    Html.div
        ([ attribute "data-slot" "progress"
         , attribute "role" "progressbar"
         , attribute "aria-valuemin" "0"
         , attribute "aria-valuemax" "100"
         , attribute "aria-valuenow" (String.fromFloat config.percentage)
         , attribute "aria-valuetext" (Maybe.withDefault (\_ -> percentage) config.value config.percentage)

         -- `Html.Attributes.style` assigns `node.style[name]`, which browsers ignore for custom
         -- properties, so the variable goes through the attribute instead.
         , attribute "style" ("--progress-value: " ++ percentage)
         , class "flex flex-wrap gap-3"
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Maybe.map viewLabel config.label
            , Maybe.map (viewValue config.percentage) config.value
            , Just viewTrack
            ]
        )


viewLabel : List (Html msg) -> Html msg
viewLabel children =
    Html.span
        [ attribute "data-slot" "progress-label"
        , class "text-sm font-medium"
        ]
        children


viewValue : Float -> (Float -> String) -> Html msg
viewValue percentage format =
    Html.span
        [ attribute "data-slot" "progress-value"
        , class "ml-auto text-sm text-muted-foreground tabular-nums"
        ]
        [ Html.text (format percentage) ]


viewTrack : Html msg
viewTrack =
    Html.div
        [ attribute "data-slot" "progress-track"
        , class "relative flex h-1 w-full items-center overflow-x-hidden rounded-full bg-muted"
        ]
        [ Html.div
            [ attribute "data-slot" "progress-indicator"
            , class "h-full w-(--progress-value) bg-primary transition-all"
            ]
            []
        ]
