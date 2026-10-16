module UI.Alert exposing
    ( Alert, Ready, NeedsTitle
    , new
    , title, description, icon, action
    , toHtml
    , destructive, colors
    )

{-| Displays a callout for user attention.

An alert is built with `new`, filled in with setters, and rendered with `toHtml`:

    Alert.new
        |> Alert.title [ text "Heads up!" ]
        |> Alert.description [ text "You can add components to your app using the CLI." ]
        |> Alert.toHtml []

Every alert needs a title. `toHtml` won't accept an alert until it has one:

    Alert.new
        |> Alert.description [ text "No title yet" ]
        |> Alert.toHtml []
        -- compile error: the alert is still `NeedsTitle`

An optional icon is placed in its own column, and an action is pinned to the top-right corner:

    Alert.new
        |> Alert.destructive
        |> Alert.icon (Phosphor.warning Regular |> Phosphor.toHtml [])
        |> Alert.title [ text "Update available" ]
        |> Alert.action
            (Button.new
                |> Button.text "Update"
                |> Button.outline
                |> Button.toHtml []
            )
        |> Alert.toHtml [ class "max-w-md" ]

@docs Alert, Ready, NeedsTitle


# Constructor

@docs new


# Setters

@docs title, description, icon, action


# Rendering

@docs toHtml


# Variants

The default variant is neutral. When several variants are given, the last one wins.

@docs destructive, colors

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, class)


{-| An alert being built.

`status` is [`Ready`](#Ready) once the alert can be rendered, or [`NeedsTitle`](#NeedsTitle) while it still lacks its title.

-}
type Alert status msg
    = Alert
        { variant : Variant
        , icon : Maybe (Html msg)
        , title : List (Html msg)
        , description : List (Html msg)
        , action : Maybe (Html msg)
        }


{-| The alert can be rendered.
-}
type Ready
    = Ready


{-| An alert that still needs [`title`](#title) before it can be rendered.
-}
type NeedsTitle
    = NeedsTitle


type Variant
    = Default
    | Destructive
    | Custom String



-- CONSTRUCTOR


{-| An empty alert. Give it a [`title`](#title) to make it renderable.
-}
new : Alert NeedsTitle msg
new =
    Alert
        { variant = Default
        , icon = Nothing
        , title = []
        , description = []
        , action = Nothing
        }



-- SETTERS


{-| The heading of the alert. Required.
-}
title : List (Html msg) -> Alert status msg -> Alert Ready msg
title children (Alert config) =
    Alert { config | title = children }


{-| Supporting text below the title.
-}
description : List (Html msg) -> Alert status msg -> Alert status msg
description children (Alert config) =
    Alert { config | description = children }


{-| An icon shown beside the title and description.
-}
icon : Html msg -> Alert status msg -> Alert status msg
icon child (Alert config) =
    Alert { config | icon = Just child }


{-| A control, such as a button, positioned in the top-right corner.
-}
action : Html msg -> Alert status msg -> Alert status msg
action child (Alert config) =
    Alert { config | action = Just child }


{-| Red text, for errors.
-}
destructive : Alert status msg -> Alert status msg
destructive (Alert config) =
    Alert { config | variant = Destructive }


{-| Your own colour classes in place of the default card colours, so they never clash with them:

    Alert.new
        |> Alert.colors "border-amber-200 bg-amber-50 text-amber-900 dark:border-amber-900 dark:bg-amber-950 dark:text-amber-50"
        |> Alert.title [ text "Your subscription will expire in 3 days." ]
        |> Alert.toHtml []

-}
colors : String -> Alert status msg -> Alert status msg
colors classes (Alert config) =
    Alert { config | variant = Custom classes }



-- RENDERING


{-| Renders the alert with the given attributes after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> Alert Ready msg -> Html msg
toHtml attrs (Alert config) =
    div
        ([ attribute "data-slot" "alert"
         , attribute "role" "alert"
         , class "group/alert relative grid w-full gap-0.5 rounded-lg border px-2.5 py-2 text-left text-sm has-data-[slot=alert-action]:pr-18 has-[>svg]:grid-cols-[auto_1fr] has-[>svg]:gap-x-2 *:[svg]:row-span-2 *:[svg]:translate-y-0.5 *:[svg]:text-current *:[svg:not([class*='size-'])]:size-4"
         , class (variantClass config.variant)
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ config.icon
            , Just (slot "alert-title" "font-medium group-has-[>svg]/alert:col-start-2 [&_a]:underline [&_a]:underline-offset-3 [&_a]:hover:text-foreground" config.title)
            , if List.isEmpty config.description then
                Nothing

              else
                Just (slot "alert-description" "text-sm text-balance text-muted-foreground group-has-[>svg]/alert:col-start-2 md:text-pretty [&_a]:underline [&_a]:underline-offset-3 [&_a]:hover:text-foreground [&_p:not(:last-child)]:mb-4" config.description)
            , Maybe.map (\child -> slot "alert-action" "absolute top-2 right-2" [ child ]) config.action
            ]
        )


slot : String -> String -> List (Html msg) -> Html msg
slot name classes children =
    div [ attribute "data-slot" name, class classes ] children


variantClass : Variant -> String
variantClass variant =
    case variant of
        Default ->
            "bg-card text-card-foreground"

        Destructive ->
            "bg-card text-destructive *:data-[slot=alert-description]:text-destructive/90 *:[svg]:text-current"

        Custom classes ->
            classes
