module UI.Attachment exposing
    ( Attachment, Ready, NeedsTitle
    , new
    , title, description
    , icon, image
    , actions, action
    , link, button
    , idle, uploading, processing, error
    , small, extraSmall
    , vertical
    , toHtml
    , group
    )

{-| Displays a file or image attachment with media, metadata, upload state, and actions.

An attachment is built with `new`, given its title, filled in with setters, and rendered with `toHtml`:

    Attachment.new
        |> Attachment.title "sales-dashboard.pdf"
        |> Attachment.description "PDF · 2.4 MB"
        |> Attachment.icon (Phosphor.fileText Regular |> Phosphor.toHtml [])
        |> Attachment.actions
            [ Button.new
                |> Button.icon (Phosphor.x Regular |> Phosphor.toHtml [])
                |> Button.label "Remove sales-dashboard.pdf"
                |> Attachment.action [ onClick Remove ]
            ]
        |> Attachment.toHtml [ class "w-full" ]

Every attachment needs a title. `toHtml` won't accept one until it has it:

    Attachment.new
        |> Attachment.description "PDF · 2.4 MB"
        |> Attachment.toHtml []
        -- compile error: the attachment is still `NeedsTitle`

The media beside the content is either an `icon`, placed on a muted square, or an `image`:

    Attachment.new
        |> Attachment.title "workspace.png"
        |> Attachment.description "PNG · 820 KB"
        |> Attachment.image [ src "/workspace.png", alt "Workspace" ]
        |> Attachment.vertical
        |> Attachment.toHtml []

Use it for files and images in chat composers, message threads, and upload lists.

@docs Attachment, Ready, NeedsTitle


# Constructor

@docs new


# Content

@docs title, description


# Media

`icon` and `image` fill the same slot, so the last one given wins. A `Spinner` in the `icon` slot shows an upload in progress.

@docs icon, image


# Actions

@docs actions, action


# Trigger

A trigger makes the whole attachment open a link or a dialog. It fills the attachment behind the actions, so the actions stay clickable and both remain separately focusable. The trigger has no text of its own, so give it an `aria-label` for what activating it does.

@docs link, button


# State

The default state is done. `uploading` and `processing` shimmer the title, `error` switches to a destructive treatment and `idle` draws a dashed border. The `error` state uses color, so keep the failure reason in the `description`. When several states are given, the last one wins.

@docs idle, uploading, processing, error


## Sizes

The default size is the largest. When several sizes are given, the last one wins.

@docs small, extraSmall


## Orientation

@docs vertical


# Rendering

@docs toHtml

A horizontal attachment is as wide as its content. Add `w-full` or a `w-*` class to size it.


# Group

@docs group

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, class)
import UI.Button as Button exposing (Button)


{-| An attachment being built.

`status` is [`Ready`](#Ready) once the attachment can be rendered, or [`NeedsTitle`](#NeedsTitle) while it still lacks its title.

-}
type Attachment status msg
    = Attachment
        { state : State
        , size : Size
        , orientation : Orientation
        , media : Maybe (Media msg)
        , title : String
        , description : Maybe String
        , actions : List (Html msg)
        , trigger : Maybe (Html msg)
        }


{-| The attachment can be rendered.
-}
type Ready
    = Ready


{-| An attachment that still needs a [`title`](#title) before it can be rendered.
-}
type NeedsTitle
    = NeedsTitle


type State
    = Idle
    | Uploading
    | Processing
    | Error
    | Done


type Size
    = Default
    | Small
    | ExtraSmall


type Orientation
    = Horizontal
    | Vertical


type Media msg
    = Icon (Html msg)
    | Image (List (Attribute msg))



-- CONSTRUCTOR


{-| A blank attachment. Give it a [`title`](#title) to make it renderable.
-}
new : Attachment NeedsTitle msg
new =
    Attachment
        { state = Done
        , size = Default
        , orientation = Horizontal
        , media = Nothing
        , title = ""
        , description = Nothing
        , actions = []
        , trigger = Nothing
        }



-- CONTENT


{-| The name of the attachment. Required. Long names are truncated.
-}
title : String -> Attachment status msg -> Attachment Ready msg
title string (Attachment config) =
    Attachment { config | title = string }


{-| Secondary metadata such as the file type, size, or upload status.
-}
description : String -> Attachment status msg -> Attachment status msg
description string (Attachment config) =
    Attachment { config | description = Just string }



-- MEDIA


{-| An icon, or a spinner, placed on a muted, rounded square beside the content.
-}
icon : Html msg -> Attachment status msg -> Attachment status msg
icon child (Attachment config) =
    Attachment { config | media = Just (Icon child) }


{-| An image preview, rendered as an `<img>` with the given attributes. Pass `src` and `alt`. The image is dimmed while the attachment is in progress or failed.
-}
image : List (Attribute msg) -> Attachment status msg -> Attachment status msg
image attrs (Attachment config) =
    Attachment { config | media = Just (Image attrs) }



-- ACTIONS


{-| One or more actions, aligned to the end of the attachment, or to its top right corner when vertical. Build each one with [`action`](#action).
-}
actions : List (Html msg) -> Attachment status msg -> Attachment status msg
actions children (Attachment config) =
    Attachment { config | actions = children }


{-| An action button. It takes an icon `Button` without its `toHtml`, makes it a ghost, extra small button, and renders it with the given attributes. The `Button` type requires a `label`, so every action has an accessible name:

    Button.new
        |> Button.icon (Phosphor.x Regular |> Phosphor.toHtml [])
        |> Button.label "Remove sales-dashboard.pdf"
        |> Attachment.action [ onClick Remove ]

-}
action : List (Attribute msg) -> Button Button.Icon Button.Ready msg -> Html msg
action attrs value =
    value
        |> Button.ghost
        |> Button.extraSmall
        |> Button.toHtml (attribute "data-slot" "attachment-action" :: attrs)



-- TRIGGER


{-| An `<a>` covering the attachment. Pass `href` and an `aria-label`:

    Attachment.link
        [ href url
        , target "_blank"
        , rel "noreferrer"
        , attribute "aria-label" "Open workspace.png"
        ]

-}
link : List (Attribute msg) -> Attachment status msg -> Attachment status msg
link attrs (Attachment config) =
    Attachment { config | trigger = Just (trigger Html.a attrs) }


{-| A `<button>` covering the attachment, for an attachment that opens a dialog or does something on click. Pass an `aria-label`:

    Attachment.button
        [ attribute "aria-label" "Preview research-summary.pdf"
        , onClick (SetPreviewing True)
        ]

-}
button : List (Attribute msg) -> Attachment status msg -> Attachment status msg
button attrs (Attachment config) =
    Attachment { config | trigger = Just (trigger Html.button (Html.Attributes.type_ "button" :: attrs)) }


trigger : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> Html msg
trigger element attrs =
    element
        (attribute "data-slot" "attachment-trigger"
            :: class "absolute inset-0 z-10 outline-none"
            :: attrs
        )
        []



-- STATE


{-| Selected, but not uploaded yet. The border is dashed.
-}
idle : Attachment status msg -> Attachment status msg
idle =
    state Idle


{-| The title shimmers.
-}
uploading : Attachment status msg -> Attachment status msg
uploading =
    state Uploading


{-| The title shimmers.
-}
processing : Attachment status msg -> Attachment status msg
processing =
    state Processing


{-| The border, media and description turn destructive.
-}
error : Attachment status msg -> Attachment status msg
error =
    state Error


state : State -> Attachment status msg -> Attachment status msg
state value (Attachment config) =
    Attachment { config | state = value }



-- SIZES


{-| -}
small : Attachment status msg -> Attachment status msg
small =
    size Small


{-| -}
extraSmall : Attachment status msg -> Attachment status msg
extraSmall =
    size ExtraSmall


size : Size -> Attachment status msg -> Attachment status msg
size value (Attachment config) =
    Attachment { config | size = value }



-- ORIENTATION


{-| Stacks the media above the content, in a fixed-width card. The actions move to the top right corner.
-}
vertical : Attachment status msg -> Attachment status msg
vertical (Attachment config) =
    Attachment { config | orientation = Vertical }



-- RENDERING


{-| Renders the attachment with the given attributes after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> Attachment Ready msg -> Html msg
toHtml attrs (Attachment config) =
    div
        ([ attribute "data-slot" "attachment"
         , attribute "data-state" (stateName config.state)
         , attribute "data-size" (sizeName config.size)
         , attribute "data-orientation" (orientationName config.orientation)
         , class "group/attachment relative flex max-w-full min-w-0 shrink-0 flex-wrap border bg-card text-card-foreground transition-colors has-[>a,>button]:hover:bg-muted/50 data-[state=error]:border-destructive/30 data-[state=idle]:border-dashed focus-within:ring-1 focus-within:ring-ring/50"
         , class (sizeClass config.size)
         , class (orientationClass config.orientation)
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Maybe.map viewMedia config.media
            , Just
                (div
                    [ attribute "data-slot" "attachment-content"
                    , class "max-w-full min-w-0 flex-1 leading-tight group-data-[orientation=vertical]/attachment:px-1"
                    ]
                    (List.filterMap identity
                        [ Just
                            (Html.span
                                [ attribute "data-slot" "attachment-title"
                                , class "block max-w-full min-w-0 truncate font-medium group-data-[state=processing]/attachment:shimmer group-data-[state=uploading]/attachment:shimmer"
                                ]
                                [ Html.text config.title ]
                            )
                        , Maybe.map
                            (\string ->
                                Html.span
                                    [ attribute "data-slot" "attachment-description"
                                    , class "mt-0.5 block max-w-full min-w-0 truncate text-xs text-muted-foreground group-data-[state=error]/attachment:text-destructive/80"
                                    ]
                                    [ Html.text string ]
                            )
                            config.description
                        ]
                    )
                )
            , if List.isEmpty config.actions then
                Nothing

              else
                Just
                    (div
                        [ attribute "data-slot" "attachment-actions"
                        , class "relative z-20 flex shrink-0 items-center group-data-[orientation=vertical]/attachment:absolute group-data-[orientation=vertical]/attachment:top-3 group-data-[orientation=vertical]/attachment:right-3 group-data-[orientation=vertical]/attachment:gap-1"
                        ]
                        config.actions
                    )
            , config.trigger
            ]
        )


viewMedia : Media msg -> Html msg
viewMedia value =
    let
        ( variant, extra, children ) =
            case value of
                Icon child ->
                    ( "icon", "", [ child ] )

                Image attrs ->
                    ( "image"
                    , "opacity-60 group-data-[state=idle]/attachment:opacity-100 group-data-[state=done]/attachment:opacity-100 *:[img]:aspect-square *:[img]:w-full *:[img]:object-cover"
                    , [ Html.img attrs [] ]
                    )
    in
    div
        [ attribute "data-slot" "attachment-media"
        , attribute "data-variant" variant
        , class "relative flex aspect-square w-10 shrink-0 items-center justify-center overflow-hidden rounded-lg bg-muted text-foreground group-data-[size=sm]/attachment:w-8 group-data-[size=xs]/attachment:w-7 group-data-[size=xs]/attachment:rounded-md group-data-[state=error]/attachment:bg-destructive/10 group-data-[state=error]/attachment:text-destructive group-data-[orientation=vertical]/attachment:w-full [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 group-data-[size=xs]/attachment:[&_svg:not([class*='size-'])]:size-3.5 group-data-[orientation=vertical]/attachment:[&_svg:not([class*='size-'])]:size-6 group-data-[orientation=vertical]/attachment:*:data-[slot=spinner]:size-6!"
        , class extra
        ]
        children



-- shadcn merges its base, size and orientation classes with tailwind-merge, which
-- drops the base classes a size or orientation overrides. Elm keeps every class, so
-- the radius lives in `sizeClass` and the width in `orientationClass`.


sizeClass : Size -> String
sizeClass value =
    case value of
        Default ->
            "gap-2 rounded-xl text-sm has-data-[slot=attachment-content]:px-2.5 has-data-[slot=attachment-content]:py-2 has-data-[slot=attachment-media]:p-2"

        Small ->
            "gap-2.5 rounded-xl text-xs has-data-[slot=attachment-content]:px-2 has-data-[slot=attachment-content]:py-1.5 has-data-[slot=attachment-media]:p-1.5"

        ExtraSmall ->
            "gap-1.5 rounded-lg text-xs has-data-[slot=attachment-content]:px-1.5 has-data-[slot=attachment-content]:py-1 has-data-[slot=attachment-media]:p-1"


orientationClass : Orientation -> String
orientationClass value =
    case value of
        Horizontal ->
            "w-fit min-w-40 items-center"

        Vertical ->
            "w-24 flex-col has-data-[slot=attachment-content]:w-30"


stateName : State -> String
stateName value =
    case value of
        Idle ->
            "idle"

        Uploading ->
            "uploading"

        Processing ->
            "processing"

        Error ->
            "error"

        Done ->
            "done"


sizeName : Size -> String
sizeName value =
    case value of
        Default ->
            "default"

        Small ->
            "sm"

        ExtraSmall ->
            "xs"


orientationName : Orientation -> String
orientationName value =
    case value of
        Horizontal ->
            "horizontal"

        Vertical ->
            "vertical"



-- GROUP


{-| Lays out attachments in a horizontally scrollable, snapping row with an edge fade. The attachments keep their own width.

    Attachment.group [ class "w-full" ]
        [ Attachment.new
            |> Attachment.title "workspace.png"
            |> Attachment.image [ src "/workspace.png", alt "Workspace" ]
            |> Attachment.vertical
            |> Attachment.toHtml []
        , Attachment.new
            |> Attachment.title "desk.jpg"
            |> Attachment.image [ src "/desk.jpg", alt "Desk" ]
            |> Attachment.vertical
            |> Attachment.toHtml []
        ]

For a row of attachments without actions or a trigger, add `tabindex 0`, `role "group"` and an `aria-label` so keyboard users can scroll it.

-}
group : List (Attribute msg) -> List (Html msg) -> Html msg
group attrs children =
    div
        ([ attribute "data-slot" "attachment-group"
         , class "flex min-w-0 gap-3 scroll-px-1 py-1 scroll-fade-x snap-x snap-mandatory scrollbar-none overflow-x-auto overscroll-x-contain *:data-[slot=attachment]:flex-none *:data-[slot=attachment]:snap-start"
         ]
            ++ attrs
        )
        children
