module UI.Popover exposing
    ( Popover, Ready, NeedsId
    , new
    , id
    , trigger
    , title, description, content
    , top, left, right, inlineStart, inlineEnd
    , alignStart, alignEnd
    , toHtml
    )

{-| Displays rich content in a portal, triggered by a button.

A popover is built with `new`, given an `id` and a `trigger`, filled in with setters, and rendered with `toHtml`:

    Popover.new
        |> Popover.id "dimensions"
        |> Popover.trigger
            (Button.new
                |> Button.outline
                |> Button.toAttributes
            )
            [ text "Open popover" ]
        |> Popover.title [ text "Dimensions" ]
        |> Popover.description [ text "Set the dimensions for the layer." ]
        |> Popover.content
            [ Input.view [ placeholder "Width" ] ]
        |> Popover.toHtml []

The trigger is a `<button type="button">` whose look comes from the attributes you give it, such as the styles of a [`Button`](Button) through `Button.toAttributes`. The content is a native popover anchored to the trigger with CSS anchor positioning, so it opens over everything else, closes on Escape or a click outside, and moves to the opposite side when it doesn't fit, with no JavaScript, ports or component state. The `id` is required because the trigger and the popover find each other by it.

@docs Popover, Ready, NeedsId


# Constructor

@docs new


# Required

`toHtml` won't accept a popover until it has an `id`.

@docs id


# Trigger

@docs trigger


# Content

The header appears when the popover has a `title` or a `description`, and `content` follows it.

@docs title, description, content


# Placement

The popover opens below the trigger, centered on it, 4px away. These change the side and the alignment along that side. When several are given, the last one wins. `left` and `right` are physical sides; `inlineStart` and `inlineEnd` follow the text direction, so they swap in a `dir="rtl"` container.

@docs top, left, right, inlineStart, inlineEnd

@docs alignStart, alignEnd


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, type_)


{-| A popover being built.

`id` is [`Ready`](#Ready) once the popover has an [`id`](#id).

-}
type Popover id msg
    = Popover
        { id : String
        , trigger : Maybe ( List (Attribute msg), List (Html msg) )
        , title : List (Html msg)
        , description : List (Html msg)
        , content : List (Html msg)
        , side : Side
        , align : Align
        }


{-| The popover can be rendered.
-}
type Ready
    = Ready


{-| A popover that still needs an [`id`](#id) before it can be rendered.
-}
type NeedsId
    = NeedsId


type Side
    = Top
    | Bottom
    | Left
    | Right
    | InlineStart
    | InlineEnd


type Align
    = Start
    | Center
    | End



-- CONSTRUCTOR


{-| An empty popover. Give it an [`id`](#id) to make it renderable.
-}
new : Popover NeedsId msg
new =
    Popover
        { id = ""
        , trigger = Nothing
        , title = []
        , description = []
        , content = []
        , side = Bottom
        , align = Center
        }



-- REQUIRED


{-| The `id` of the trigger button. It must be unique on the page: the popover is named after it.
-}
id : String -> Popover NeedsId msg -> Popover Ready msg
id string (Popover config) =
    Popover { config | id = string }



-- TRIGGER


{-| The button that opens the popover, with the given attributes after the popover's own, so they can override them. Pass `Button.toAttributes` for the button styles, or `disabled True` to disable it.
-}
trigger : List (Attribute msg) -> List (Html msg) -> Popover id msg -> Popover id msg
trigger attrs children (Popover config) =
    Popover { config | trigger = Just ( attrs, children ) }



-- CONTENT


{-| The heading of the popover, which also names it for assistive technology.
-}
title : List (Html msg) -> Popover id msg -> Popover id msg
title children (Popover config) =
    Popover { config | title = children }


{-| Supporting text below the title.
-}
description : List (Html msg) -> Popover id msg -> Popover id msg
description children (Popover config) =
    Popover { config | description = children }


{-| The body of the popover, after the header. The popover is a column with a gap between its children.
-}
content : List (Html msg) -> Popover id msg -> Popover id msg
content children (Popover config) =
    Popover { config | content = children }



-- PLACEMENT


{-| Opens above the trigger, or below it when it doesn't fit above.
-}
top : Popover id msg -> Popover id msg
top =
    side Top


{-| Opens to the left of the trigger, or to the right when it doesn't fit.
-}
left : Popover id msg -> Popover id msg
left =
    side Left


{-| Opens to the right of the trigger, or to the left when it doesn't fit.
-}
right : Popover id msg -> Popover id msg
right =
    side Right


{-| Opens on the side where text starts, the left in left-to-right text and the right in right-to-left text, or on the opposite side when it doesn't fit.
-}
inlineStart : Popover id msg -> Popover id msg
inlineStart =
    side InlineStart


{-| Opens on the side where text ends, the right in left-to-right text and the left in right-to-left text, or on the opposite side when it doesn't fit.
-}
inlineEnd : Popover id msg -> Popover id msg
inlineEnd =
    side InlineEnd


side : Side -> Popover id msg -> Popover id msg
side s (Popover config) =
    Popover { config | side = s }


{-| Lines the popover up with the start of the trigger: its left edge when it opens above or below, its top edge when it opens to a side.
-}
alignStart : Popover id msg -> Popover id msg
alignStart (Popover config) =
    Popover { config | align = Start }


{-| Lines the popover up with the end of the trigger: its right edge when it opens above or below, its bottom edge when it opens to a side.
-}
alignEnd : Popover id msg -> Popover id msg
alignEnd (Popover config) =
    Popover { config | align = End }



-- RENDERING


{-| Renders the trigger followed by the popover, with the given attributes on the popover after the styles, so they can override them. Pass a width such as `class "w-64"` to change the default `w-72`.

The default width is the `--popover-width` variable, so `class "[--popover-width:20rem]"` works too.

-}
toHtml : List (Attribute msg) -> Popover Ready msg -> Html msg
toHtml attrs (Popover config) =
    let
        popoverId =
            config.id ++ "-content"

        titleId =
            config.id ++ "-title"

        descriptionId =
            config.id ++ "-description"

        anchor =
            "--popover-" ++ config.id

        hasTitle =
            not (List.isEmpty config.title)

        hasDescription =
            not (List.isEmpty config.description)
    in
    Html.span
        [ attribute "data-slot" "popover", class "contents" ]
        [ case config.trigger of
            Just ( triggerAttrs, children ) ->
                Html.button
                    ([ type_ "button"
                     , Html.Attributes.id config.id
                     , attribute "popovertarget" popoverId
                     , attribute "data-slot" "popover-trigger"
                     , attribute "style" ("anchor-name: " ++ anchor)
                     ]
                        ++ triggerAttrs
                    )
                    children

            Nothing ->
                Html.text ""
        , Html.div
            ([ Html.Attributes.id popoverId
             , attribute "popover" "auto"
             , attribute "role" "dialog"
             , attribute "data-slot" "popover-content"
             , attribute "data-side" (sideName config.side)
             , attribute "data-align" (alignName config.align)
             , attribute "style" ("position-anchor: " ++ anchor ++ "; " ++ placement config.side config.align)

             -- `open:flex` rather than `flex`: an author `display` would override the
             -- browser's `display: none` for a closed popover and leave it invisible
             -- but in the way of clicks. The width comes from a variable so that a
             -- `w-*` class passed to `toHtml` wins: Tailwind puts `w-(--…)` before
             -- every other `w-*`.
             , class "fixed isolate z-50 inset-auto w-(--popover-width) flex-col open:flex gap-2.5 rounded-lg border-0 bg-popover p-2.5 text-sm text-popover-foreground shadow-md ring-1 ring-foreground/10 outline-hidden [--popover-width:--spacing(72)]"
             , class "scale-95 opacity-0 ease-out open:scale-100 open:translate-0 open:opacity-100 starting:open:scale-95 starting:open:opacity-0"
             , class (slideClass config.side)
             , class "[transition:opacity_100ms,scale_100ms,translate_100ms,display_100ms_allow-discrete,overlay_100ms_allow-discrete]"

             -- The `::backdrop` covers the viewport, which makes iOS Safari stop sampling
             -- the page color for its bars and paint them with the body background.
             , class "backdrop:hidden"
             ]
                ++ (if hasTitle then
                        [ attribute "aria-labelledby" titleId ]

                    else
                        []
                   )
                ++ (if hasDescription then
                        [ attribute "aria-describedby" descriptionId ]

                    else
                        []
                   )
                ++ attrs
            )
            ((if hasTitle || hasDescription then
                [ Html.div
                    [ attribute "data-slot" "popover-header", class "flex flex-col gap-0.5 text-sm" ]
                    (List.filterMap identity
                        [ if hasTitle then
                            Just
                                (Html.h2
                                    [ Html.Attributes.id titleId
                                    , attribute "data-slot" "popover-title"
                                    , class "font-medium"
                                    ]
                                    config.title
                                )

                          else
                            Nothing
                        , if hasDescription then
                            Just
                                (Html.p
                                    [ Html.Attributes.id descriptionId
                                    , attribute "data-slot" "popover-description"
                                    , class "text-muted-foreground"
                                    ]
                                    config.description
                                )

                          else
                            Nothing
                        ]
                    )
                ]

              else
                []
             )
                ++ config.content
            )
        ]



-- PLACEMENT
--
-- The popover is placed with CSS anchor positioning. `position-area` picks the side
-- of the trigger and, with `span-*`, which edge of the trigger it lines up with. The
-- fallbacks flip it to the opposite side, then the opposite alignment, when it would
-- leave the viewport. The margin keeps it 4px off the trigger along the side axis
-- only, so the alignment edges stay flush.


placement : Side -> Align -> String
placement s a =
    let
        area =
            case ( s, a ) of
                ( Top, Center ) ->
                    "top"

                ( Top, Start ) ->
                    "top span-right"

                ( Top, End ) ->
                    "top span-left"

                ( Bottom, Center ) ->
                    "bottom"

                ( Bottom, Start ) ->
                    "bottom span-right"

                ( Bottom, End ) ->
                    "bottom span-left"

                ( Left, Center ) ->
                    "left"

                ( Left, Start ) ->
                    "left span-bottom"

                ( Left, End ) ->
                    "left span-top"

                ( Right, Center ) ->
                    "right"

                ( Right, Start ) ->
                    "right span-bottom"

                ( Right, End ) ->
                    "right span-top"

                ( InlineStart, Center ) ->
                    "self-inline-start"

                ( InlineStart, Start ) ->
                    "self-inline-start span-self-block-end"

                ( InlineStart, End ) ->
                    "self-inline-start span-self-block-start"

                ( InlineEnd, Center ) ->
                    "self-inline-end"

                ( InlineEnd, Start ) ->
                    "self-inline-end span-self-block-end"

                ( InlineEnd, End ) ->
                    "self-inline-end span-self-block-start"

        ( fallbacks, margin ) =
            case s of
                Top ->
                    ( "flip-block, flip-inline, flip-block flip-inline", "0.25rem 0" )

                Bottom ->
                    ( "flip-block, flip-inline, flip-block flip-inline", "0.25rem 0" )

                Left ->
                    ( "flip-inline, flip-block, flip-inline flip-block", "0 0.25rem" )

                Right ->
                    ( "flip-inline, flip-block, flip-inline flip-block", "0 0.25rem" )

                InlineStart ->
                    ( "flip-inline, flip-block, flip-inline flip-block", "0 0.25rem" )

                InlineEnd ->
                    ( "flip-inline, flip-block, flip-inline flip-block", "0 0.25rem" )
    in
    "position-area: " ++ area ++ "; position-try-fallbacks: " ++ fallbacks ++ "; margin: " ++ margin


{-| Grows out of the trigger's side and slides 0.5rem towards it on the way in, like shadcn's `slide-in-from-*-2`.
-}
slideClass : Side -> String
slideClass s =
    case s of
        Top ->
            "origin-bottom translate-y-2 starting:open:translate-y-2"

        Bottom ->
            "origin-top -translate-y-2 starting:open:-translate-y-2"

        Left ->
            "origin-right translate-x-2 starting:open:translate-x-2"

        Right ->
            "origin-left -translate-x-2 starting:open:-translate-x-2"

        InlineStart ->
            "origin-right translate-x-2 starting:open:translate-x-2 rtl:origin-left rtl:-translate-x-2 rtl:starting:open:-translate-x-2"

        InlineEnd ->
            "origin-left -translate-x-2 starting:open:-translate-x-2 rtl:origin-right rtl:translate-x-2 rtl:starting:open:translate-x-2"


sideName : Side -> String
sideName s =
    case s of
        Top ->
            "top"

        Bottom ->
            "bottom"

        Left ->
            "left"

        Right ->
            "right"

        InlineStart ->
            "inline-start"

        InlineEnd ->
            "inline-end"


alignName : Align -> String
alignName a =
    case a of
        Start ->
            "start"

        Center ->
            "center"

        End ->
            "end"
