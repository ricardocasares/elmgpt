module UI.InputGroup exposing
    ( view
    , input, textarea
    , start, end, top, bottom
    , text, button
    )

{-| Displays additional information or actions to an input or textarea.

A group is a bordered box that holds one control and any number of addons. The border, focus ring and error state move from the control to the box, so the addons sit inside them:

    InputGroup.view []
        [ InputGroup.input [ placeholder "Search..." ]
        , InputGroup.start [] [ Phosphor.magnifyingGlass Regular |> Phosphor.toHtml [] ]
        , InputGroup.end [] [ Html.text "12 results" ]
        ]

The control comes first in the markup, before the addons. `start` and `end` addons are ordered for you, and the box grows with a `textarea` or with a `top` or `bottom` addon:

    InputGroup.view []
        [ InputGroup.textarea [ placeholder "Share your thoughts..." ]
        , InputGroup.bottom []
            [ InputGroup.text [] [ Html.text "0/500 characters" ]
            , InputGroup.button
                |> Button.icon (Phosphor.arrowUp Regular |> Phosphor.toHtml [])
                |> Button.label "Send"
                |> Button.toHtml [ class "ms-auto", onClick Send ]
            ]
        ]

Set `aria-invalid` on the control to show the error state on the whole group, and `disabled` to dim it.

@docs view


# Controls

@docs input, textarea


# Addons

An addon is a row of icons, text, keys or buttons. Its position in the group is the function you pick, not the order in the children.

@docs start, end, top, bottom


# Addon content

@docs text, button

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)
import UI.Button as Button exposing (Button, NeedsContent)


{-| Renders the group, a `<div role="group">` around one control and its addons.
-}
view : List (Attribute msg) -> List (Html msg) -> Html msg
view attrs children =
    Html.div
        ([ attribute "data-slot" "input-group"
         , attribute "role" "group"
         , class "group/input-group relative flex h-8 w-full min-w-0 items-center rounded-lg border border-input transition-colors outline-none has-[>textarea]:h-auto dark:bg-input/30"
         , class "has-[[data-slot=input-group-control]:focus-visible]:border-ring has-[[data-slot=input-group-control]:focus-visible]:ring-3 has-[[data-slot=input-group-control]:focus-visible]:ring-ring/50"
         , class "has-[[data-slot][aria-invalid=true]]:border-destructive has-[[data-slot][aria-invalid=true]]:ring-3 has-[[data-slot][aria-invalid=true]]:ring-destructive/20 dark:has-[[data-slot][aria-invalid=true]]:ring-destructive/40"
         , class "has-disabled:bg-input/50 has-disabled:opacity-50 dark:has-disabled:bg-input/80"
         , class "has-[>[data-align=block-start]]:h-auto has-[>[data-align=block-start]]:flex-col has-[>[data-align=block-start]]:[&>input]:pb-3 has-[>[data-align=block-end]]:h-auto has-[>[data-align=block-end]]:flex-col has-[>[data-align=block-end]]:[&>input]:pt-3"
         , class "has-[>[data-align=inline-start]]:[&>input]:ps-1.5 has-[>[data-align=inline-end]]:[&>input]:pe-1.5"
         ]
            ++ attrs
        )
        children



-- CONTROLS


{-| An `<input>` without its own border and ring, since the group draws them. Takes the same attributes as `Input.view`.
-}
input : List (Attribute msg) -> Html msg
input attrs =
    Html.input
        (attribute "data-slot" "input-group-control"
            :: class "h-8 w-full min-w-0 flex-1 rounded-none border-0 bg-transparent px-2.5 py-1 text-base outline-none file:inline-flex file:h-6 file:border-0 file:bg-transparent file:text-sm file:font-medium file:text-foreground placeholder:text-muted-foreground disabled:pointer-events-none disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
            :: attrs
        )
        []


{-| A `<textarea>` without its own border and ring. It grows with its content and can't be resized by hand.
-}
textarea : List (Attribute msg) -> Html msg
textarea attrs =
    Html.textarea
        (attribute "data-slot" "input-group-control"
            :: class "flex field-sizing-content min-h-16 w-full flex-1 resize-none rounded-none border-0 bg-transparent px-2.5 py-2 text-base outline-none placeholder:text-muted-foreground disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
            :: attrs
        )
        []



-- ADDONS


{-| An addon before the control, on the same line.
-}
start : List (Attribute msg) -> List (Html msg) -> Html msg
start =
    addon "inline-start" "order-first ps-2 has-[>button]:ms-[-0.3rem] has-[>kbd]:ms-[-0.15rem]"


{-| An addon after the control, on the same line.
-}
end : List (Attribute msg) -> List (Html msg) -> Html msg
end =
    addon "inline-end" "order-last pe-2 has-[>button]:me-[-0.3rem] has-[>kbd]:me-[-0.15rem]"


{-| A full-width addon above the control. Add `border-b` to separate it with a line.
-}
top : List (Attribute msg) -> List (Html msg) -> Html msg
top =
    addon "block-start" "order-first w-full justify-start px-2.5 pt-2 group-has-[>input]/input-group:pt-2 [.border-b]:pb-2"


{-| A full-width addon below the control. Add `border-t` to separate it with a line.
-}
bottom : List (Attribute msg) -> List (Html msg) -> Html msg
bottom =
    addon "block-end" "order-last w-full justify-start px-2.5 pb-2 group-has-[>input]/input-group:pb-2 [.border-t]:pt-2"


addon : String -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
addon align classes attrs children =
    Html.div
        ([ attribute "data-slot" "input-group-addon"
         , attribute "data-align" align
         , attribute "role" "group"
         , class "flex h-auto items-center justify-center gap-2 py-1.5 text-sm font-medium text-muted-foreground select-none group-data-[disabled=true]/input-group:opacity-50 [&>kbd]:rounded-[calc(var(--radius)-5px)] [&>svg:not([class*='size-'])]:size-4"
         , class classes
         ]
            ++ attrs
        )
        children



-- ADDON CONTENT


{-| Muted text inside an addon, such as a prefix, a hint or a character count. An icon inside it is sized to match.
-}
text : List (Attribute msg) -> List (Html msg) -> Html msg
text attrs children =
    Html.span
        (attribute "data-slot" "input-group-text"
            :: class "flex items-center gap-2 text-sm text-muted-foreground [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4"
            :: attrs
        )
        children


{-| A button sized for an addon: a ghost, extra small `Button` ready for its content. Any `Button` variant or size can replace the defaults:

    InputGroup.button
        |> Button.text "Copy"
        |> Button.secondary
        |> Button.toHtml [ onClick Copy ]

-}
button : Button shape NeedsContent msg
button =
    Button.new
        |> Button.ghost
        |> Button.extraSmall
