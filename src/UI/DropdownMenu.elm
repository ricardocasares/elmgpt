module UI.DropdownMenu exposing
    ( DropdownMenu, Ready, NeedsId
    , new
    , id
    , trigger
    , content
    , Item, item, checkboxItem, radioItem, submenu
    , destructive, inset, disabled
    , group, label, separator
    , shortcut
    , top, left, right
    , alignCenter, alignEnd
    , toHtml
    )

{-| Displays a menu to the user, such as a set of actions or functions, triggered by a button.

A dropdown menu is built with `new`, given an `id` and a `trigger`, filled with `content`, and rendered with `toHtml`:

    DropdownMenu.new
        |> DropdownMenu.id "account"
        |> DropdownMenu.trigger
            (Button.new
                |> Button.outline
                |> Button.toAttributes
            )
            [ text "Open" ]
        |> DropdownMenu.content
            [ DropdownMenu.group
                [ DropdownMenu.label [ text "My Account" ]
                , DropdownMenu.item [ onClick Profile ] [ text "Profile" ]
                , DropdownMenu.item [ onClick Billing ] [ text "Billing" ]
                ]
            , DropdownMenu.separator
            , DropdownMenu.item [] [ text "GitHub" ]
            , DropdownMenu.item [] [ text "API" ]
                |> DropdownMenu.disabled
            ]
        |> DropdownMenu.toHtml []

The trigger is a `<button type="button">` whose look comes from the attributes you give it, such as the styles of a [`Button`](Button) through `Button.toAttributes`. The menu is a native popover anchored to the trigger with CSS anchor positioning, so it opens over everything else, closes on Escape, a click outside or a click on an item, moves to the opposite side when it doesn't fit, and is navigated with the arrow keys (a `focusgroup`), with no JavaScript, ports or component state. The `id` is required because the trigger and the menu find each other by it.

Items are buttons. Give them `onClick` to be told when they are picked; the menu closes by itself. Checkbox and radio items keep the menu open, and show the state you give them from your model.

@docs DropdownMenu, Ready, NeedsId


# Constructor

@docs new


# Required

`toHtml` won't accept a menu until it has an `id`.

@docs id


# Trigger

@docs trigger


# Content

@docs content


## Items

@docs Item, item, checkboxItem, radioItem, submenu

@docs destructive, inset, disabled


## Structure

@docs group, label, separator


## Inside an item

@docs shortcut


# Placement

The menu opens below the trigger, lined up with its left edge, 4px away. These change the side and the alignment along that side. When several are given, the last one wins.

@docs top, left, right

@docs alignCenter, alignEnd


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, autofocus, class, type_)


{-| A dropdown menu being built.

`id` is [`Ready`](#Ready) once the menu has an [`id`](#id).

-}
type DropdownMenu id msg
    = DropdownMenu
        { id : String
        , trigger : Maybe ( List (Attribute msg), List (Html msg) )
        , content : List (Item msg)
        , side : Side
        , align : Align
        }


{-| The menu can be rendered.
-}
type Ready
    = Ready


{-| A menu that still needs an [`id`](#id) before it can be rendered.
-}
type NeedsId
    = NeedsId


type Side
    = Top
    | Bottom
    | Left
    | Right


type Align
    = Start
    | Center
    | End


{-| An entry in the menu: an [`item`](#item) of some kind, a [`label`](#label), a [`separator`](#separator) or a [`group`](#group) of them.
-}
type Item msg
    = Item
        { kind : Kind
        , attrs : List (Attribute msg)
        , children : List (Html msg)
        , inset : Bool
        , disabled : Bool
        , destructive : Bool
        }
    | Submenu
        { children : List (Html msg)
        , items : List (Item msg)
        , inset : Bool
        , disabled : Bool
        }
    | Label Bool (List (Html msg))
    | Separator
    | Group (List (Item msg))


type Kind
    = Action
    | Checkbox Bool
    | Radio Bool



-- CONSTRUCTOR


{-| An empty menu. Give it an [`id`](#id) to make it renderable.
-}
new : DropdownMenu NeedsId msg
new =
    DropdownMenu
        { id = ""
        , trigger = Nothing
        , content = []
        , side = Bottom
        , align = Start
        }



-- REQUIRED


{-| The `id` of the trigger button. It must be unique on the page: the menu is named after it.
-}
id : String -> DropdownMenu NeedsId msg -> DropdownMenu Ready msg
id string (DropdownMenu config) =
    DropdownMenu { config | id = string }



-- TRIGGER


{-| The button that opens the menu, with the given attributes after the menu's own, so they can override them. Pass `Button.toAttributes` for the button styles, or `disabled True` to disable it.
-}
trigger : List (Attribute msg) -> List (Html msg) -> DropdownMenu id msg -> DropdownMenu id msg
trigger attrs children (DropdownMenu config) =
    DropdownMenu { config | trigger = Just ( attrs, children ) }



-- CONTENT


{-| The entries of the menu, in order.
-}
content : List (Item msg) -> DropdownMenu id msg -> DropdownMenu id msg
content items (DropdownMenu config) =
    DropdownMenu { config | content = items }


{-| An action, with the given attributes after the item's own, so they can override them. Pass `onClick` to be told when it is picked. Picking it closes the menu.
-}
item : List (Attribute msg) -> List (Html msg) -> Item msg
item attrs children =
    Item
        { kind = Action
        , attrs = attrs
        , children = children
        , inset = False
        , disabled = False
        , destructive = False
        }


{-| An item that can be checked, with a check mark on the right when it is. The state comes from your model, and `onClick` tells you to flip it. Picking it keeps the menu open.
-}
checkboxItem : Bool -> List (Attribute msg) -> List (Html msg) -> Item msg
checkboxItem checked attrs children =
    Item
        { kind = Checkbox checked
        , attrs = attrs
        , children = children
        , inset = False
        , disabled = False
        , destructive = False
        }


{-| One of a set of items of which only one is checked, with a check mark on the right when it is. The state comes from your model, and `onClick` tells you which one was picked. Picking it keeps the menu open.
-}
radioItem : Bool -> List (Attribute msg) -> List (Html msg) -> Item msg
radioItem checked attrs children =
    Item
        { kind = Radio checked
        , attrs = attrs
        , children = children
        , inset = False
        , disabled = False
        , destructive = False
        }


{-| An item that opens another menu at its end, to its right in left-to-right text and to its left in right-to-left text, or on the opposite side when it doesn't fit, with the given label and entries. A click, Enter or Space opens it, and Escape closes it alone. Picking an item in it closes every menu.
-}
submenu : List (Html msg) -> List (Item msg) -> Item msg
submenu children items =
    Submenu
        { children = children
        , items = items
        , inset = False
        , disabled = False
        }


{-| Shows the item in the destructive color.
-}
destructive : Item msg -> Item msg
destructive entry =
    case entry of
        Item config ->
            Item { config | destructive = True }

        _ ->
            entry


{-| Indents an item or a label to line up with items that have an icon or a check mark.
-}
inset : Item msg -> Item msg
inset entry =
    case entry of
        Item config ->
            Item { config | inset = True }

        Submenu config ->
            Submenu { config | inset = True }

        Label _ children ->
            Label True children

        _ ->
            entry


{-| Shows the item dimmed and makes it unpickable.
-}
disabled : Item msg -> Item msg
disabled entry =
    case entry of
        Item config ->
            Item { config | disabled = True }

        Submenu config ->
            Submenu { config | disabled = True }

        _ ->
            entry


{-| A set of related entries, usually under a [`label`](#label).
-}
group : List (Item msg) -> Item msg
group =
    Group


{-| A heading for the entries after it.
-}
label : List (Html msg) -> Item msg
label =
    Label False


{-| A line between entries.
-}
separator : Item msg
separator =
    Separator


{-| The keyboard shortcut of an item, shown at its right edge. It is only a hint: the key itself is up to you.

    DropdownMenu.item [ onClick Save ]
        [ text "Save"
        , DropdownMenu.shortcut [ text "⌘S" ]
        ]

-}
shortcut : List (Html msg) -> Html msg
shortcut children =
    Html.span
        [ attribute "data-slot" "dropdown-menu-shortcut"
        , class "ms-auto text-xs tracking-widest text-muted-foreground"
        ]
        children



-- PLACEMENT


{-| Opens above the trigger, or below it when it doesn't fit above.
-}
top : DropdownMenu id msg -> DropdownMenu id msg
top =
    side Top


{-| Opens to the left of the trigger, or to the right when it doesn't fit.
-}
left : DropdownMenu id msg -> DropdownMenu id msg
left =
    side Left


{-| Opens to the right of the trigger, or to the left when it doesn't fit.
-}
right : DropdownMenu id msg -> DropdownMenu id msg
right =
    side Right


side : Side -> DropdownMenu id msg -> DropdownMenu id msg
side s (DropdownMenu config) =
    DropdownMenu { config | side = s }


{-| Centers the menu on the trigger instead of lining it up with the trigger's start edge.
-}
alignCenter : DropdownMenu id msg -> DropdownMenu id msg
alignCenter (DropdownMenu config) =
    DropdownMenu { config | align = Center }


{-| Lines the menu up with the end of the trigger: its right edge when it opens above or below, its bottom edge when it opens to a side.
-}
alignEnd : DropdownMenu id msg -> DropdownMenu id msg
alignEnd (DropdownMenu config) =
    DropdownMenu { config | align = End }



-- RENDERING


{-| Renders the trigger followed by the menu, with the given attributes on the menu after the styles, so they can override them. The menu is at least as wide as the trigger and `min-w-32`; pass a width such as `class "w-44"` to fix it.
-}
toHtml : List (Attribute msg) -> DropdownMenu Ready msg -> Html msg
toHtml attrs (DropdownMenu config) =
    let
        menuId =
            config.id ++ "-menu"

        anchor =
            "--dropdown-menu-" ++ config.id
    in
    Html.span
        [ attribute "data-slot" "dropdown-menu", class "contents" ]
        [ case config.trigger of
            Just ( triggerAttrs, children ) ->
                Html.button
                    ([ type_ "button"
                     , Html.Attributes.id config.id
                     , attribute "popovertarget" menuId
                     , attribute "aria-haspopup" "menu"
                     , attribute "data-slot" "dropdown-menu-trigger"
                     , attribute "style" ("anchor-name: " ++ anchor)
                     ]
                        ++ triggerAttrs
                    )
                    children

            Nothing ->
                Html.text ""
        , Html.div
            ([ Html.Attributes.id menuId
             , attribute "popover" "auto"
             , attribute "role" "menu"
             , attribute "aria-labelledby" config.id

             -- Arrow keys move focus between the items, wrapping around at the ends.
             , attribute "focusgroup" "menu block wrap"
             , attribute "data-slot" "dropdown-menu-content"
             , attribute "data-side" (sideName config.side)
             , attribute "data-align" (alignName config.align)
             , attribute "style" ("position-anchor: " ++ anchor ++ "; " ++ placement config.side config.align ++ "; " ++ minHeight config.content)
             , class "min-w-[max(--spacing(32),anchor-size(width))]"
             , class (menuClass config.side)
             ]
                ++ attrs
            )
            (viewItems menuId menuId config.content)
        ]


{-| The classes shared by the menu and its submenus: a translucent popover that grows out of its trigger, with the arrow keys moving between its items.
-}
menuClass : Side -> String
menuClass s =
    String.join " "
        [ "group/menu fixed isolate z-50 m-0 inset-auto max-h-[calc(100%-0.5rem)] overflow-x-hidden overflow-y-auto rounded-lg border-0 p-1 text-sm text-popover-foreground shadow-md ring-1 ring-foreground/10 outline-hidden"
        , "scale-95 opacity-0 ease-out open:scale-100 open:translate-0 open:opacity-100 starting:open:scale-95 starting:open:opacity-0"
        , slideClass s
        , "[transition:opacity_100ms,scale_100ms,translate_100ms,display_100ms_allow-discrete,overlay_100ms_allow-discrete]"

        -- The blur is on the popover itself rather than a `::before`, which would
        -- scroll away with the content.
        , "bg-popover/70 backdrop-blur-2xl backdrop-saturate-150"

        -- The `::backdrop` covers the viewport, which makes iOS Safari stop sampling
        -- the page color for its bars and paint them with the body background.
        , "backdrop:hidden"
        ]


viewItems : String -> String -> List (Item msg) -> List (Html msg)
viewItems rootId prefix items =
    let
        -- The entry focused when the menu opens: the first that can be, counted
        -- among the focusable entries.
        first =
            items
                |> List.concatMap focusable
                |> List.indexedMap Tuple.pair
                |> List.filter Tuple.second
                |> List.head
                |> Maybe.map Tuple.first
    in
    viewList rootId prefix first 0 items
        |> Tuple.first


{-| Whether each focusable entry, in order, can be focused.
-}
focusable : Item msg -> List Bool
focusable entry =
    case entry of
        Item config ->
            [ not config.disabled ]

        Submenu config ->
            [ not config.disabled ]

        Group items ->
            List.concatMap focusable items

        _ ->
            []


{-| Renders the entries, counting the focusable ones from `index` so that each one knows its place and submenus get unique ids.
-}
viewList : String -> String -> Maybe Int -> Int -> List (Item msg) -> ( List (Html msg), Int )
viewList rootId prefix first index items =
    case items of
        [] ->
            ( [], index )

        entry :: rest ->
            let
                ( html, next ) =
                    viewItem rootId prefix first index entry

                ( htmls, final ) =
                    viewList rootId prefix first next rest
            in
            ( html :: htmls, final )


viewItem : String -> String -> Maybe Int -> Int -> Item msg -> ( Html msg, Int )
viewItem rootId prefix first index entry =
    let
        isFirst =
            first == Just index

        insetClass isInset =
            class
                (if isInset then
                    "ps-7"

                 else
                    "ps-1.5"
                )
    in
    case entry of
        Item config ->
            ( Html.button
                ([ type_ "button"
                 , attribute "role" (roleName config.kind)
                 , attribute "data-slot" (slotName config.kind)
                 , Html.Attributes.disabled config.disabled
                 , autofocus isFirst
                 , class itemClass
                 , insetClass config.inset
                 ]
                    ++ (case config.kind of
                            Action ->
                                [ attribute "popovertarget" rootId
                                , attribute "popovertargetaction" "hide"
                                , class "pe-1.5"
                                ]

                            Checkbox checked ->
                                [ attribute "aria-checked" (boolean checked), class "pe-8" ]

                            Radio checked ->
                                [ attribute "aria-checked" (boolean checked), class "pe-8" ]
                       )
                    ++ (if config.destructive then
                            [ attribute "data-variant" "destructive"

                            -- Through the `data-variant` so these win over the
                            -- `hover:bg-*` and `focus:bg-*` of every item.
                            , class "data-[variant=destructive]:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:focus:bg-destructive/10 dark:data-[variant=destructive]:hover:bg-destructive/20 dark:data-[variant=destructive]:focus:bg-destructive/20 data-[variant=destructive]:*:[svg]:text-destructive"
                            ]

                        else
                            [ attribute "data-variant" "default" ]
                       )
                    ++ config.attrs
                )
                (config.children
                    ++ (case config.kind of
                            Action ->
                                []

                            Checkbox checked ->
                                [ indicator (slotName config.kind ++ "-indicator") checked ]

                            Radio checked ->
                                [ indicator (slotName config.kind ++ "-indicator") checked ]
                       )
                )
            , index + 1
            )

        Submenu config ->
            let
                subId =
                    prefix ++ "-sub-" ++ String.fromInt index

                subAnchor =
                    "--" ++ subId
            in
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-sub", class "contents" ]
                [ Html.button
                    [ type_ "button"
                    , attribute "role" "menuitem"
                    , attribute "aria-haspopup" "menu"
                    , attribute "popovertarget" subId
                    , attribute "data-slot" "dropdown-menu-sub-trigger"
                    , attribute "style" ("anchor-name: " ++ subAnchor)
                    , Html.Attributes.disabled config.disabled
                    , autofocus isFirst
                    , class itemClass

                    -- Highlighted while its submenu, the next sibling, is open.
                    , class "pe-1.5 has-[+:popover-open]:bg-foreground/10"
                    , insetClass config.inset
                    ]
                    (config.children
                        ++ [ Html.span
                                [ attribute "aria-hidden" "true"
                                , class "pointer-events-none ms-auto flex size-4 items-center justify-center rtl:-scale-x-100"
                                ]
                                [ Html.span [ class "me-1 size-2 rotate-45 border-t-[1.5px] border-r-[1.5px] border-current" ] [] ]
                           ]
                    )
                , Html.div
                    [ Html.Attributes.id subId
                    , attribute "popover" "auto"
                    , attribute "role" "menu"
                    , attribute "focusgroup" "menu block wrap"
                    , attribute "data-slot" "dropdown-menu-sub-content"
                    , attribute "data-side" "right"
                    , attribute "style" ("position-anchor: " ++ subAnchor ++ "; " ++ submenuPlacement ++ "; " ++ minHeight config.items)
                    , class "min-w-24 shadow-lg [position-area:right_span-bottom] rtl:[position-area:left_span-bottom]"
                    , class (menuClass Right)
                    ]
                    (viewItems rootId subId config.items)
                ]
            , index + 1
            )

        Label isInset children ->
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-label"
                , class "py-1 pe-1.5 text-xs font-medium text-muted-foreground"
                , insetClass isInset
                ]
                children
            , index
            )

        Separator ->
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-separator"
                , attribute "role" "separator"
                , class "pointer-events-none -mx-1 my-1 h-px bg-border"
                ]
                []
            , index
            )

        Group items ->
            let
                ( htmls, next ) =
                    viewList rootId prefix first index items
            in
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-group"
                , attribute "role" "group"
                ]
                htmls
            , next
            )


{-| Hovering highlights an item, and focus does too, except while another item is hovered. Focus follows the pointer in a real menu; here they are two highlights that must not both show.
-}
itemClass : String
itemClass =
    "relative flex w-full cursor-default items-center gap-1.5 rounded-md py-1 text-start text-sm outline-hidden select-none hover:bg-foreground/10 focus:bg-foreground/10 group-hover/menu:not-hover:focus:bg-transparent disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg]:shrink-0 [&_svg:not([class*='size-'])]:size-4"


indicator : String -> Bool -> Html msg
indicator slot checked =
    Html.span
        [ attribute "data-slot" slot
        , attribute "aria-hidden" "true"
        , class "pointer-events-none absolute end-2 flex size-4 items-center justify-center"
        , class
            (if checked then
                "visible"

             else
                "invisible"
            )
        ]
        [ Html.span [ class "mb-0.5 h-2.5 w-1.5 rotate-45 border-r-[1.5px] border-b-[1.5px] border-current" ] [] ]


roleName : Kind -> String
roleName kind =
    case kind of
        Action ->
            "menuitem"

        Checkbox _ ->
            "menuitemcheckbox"

        Radio _ ->
            "menuitemradio"


slotName : Kind -> String
slotName kind =
    case kind of
        Action ->
            "dropdown-menu-item"

        Checkbox _ ->
            "dropdown-menu-checkbox-item"

        Radio _ ->
            "dropdown-menu-radio-item"


boolean : Bool -> String
boolean b =
    if b then
        "true"

    else
        "false"



-- HEIGHT
--
-- A scrolling popover never overflows the side it is placed on: it shrinks to the
-- room there instead, so the browser never tries the other side. A minimum height
-- makes a sliver of room count as not fitting, so the menu flips to the side with
-- room for it. The height comes from the default heights of the parts, in rem so it
-- follows the root font size, capped at 10rem like a list that scrolls.


{-| A CSS length as `calc(<rem>rem + <px>px)`.
-}
type alias Length =
    { rem : Float, px : Float }


add : Length -> Length -> Length
add a b =
    { rem = a.rem + b.rem, px = a.px + b.px }


css : Length -> String
css l =
    "calc(" ++ String.fromFloat l.rem ++ "rem + " ++ String.fromFloat l.px ++ "px)"


menuPadding : Length
menuPadding =
    { rem = 0.5, px = 0 }


labelHeight : Length
labelHeight =
    { rem = 1.5, px = 0 }


itemHeight : Length
itemHeight =
    { rem = 1.75, px = 0 }


separatorHeight : Length
separatorHeight =
    { rem = 0.5, px = 1 }


minHeight : List (Item msg) -> String
minHeight items =
    "min-height: min(" ++ css (List.foldl (\entry total -> add total (height entry)) menuPadding items) ++ ", 10rem)"


height : Item msg -> Length
height entry =
    case entry of
        Item _ ->
            itemHeight

        Submenu _ ->
            itemHeight

        Label _ _ ->
            labelHeight

        Separator ->
            separatorHeight

        Group items ->
            List.foldl (\e total -> add total (height e)) { rem = 0, px = 0 } items



-- PLACEMENT
--
-- The menu is placed with CSS anchor positioning. `position-area` picks the side
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
    in
    "position-area: " ++ area ++ "; position-try-fallbacks: " ++ fallbacks ++ "; margin: " ++ margin


{-| The fallbacks and margin of a submenu, which opens at the end of its trigger in the text direction (the `position-area` is a class, because it swaps in right-to-left text). It is flush with the trigger, with its first item 3px above the trigger's top so the two line up, like shadcn's `alignOffset` of -3.
-}
submenuPlacement : String
submenuPlacement =
    "position-try-fallbacks: flip-inline, flip-block, flip-inline flip-block; margin: -3px 0 0 0"


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


alignName : Align -> String
alignName a =
    case a of
        Start ->
            "start"

        Center ->
            "center"

        End ->
            "end"
