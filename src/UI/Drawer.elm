module UI.Drawer exposing
    ( Drawer, Ready, NeedsId, NeedsOpen
    , new
    , id, open
    , title, description, content, footer
    , down, up, left, right, downOnMobile
    , swipeHandle, snapPoints, nonModal, height, width
    , toHtml
    )

{-| A panel that slides in from an edge of the screen.

A drawer is built with `new`, given an `id` and its `open` state, filled in with parts, and rendered with `toHtml`:

    Drawer.new
        |> Drawer.id "goal"
        |> Drawer.open model.editingGoal (SetEditingGoal False)
        |> Drawer.title [ text "Move Goal" ]
        |> Drawer.description [ text "Set your daily activity goal." ]
        |> Drawer.content [ div [ class "flex-1 p-4" ] [ text "…" ] ]
        |> Drawer.footer []
            [ Button.new
                |> Button.text "Submit"
                |> Button.toHtml [ onClick Submit ]
            , Button.new
                |> Button.text "Cancel"
                |> Button.outline
                |> Button.toHtml [ onClick (SetEditingGoal False) ]
            ]
        |> Drawer.toHtml []

The open state lives in your model. Open the drawer from any button of your own:

    Button.new
        |> Button.text "Open Drawer"
        |> Button.secondary
        |> Button.toHtml [ onClick (SetEditingGoal True) ]

The drawer is a native `<dialog>` inside `<elmcn-dialog>`, the same small custom element as `Dialog`'s, which opens it when `open` is true, and closes it for real once it has slid out. Import the element once from your JavaScript entry point:

    import "./UI/elmcn-dialog";

The drawer opens in the top layer over a dimmed, blurred backdrop: it traps focus, makes the rest of the page inert and stops it scrolling. Escape, a tap on the backdrop and a [swipe](#swiping) that closes it send the message you gave [`open`](#open), and the drawer slides out when your model says so. It slides in from the bottom unless given a [side](#sides). Drawers nest: a drawer opened from inside another one stacks in front, and the ones behind shrink and peek out from the edge.

@docs Drawer, Ready, NeedsId, NeedsOpen


# Constructor

@docs new


# Required

`toHtml` won't accept a drawer until it has an `id` and its `open` state.

@docs id, open


# Parts

Every part is optional. The header appears when the drawer has a `title` or a `description`. The parts stack in a flex column, so give the content `class "flex-1 p-4"` to fill the space between the header and the footer, and `overflow-y-auto` as well to scroll it.

@docs title, description, content, footer


# Sides

The last one wins. A vertical drawer sizes itself to its content, capped at `calc(100dvh - 6rem)`. A side drawer spans three quarters of the viewport, or `24rem` from `sm` up.

@docs down, up, left, right, downOnMobile


# Setters

@docs swipeHandle, snapPoints, nonModal, height, width


# Swiping

A swipe is a scroll gesture, handled by the browser rather than by Elm. The drawer sits in a scroller that is hidden but snaps to two places, closed and open, so a finger or a trackpad drags it with momentum and rubber band, off the main thread, and the backdrop fades in step with it through a CSS view timeline. Let go a little over halfway out, or flick it toward its edge, and the drawer asks to close with the message you gave [`open`](#open), like Escape does; let go sooner and it settles back, or at the nearest of the [`snapPoints`](#snapPoints). Every drawer can be swiped, and the [`swipeHandle`](#swipeHandle) only hints at it.

A mouse can't drag the drawer, since a mouse doesn't scroll by dragging: the wheel and a trackpad work, and touch is the primary input. Browsers that don't support `scrollend` or view timelines keep a drawer that opens and closes, without the swipe dismissal or the synced backdrop.


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)
import Html.Events
import Json.Decode as Decode exposing (Decoder)


{-| A drawer being built.

`id` is [`Ready`](#Ready) once the drawer has an [`id`](#id), and `state` once it has its [`open`](#open) state.

-}
type Drawer id state msg
    = Drawer
        { id : String
        , open : Bool
        , onClose : Maybe msg
        , title : List (Html msg)
        , description : List (Html msg)
        , content : List (Html msg)
        , footer : Maybe ( List (Attribute msg), List (Html msg) )
        , side : Side
        , downOnMobile : Bool
        , handle : Bool
        , snapPoints : List Float
        , modal : Bool
        , height : Maybe String
        , width : Maybe String
        }


type Side
    = Down
    | Up
    | Left
    | Right


{-| The drawer can be rendered.
-}
type Ready
    = Ready


{-| A drawer that still needs an [`id`](#id) before it can be rendered.
-}
type NeedsId
    = NeedsId


{-| A drawer that still needs its [`open`](#open) state before it can be rendered.
-}
type NeedsOpen
    = NeedsOpen



-- CONSTRUCTOR


{-| An empty drawer sliding in from the bottom. Give it an [`id`](#id) and its [`open`](#open) state to make it renderable.
-}
new : Drawer NeedsId NeedsOpen msg
new =
    Drawer
        { id = ""
        , open = False
        , onClose = Nothing
        , title = []
        , description = []
        , content = []
        , footer = Nothing
        , side = Down
        , downOnMobile = False
        , handle = False
        , snapPoints = []
        , modal = True
        , height = Nothing
        , width = Nothing
        }



-- REQUIRED


{-| The `id` of the `<elmcn-dialog>`. It must be unique on the page: the title and description are named after it.
-}
id : String -> Drawer NeedsId state msg -> Drawer Ready state msg
id string (Drawer config) =
    Drawer { config | id = string }


{-| Whether the drawer is open, and the message it sends when it asks to close: on Escape, a tap on the backdrop, or a swipe that ends closed. The drawer slides in and out whenever the value changes:

    Drawer.open model.editingGoal (SetEditingGoal False)

-}
open : Bool -> msg -> Drawer id NeedsOpen msg -> Drawer id Ready msg
open isOpen onClose (Drawer config) =
    Drawer { config | open = isOpen, onClose = Just onClose }



-- PARTS


{-| The heading of the drawer, an `<h2>` the drawer is labelled by.
-}
title : List (Html msg) -> Drawer id state msg -> Drawer id state msg
title children (Drawer config) =
    Drawer { config | title = children }


{-| Supporting text below the title, a `<p>` the drawer is described by.
-}
description : List (Html msg) -> Drawer id state msg -> Drawer id state msg
description children (Drawer config) =
    Drawer { config | description = children }


{-| The body of the drawer, between the header and the footer. The children go straight into the drawer's flex column without padding, so a single `div [ class "flex-1 p-4" ]` is the usual shape.

A scrollable child (`overflow-y-auto`) chains to the drawer once it is scrolled to its start, so pulling it down further swipes the drawer away, like on a phone.

-}
content : List (Html msg) -> Drawer id state msg -> Drawer id state msg
content children (Drawer config) =
    Drawer { config | content = children }


{-| The bottom of the drawer, with its buttons stacked and pushed to the end. The attributes come after the styles so they can override them.
-}
footer : List (Attribute msg) -> List (Html msg) -> Drawer id state msg -> Drawer id state msg
footer attrs children (Drawer config) =
    Drawer { config | footer = Just ( attrs, children ) }



-- SIDES


{-| Slides up from the bottom. The default.
-}
down : Drawer id state msg -> Drawer id state msg
down (Drawer config) =
    Drawer { config | side = Down }


{-| Slides down from the top.
-}
up : Drawer id state msg -> Drawer id state msg
up (Drawer config) =
    Drawer { config | side = Up }


{-| Slides in from the left.
-}
left : Drawer id state msg -> Drawer id state msg
left (Drawer config) =
    Drawer { config | side = Left }


{-| Slides in from the right.
-}
right : Drawer id state msg -> Drawer id state msg
right (Drawer config) =
    Drawer { config | side = Right }


{-| Below the `md` breakpoint, slides up from the bottom with a swipe handle instead of using the side it was given. This is how shadcn's demos switch between a side drawer on desktop and a bottom sheet on phones.
-}
downOnMobile : Drawer id state msg -> Drawer id state msg
downOnMobile (Drawer config) =
    Drawer { config | downOnMobile = True }



-- SETTERS


{-| Shows a swipe handle along the edge the drawer slid in from, a bar that hints the drawer can be dragged. It is purely visual: every drawer can be [swiped](#swiping), with or without it.
-}
swipeHandle : Drawer id state msg -> Drawer id state msg
swipeHandle (Drawer config) =
    Drawer { config | handle = True }


{-| Heights a vertical drawer settles at when a swipe ends, instead of sizing itself to its content. A number up to `1` is a fraction of the viewport height, and a bigger one is in pixels. The drawer opens at the first one, and is as tall as the viewport behind its visible part.

    Drawer.snapPoints [ 496, 1 ]

A swipe from the smallest point toward the edge still closes the drawer. Side drawers ignore snap points.

-}
snapPoints : List Float -> Drawer id state msg -> Drawer id state msg
snapPoints points (Drawer config) =
    Drawer { config | snapPoints = points }


{-| Opens the drawer without a backdrop, leaving the rest of the page usable and scrollable. A tap outside doesn't close it: Escape, a swipe and your own buttons still do.
-}
nonModal : Drawer id state msg -> Drawer id state msg
nonModal (Drawer config) =
    Drawer { config | modal = False }


{-| A CSS height for a vertical drawer, such as `"31rem"` or `"100dvh"`, instead of the height of its content. It lifts the default cap, and changing it animates, so a drawer can snap between heights:

    Drawer.height
        (if expanded then
            "100dvh"

         else
            "31rem"
        )

-}
height : String -> Drawer id state msg -> Drawer id state msg
height value (Drawer config) =
    Drawer { config | height = Just value }


{-| A CSS width for a side drawer, such as `"30rem"`, instead of the default.
-}
width : String -> Drawer id state msg -> Drawer id state msg
width value (Drawer config) =
    Drawer { config | width = Just value }


snapPointCss : Float -> String
snapPointCss point =
    if point <= 1 then
        String.fromFloat (point * 100) ++ "dvh"

    else
        String.fromFloat point ++ "px"


{-| Only a scroll that ended at the closed position of a drawer that is still meant to be open counts.

The drawer scrolls to 0 when it is closed, and a drawer that closes any other way, or is not open yet, rests there too. The scroll offset is negative in the directions that scroll backwards, and one axis is always 0.

-}
dismissDecoder : msg -> Decoder msg
dismissDecoder msg =
    Decode.map3
        (\scrollTop scrollLeft _ ->
            if abs scrollTop <= 1 && abs scrollLeft <= 1 then
                Decode.succeed msg

            else
                Decode.fail "not swiped away"
        )
        (Decode.at [ "currentTarget", "scrollTop" ] Decode.float)
        (Decode.at [ "currentTarget", "scrollLeft" ] Decode.float)
        (Decode.at [ "currentTarget", "parentNode", "parentNode", "attributes", "open" ] Decode.value)
        |> Decode.andThen identity


{-| Escape asks the drawer to close, caught on `keydown` before it becomes a `cancel` that would close the dialog without its exit transition, and stopped at the topmost drawer. The back gesture on Android still goes through `cancel`, and when the browser closes the dialog on its own, `close` is reported while `<elmcn-dialog>` is still `open`. See `UI.Dialog` for the details.
-}
closeRequests : msg -> List (Attribute msg)
closeRequests msg =
    [ Html.Events.custom "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Escape" then
                        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    else
                        Decode.fail key
                )
        )
    , Html.Events.preventDefaultOn "cancel" (Decode.succeed ( msg, True ))
    , Html.Events.on "close"
        (Decode.at [ "target", "parentElement", "attributes", "open" ] Decode.value
            |> Decode.map (\_ -> msg)
        )
    ]



-- RENDERING


{-| Renders the `<dialog>`. The attributes go on the panel inside it, after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> Drawer Ready Ready msg -> Html msg
toHtml attrs (Drawer config) =
    let
        titleId =
            config.id ++ "-title"

        descriptionId =
            config.id ++ "-description"

        hasHeader =
            not (List.isEmpty config.title && List.isEmpty config.description)

        onClose =
            List.filterMap identity [ config.onClose ]

        layout =
            if config.downOnMobile then
                case config.side of
                    Down ->
                        FixedSide Down

                    side ->
                        DownBelowMd side

            else
                FixedSide config.side

        -- Side drawers have no use for snap points.
        points =
            case layout of
                FixedSide Left ->
                    []

                FixedSide Right ->
                    []

                _ ->
                    config.snapPoints

        sizes =
            List.filterMap identity
                [ case points of
                    [] ->
                        Maybe.map (\value -> "--drawer-height: " ++ value ++ "; --drawer-max-height: none") config.height

                    _ ->
                        Just "--drawer-height: 100dvh; --drawer-max-height: none"
                , Maybe.map (\value -> "--drawer-width: " ++ value) config.width
                ]

        handle =
            if config.handle then
                Just (viewHandle layout [])

            else if config.downOnMobile then
                Just (viewHandle layout [ class "md:hidden" ])

            else
                Nothing
    in
    Html.node "elmcn-dialog"
        ([ Html.Attributes.id config.id
         , attribute "data-slot" "drawer"
         , class "contents"
         ]
            ++ (if config.open then
                    [ attribute "open" "" ]

                else
                    []
               )
        )
        [ -- `overflow-clip`, not `overflow-hidden`: the scroller inside does the
          -- scrolling, and the dialog must stay put while it focuses. It shares the
          -- panel's view timeline between the scroller and the backdrop through
          -- `timeline-scope`.
          Html.node "dialog"
            (class "fixed inset-0 z-50 m-0 hidden h-full max-h-none w-full max-w-none overflow-clip bg-transparent p-0 text-inherit outline-none backdrop:bg-transparent open:block [timeline-scope:--drawer]"
                :: (if config.modal then
                        -- The page behind stops scrolling while the drawer is modal,
                        -- keeping its scrollbar gutter so that nothing shifts.
                        [ class "[html:has(&:modal)]:overflow-hidden [html:has(&:modal)]:[scrollbar-gutter:stable]" ]

                    else
                        -- A non-modal drawer is a manual popover rather than a modal
                        -- dialog: in the top layer, but with the page behind it still
                        -- usable. The dialog lets clicks through to the page.
                        [ attribute "popover" "manual"
                        , class "pointer-events-none"
                        ]
                   )
                ++ (if List.isEmpty config.title then
                        []

                    else
                        [ attribute "aria-labelledby" titleId ]
                   )
                ++ (if List.isEmpty config.description then
                        []

                    else
                        [ attribute "aria-describedby" descriptionId ]
                   )
                ++ List.concatMap closeRequests onClose
            )
            [ -- The backdrop is the dialog's own child, so `<elmcn-dialog>` waits for its
              -- fade out before it closes the dialog. It fades in as the panel's edge
              -- comes into view, through the view timeline, and out on a clock while
              -- the panel slides out. The fade out is a filter, multiplied with the
              -- opacity the timeline gives it: an animation moving from the timeline
              -- to a clock keeps its progress, and a separate fade out would flash a
              -- drawer that was swiped away back in. A non-modal drawer's backdrop is
              -- transparent, and only there for its transition. In Firefox, which
              -- scrolls the drawer in at once, the filter fades the backdrop in too.
              Html.div
                ([ attribute "data-slot" "drawer-backdrop"
                 , attribute "aria-hidden" "true"
                 , class "pointer-events-none absolute inset-0 [filter:opacity(1)] transition-[filter] duration-450 ease-linear [elmcn-dialog:not([open])>dialog>&]:[filter:opacity(0)] supports-[-moz-appearance:none]:starting:[elmcn-dialog[open]>dialog>&]:[filter:opacity(0)]"
                 ]
                    ++ (if config.modal then
                            class "bg-black/10 opacity-0 backdrop-blur-xs [animation-fill-mode:both] [animation-name:drawer-fade] [animation-range:entry] [animation-timeline:--drawer] [animation-timing-function:linear]"
                                :: backdropRange points

                        else
                            []
                       )
                )
                []

            -- The scroller is the drawer's whole gesture: it holds the closed and
            -- open positions as snap points, its scrollbar is hidden, and its
            -- scrolling is smooth, so the entrance is a scroll too.
            , Html.div
                ([ attribute "data-slot" "drawer-scroller"
                 , class "absolute inset-0 scroll-smooth [scrollbar-width:none] overscroll-contain motion-reduce:scroll-auto [&::-webkit-scrollbar]:hidden"
                 , class (scrollerClass layout)

                 -- No snapping on the way out. Browsers place snap points where the
                 -- markers are drawn, so while the panel slides out they would scroll
                 -- after the moving markers, and the panel would come back up.
                 , class "[elmcn-dialog:not([open])>dialog>&]:[scroll-snap-type:none]"
                 ]
                    -- A swipe ends in a scroll that ends: `scroll` also fires while
                    -- the drawer opens and closes.
                    ++ List.map (Html.Events.on "scrollend" << dismissDecoder) onClose
                    ++ (if config.modal then
                            []

                        else
                            -- Safari on iOS won't scroll a scroller that ignores pointer
                            -- events, even from the panel inside it. Where scroll timelines
                            -- exist, the scroller takes them instead, clipped to the part of
                            -- the screen its panel covers, which leaves the page around it
                            -- usable.
                            [ class "pointer-events-none supports-[animation-timeline:scroll()]:pointer-events-auto supports-[animation-timeline:scroll()]:[animation-name:drawer-reveal] supports-[animation-timeline:scroll()]:[animation-fill-mode:both] supports-[animation-timeline:scroll()]:[animation-timing-function:linear]"
                            , class (revealClass layout)
                            ]
                       )
                )
                [ Html.div
                    [ class "pointer-events-none relative flex"
                    , class (contentClass layout)

                    -- Unseen at the end of the slide-out, when the exit area and the
                    -- bleed collapse and the panel would jump. Through `opacity`, not
                    -- `visibility`, which a button inside that transitions `all`
                    -- would keep for its own transition.
                    , class "transition-opacity duration-0 [elmcn-dialog:not([open])>dialog>*>&]:opacity-0 [elmcn-dialog:not([open])>dialog>*>&]:delay-400"
                    ]
                    -- The exit area and the bleed only exist while the drawer is open.
                    -- Browsers keep a scroller's offset through `display: none`, so a
                    -- drawer closed by a button would reopen already scrolled in, with
                    -- nothing left to animate. They collapse near the end of the
                    -- slide-out, while the dialog is still shown, so that the scroller
                    -- has no room left and its offset clamps back to zero. Only on the
                    -- way out: a drawer opened again comes back with them at once. The
                    -- exit area is the part of the backdrop above the panel, so a tap
                    -- on it asks to close a modal drawer, and a drag on it swipes it.
                    [ Html.div
                        ([ attribute "data-slot" "drawer-exit"
                         , attribute "aria-hidden" "true"
                         , class "shrink-0 [:not(:is([open],:popover-open))>*>*>&]:hidden"
                         , class "max-h-[200dvh] max-w-[200dvw] transition-[max-width,max-height] duration-0 [elmcn-dialog:not([open])>dialog>*>*>&]:max-h-0 [elmcn-dialog:not([open])>dialog>*>*>&]:max-w-0 [elmcn-dialog:not([open])>dialog>*>*>&]:delay-400"
                         , class (exitClass layout)
                         ]
                            ++ (if config.modal then
                                    class "pointer-events-auto" :: List.map Html.Events.onClick onClose

                                else
                                    []
                               )
                        )
                        []
                    , Html.div
                        ([ attribute "data-slot" "drawer-stage"
                         , class "relative shrink-0 outline-none"
                         , class (stageClass (not (List.isEmpty points)) layout)

                         -- Clipped on the way out, so that the sliding panel doesn't
                         -- count toward what scrolls, and the scroller loses all its room
                         -- once the exit area and the bleed collapse. Its edges are at
                         -- or past the screen's where the panel leaves.
                         , class "[elmcn-dialog:not([open])>dialog>*>*>&]:overflow-clip"
                         ]
                            ++ (if List.isEmpty points then
                                    entrance

                                else
                                    []
                               )
                        )
                        (List.indexedMap (viewSnapPoint layout) points
                            ++ [ Html.div
                                    ([ attribute "data-slot" "drawer-popup"
                                     , class "group/drawer-popup pointer-events-auto relative flex min-h-0 bg-popover text-sm text-popover-foreground outline-none select-none will-change-transform [interpolate-size:allow-keywords]"

                                     -- Slides in while its own `<elmcn-dialog>` is `open`, and out
                                     -- once it isn't. Shrinks and peeks out behind the drawers
                                     -- nested in it, one step per level.
                                     , class "transition-[translate,scale,filter,height] duration-450 ease-[cubic-bezier(0.22,1,0.36,1)] [&:has(elmcn-dialog[open])]:overflow-hidden [&:has(elmcn-dialog[open])]:brightness-95 [&:has(elmcn-dialog[open])]:scale-95 [&:has(elmcn-dialog[open]_elmcn-dialog[open])]:scale-90 [&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:scale-[0.85]"
                                     , class (panelClass layout)
                                     ]
                                        ++ (if List.isEmpty sizes then
                                                []

                                            else
                                                [ attribute "style" (String.join "; " sizes) ]
                                           )
                                        ++ attrs
                                    )
                                    (List.concat
                                        [ List.filterMap identity [ handle ]
                                        , [ Html.div
                                                [ attribute "data-slot" "drawer-content"
                                                , class "flex min-h-0 flex-1 flex-col overflow-hidden overscroll-auto rounded-[inherit] transition-opacity duration-300 ease-[cubic-bezier(0.45,1.005,0,1.005)] select-text [&:has(elmcn-dialog[open])]:opacity-0"
                                                ]
                                                (List.filterMap identity
                                                    [ if hasHeader then
                                                        Just
                                                            (Html.div
                                                                [ attribute "data-slot" "drawer-header"
                                                                , class "flex shrink-0 flex-col gap-0.5 p-4 pb-0 md:gap-0.5 md:text-left"
                                                                , class (headerClass layout)
                                                                ]
                                                                (List.filterMap identity
                                                                    [ if List.isEmpty config.title then
                                                                        Nothing

                                                                      else
                                                                        Just
                                                                            (Html.h2
                                                                                [ Html.Attributes.id titleId
                                                                                , attribute "data-slot" "drawer-title"
                                                                                , class "text-base font-medium text-foreground"
                                                                                ]
                                                                                config.title
                                                                            )
                                                                    , if List.isEmpty config.description then
                                                                        Nothing

                                                                      else
                                                                        Just
                                                                            (Html.p
                                                                                [ Html.Attributes.id descriptionId
                                                                                , attribute "data-slot" "drawer-description"
                                                                                , class "text-sm text-balance text-muted-foreground"
                                                                                ]
                                                                                config.description
                                                                            )
                                                                    ]
                                                                )
                                                            )

                                                      else
                                                        Nothing
                                                    ]
                                                    ++ config.content
                                                    ++ List.filterMap identity
                                                        [ Maybe.map viewFooter config.footer ]
                                                )
                                          ]
                                        ]
                                    )
                               ]
                        )
                    , Html.div [ class "relative shrink-0 bg-popover [:not(:is([open],:popover-open))>*>*>&]:hidden", class "max-h-[200dvh] max-w-[200dvw] transition-[max-width,max-height] duration-0 [elmcn-dialog:not([open])>dialog>*>*>&]:max-h-0 [elmcn-dialog:not([open])>dialog>*>*>&]:max-w-0 [elmcn-dialog:not([open])>dialog>*>*>&]:delay-400", class (bleedClass layout) ] []
                    ]
                ]
            ]
        ]


viewFooter : ( List (Attribute msg), List (Html msg) ) -> Html msg
viewFooter ( attrs, children ) =
    Html.div
        ([ attribute "data-slot" "drawer-footer"
         , class "mt-auto flex shrink-0 flex-col gap-2 p-4 pt-0"
         ]
            ++ attrs
        )
        children


viewHandle : Layout -> List (Attribute msg) -> Html msg
viewHandle layout attrs =
    Html.div
        ([ attribute "data-slot" "drawer-swipe-handle"
         , attribute "aria-hidden" "true"
         , class "relative z-10 flex shrink-0 transition-opacity duration-200 after:block after:shrink-0 after:rounded-full after:bg-muted [:has(elmcn-dialog[open])>&]:opacity-0"
         , class (handleClass layout)
         ]
            ++ attrs
        )
        []


{-| A snap point is a marker in the stage, `point` in from the panel's leading edge. It snaps to the
leading edge of the scroller by way of a scroll margin of one viewport, which leaves exactly `point`
of the panel in view. The margin also makes the marker's snap area as tall as the viewport, so that
the dialog focusing the first marker on opening scrolls it to the same place, whichever way the
browser aligns what it scrolls into view. The marker is two pixels tall, since Chrome ignores a
smaller one.

The markers sit beside the panel rather than in it, because browsers place snap points and scroll
focused elements into view where they are drawn: in Firefox, the panel is still translated off
screen by `@starting-style` when the dialog focuses the first marker, which scrolled the drawer a
whole panel too far, to its tallest point, before it snapped back.

-}
viewSnapPoint : Layout -> Int -> Float -> Html msg
viewSnapPoint layout index point =
    Html.div
        ([ attribute "data-slot" "drawer-snap-point"
         , class "pointer-events-none absolute left-0 h-0.5 w-0.5 snap-start outline-none"
         , case layout of
            FixedSide Up ->
                class "scroll-mb-[100dvh]"

            _ ->
                class "scroll-mt-[100dvh]"
         , attribute "style"
            ((case layout of
                FixedSide Up ->
                    "bottom: "

                _ ->
                    "top: "
             )
                ++ snapPointCss point
            )
         ]
            ++ (case layout of
                    DownBelowMd _ ->
                        [ class "md:hidden" ]

                    FixedSide _ ->
                        []
               )
            ++ (if index == 0 then
                    entrance

                else
                    []
               )
        )
        []



-- LAYOUT


{-| A fixed side, or a side from `md` up with the bottom below it. Tailwind only finds
classes written out in full, so every combination is spelled out below.

The panel is in while its own `<elmcn-dialog>` is `open`. `:where` keeps that selector as specific
as a class and an attribute, so that the steps back behind nested drawers, which add a `:has`, win
over it.

Firefox, the browser with `-moz-appearance`, ignores `scroll-behavior` when it scrolls what it
focuses into view, so the entrance there is a jump. It slides the panel in from `@starting-style`
instead.

Every layout is the same scroller seen from a different edge. It scrolls along the axis the drawer
slides on, and a swipe is a scroll toward the origin, where the closed drawer rests. The origin is
the top for `Down`, the bottom for `Up`, the left for `Right` and the right for `Left`, which is why
the last two flip the direction, and the second one the writing mode.

-}
type Layout
    = FixedSide Side
    | DownBelowMd Side


panelClass : Layout -> String
panelClass layout =
    case layout of
        FixedSide Down ->
            "flex-col rounded-t-xl border-t origin-bottom h-(--drawer-height) max-h-(--drawer-max-height) [--drawer-height:auto] [--drawer-max-height:calc(100dvh-6rem)] translate-y-[calc(100%+2px)] [:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-y-0 supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-y-[calc(100%+2px)] [&:has(elmcn-dialog[open])]:-translate-y-[calc(1rem+5%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open])]:-translate-y-[calc(2rem+10%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:-translate-y-[calc(3rem+15%)] [view-timeline:--drawer_y]"

        FixedSide Up ->
            "flex-col rounded-b-xl border-b origin-top h-(--drawer-height) max-h-(--drawer-max-height) [--drawer-height:auto] [--drawer-max-height:calc(100dvh-6rem)] -translate-y-[calc(100%+2px)] [:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-y-0 supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:-translate-y-[calc(100%+2px)] [&:has(elmcn-dialog[open])]:translate-y-[calc(1rem+5%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open])]:translate-y-[calc(2rem+10%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:translate-y-[calc(3rem+15%)] [view-timeline:--drawer_y]"

        FixedSide Left ->
            "h-full flex-row rounded-r-xl border-r origin-left w-(--drawer-width) [--drawer-width:75dvw] sm:[--drawer-width:24rem] -translate-x-[calc(100%+2px)] [:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-x-0 supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:-translate-x-[calc(100%+2px)] [&:has(elmcn-dialog[open])]:translate-x-[calc(1rem+5%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open])]:translate-x-[calc(2rem+10%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:translate-x-[calc(3rem+15%)] [view-timeline:--drawer_x]"

        FixedSide Right ->
            "h-full flex-row rounded-l-xl border-l origin-right w-(--drawer-width) [--drawer-width:75dvw] sm:[--drawer-width:24rem] translate-x-[calc(100%+2px)] [:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-x-0 supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-x-[calc(100%+2px)] [&:has(elmcn-dialog[open])]:-translate-x-[calc(1rem+5%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open])]:-translate-x-[calc(2rem+10%)] [&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:-translate-x-[calc(3rem+15%)] [view-timeline:--drawer_x]"

        DownBelowMd side ->
            "max-md:flex-col max-md:rounded-t-xl max-md:border-t max-md:origin-bottom max-md:h-(--drawer-height) max-md:max-h-(--drawer-max-height) max-md:[--drawer-height:auto] max-md:[--drawer-max-height:calc(100dvh-6rem)] max-md:translate-y-[calc(100%+2px)] max-md:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-y-0 max-md:supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-y-[calc(100%+2px)] max-md:[&:has(elmcn-dialog[open])]:-translate-y-[calc(1rem+5%)] max-md:[&:has(elmcn-dialog[open]_elmcn-dialog[open])]:-translate-y-[calc(2rem+10%)] max-md:[&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:-translate-y-[calc(3rem+15%)] max-md:[view-timeline:--drawer_y] "
                ++ (case side of
                        Up ->
                            "md:flex-col md:rounded-b-xl md:border-b md:origin-top md:h-(--drawer-height) md:max-h-(--drawer-max-height) md:[--drawer-height:auto] md:[--drawer-max-height:calc(100dvh-6rem)] md:-translate-y-[calc(100%+2px)] md:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-y-0 md:supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:-translate-y-[calc(100%+2px)] md:[&:has(elmcn-dialog[open])]:translate-y-[calc(1rem+5%)] md:[&:has(elmcn-dialog[open]_elmcn-dialog[open])]:translate-y-[calc(2rem+10%)] md:[&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:translate-y-[calc(3rem+15%)] md:[view-timeline:--drawer_y]"

                        Left ->
                            "md:h-full md:flex-row md:rounded-r-xl md:border-r md:origin-left md:w-(--drawer-width) md:[--drawer-width:24rem] md:-translate-x-[calc(100%+2px)] md:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-x-0 md:supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:-translate-x-[calc(100%+2px)] md:[&:has(elmcn-dialog[open])]:translate-x-[calc(1rem+5%)] md:[&:has(elmcn-dialog[open]_elmcn-dialog[open])]:translate-x-[calc(2rem+10%)] md:[&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:translate-x-[calc(3rem+15%)] md:[view-timeline:--drawer_x]"

                        _ ->
                            "md:h-full md:flex-row md:rounded-l-xl md:border-l md:origin-right md:w-(--drawer-width) md:[--drawer-width:24rem] md:translate-x-[calc(100%+2px)] md:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-x-0 md:supports-[-moz-appearance:none]:starting:[:where(elmcn-dialog)[open]>:where(dialog)>*>*>*>&]:translate-x-[calc(100%+2px)] md:[&:has(elmcn-dialog[open])]:-translate-x-[calc(1rem+5%)] md:[&:has(elmcn-dialog[open]_elmcn-dialog[open])]:-translate-x-[calc(2rem+10%)] md:[&:has(elmcn-dialog[open]_elmcn-dialog[open]_elmcn-dialog[open])]:-translate-x-[calc(3rem+15%)] md:[view-timeline:--drawer_x]"
                   )


scrollerClass : Layout -> String
scrollerClass layout =
    case layout of
        FixedSide Down ->
            "overflow-y-auto overflow-x-hidden [scroll-snap-type:y_mandatory]"

        FixedSide Up ->
            "overflow-y-auto overflow-x-hidden [scroll-snap-type:y_mandatory] [writing-mode:vertical-rl] [direction:rtl]"

        FixedSide Left ->
            "overflow-x-auto overflow-y-hidden [scroll-snap-type:x_mandatory] [direction:rtl]"

        FixedSide Right ->
            "overflow-x-auto overflow-y-hidden [scroll-snap-type:x_mandatory]"

        DownBelowMd side ->
            "max-md:overflow-y-auto max-md:overflow-x-hidden max-md:[scroll-snap-type:y_mandatory] "
                ++ (case side of
                        Up ->
                            "md:overflow-y-auto md:overflow-x-hidden md:[scroll-snap-type:y_mandatory] md:[writing-mode:vertical-rl] md:[direction:rtl]"

                        Left ->
                            "md:overflow-x-auto md:overflow-y-hidden md:[scroll-snap-type:x_mandatory] md:[direction:rtl]"

                        _ ->
                            "md:overflow-x-auto md:overflow-y-hidden md:[scroll-snap-type:x_mandatory]"
                   )


contentClass : Layout -> String
contentClass layout =
    case layout of
        FixedSide Down ->
            "flex-col"

        FixedSide Up ->
            "w-full flex-row"

        FixedSide Left ->
            "h-full flex-row"

        FixedSide Right ->
            "h-full flex-row"

        DownBelowMd side ->
            "max-md:flex-col "
                ++ (case side of
                        Up ->
                            "md:w-full md:flex-row"

                        Left ->
                            "md:h-full md:flex-row"

                        _ ->
                            "md:h-full md:flex-row"
                   )


{-| A non-modal drawer's scroller is clipped on the side of its exit area, by as much of the exit
area as is in view: all of it while the drawer is closed, and one pixel less per pixel scrolled.
-}
revealClass : Layout -> String
revealClass layout =
    case layout of
        FixedSide Down ->
            "supports-[animation-timeline:scroll()]:[clip-path:inset(100%_0_0_0)] supports-[animation-timeline:scroll()]:[animation-timeline:scroll(self_y)] supports-[animation-timeline:scroll()]:[animation-range:0px_100dvh]"

        FixedSide Up ->
            "supports-[animation-timeline:scroll()]:[clip-path:inset(0_0_100%_0)] supports-[animation-timeline:scroll()]:[animation-timeline:scroll(self_y)] supports-[animation-timeline:scroll()]:[animation-range:0px_100dvh]"

        FixedSide Left ->
            "supports-[animation-timeline:scroll()]:[clip-path:inset(0_100%_0_0)] supports-[animation-timeline:scroll()]:[animation-timeline:scroll(self_x)] supports-[animation-timeline:scroll()]:[animation-range:0px_100dvw]"

        FixedSide Right ->
            "supports-[animation-timeline:scroll()]:[clip-path:inset(0_0_0_100%)] supports-[animation-timeline:scroll()]:[animation-timeline:scroll(self_x)] supports-[animation-timeline:scroll()]:[animation-range:0px_100dvw]"

        DownBelowMd side ->
            "supports-[animation-timeline:scroll()]:max-md:[clip-path:inset(100%_0_0_0)] supports-[animation-timeline:scroll()]:max-md:[animation-timeline:scroll(self_y)] supports-[animation-timeline:scroll()]:max-md:[animation-range:0px_100dvh] "
                ++ (case side of
                        Up ->
                            "supports-[animation-timeline:scroll()]:md:[clip-path:inset(0_0_100%_0)] supports-[animation-timeline:scroll()]:md:[animation-timeline:scroll(self_y)] supports-[animation-timeline:scroll()]:md:[animation-range:0px_100dvh]"

                        Left ->
                            "supports-[animation-timeline:scroll()]:md:[clip-path:inset(0_100%_0_0)] supports-[animation-timeline:scroll()]:md:[animation-timeline:scroll(self_x)] supports-[animation-timeline:scroll()]:md:[animation-range:0px_100dvw]"

                        _ ->
                            "supports-[animation-timeline:scroll()]:md:[clip-path:inset(0_0_0_100%)] supports-[animation-timeline:scroll()]:md:[animation-timeline:scroll(self_x)] supports-[animation-timeline:scroll()]:md:[animation-range:0px_100dvw]"
                   )


exitClass : Layout -> String
exitClass layout =
    case layout of
        FixedSide Down ->
            "h-[100dvh] snap-start"

        FixedSide Up ->
            "h-[100dvh] [writing-mode:horizontal-tb] [direction:ltr] snap-start"

        FixedSide Left ->
            "w-[100dvw] snap-start"

        FixedSide Right ->
            "w-[100dvw] snap-start"

        DownBelowMd side ->
            "max-md:h-[100dvh] max-md:snap-start "
                ++ (case side of
                        Up ->
                            "md:h-[100dvh] md:[writing-mode:horizontal-tb] md:[direction:ltr] md:snap-start"

                        Left ->
                            "md:w-[100dvw] md:snap-start"

                        _ ->
                            "md:w-[100dvw] md:snap-start"
                   )


stageClass : Bool -> Layout -> String
stageClass snapped layout =
    case layout of
        FixedSide Down ->
            if snapped then
                ""

            else
                "snap-end"

        FixedSide Up ->
            if snapped then
                "[writing-mode:horizontal-tb] [direction:ltr]"

            else
                "[writing-mode:horizontal-tb] [direction:ltr] snap-end"

        FixedSide Left ->
            if snapped then
                "[direction:ltr]"

            else
                "[direction:ltr] snap-end"

        FixedSide Right ->
            if snapped then
                ""

            else
                "snap-end"

        DownBelowMd side ->
            (if snapped then
                " "

             else
                "max-md:snap-end "
            )
                ++ (case side of
                        Up ->
                            if snapped then
                                "md:[writing-mode:horizontal-tb] md:[direction:ltr]"

                            else
                                "md:[writing-mode:horizontal-tb] md:[direction:ltr] md:snap-end"

                        Left ->
                            if snapped then
                                "md:[direction:ltr]"

                            else
                                "md:[direction:ltr] md:snap-end"

                        _ ->
                            if snapped then
                                ""

                            else
                                "md:snap-end"
                   )


bleedClass : Layout -> String
bleedClass layout =
    case layout of
        FixedSide Down ->
            "h-[100dvh]"

        FixedSide Up ->
            "h-[100dvh] [writing-mode:horizontal-tb] [direction:ltr]"

        FixedSide Left ->
            "w-[100dvw]"

        FixedSide Right ->
            "w-[100dvw]"

        DownBelowMd side ->
            "max-md:h-[100dvh] "
                ++ (case side of
                        Up ->
                            "md:h-[100dvh] md:[writing-mode:horizontal-tb] md:[direction:ltr]"

                        Left ->
                            "md:w-[100dvw]"

                        _ ->
                            "md:w-[100dvw]"
                   )


handleClass : Layout -> String
handleClass layout =
    case layout of
        FixedSide Down ->
            "h-3 w-full items-end justify-center after:h-1 after:w-24"

        FixedSide Up ->
            "order-last h-3 w-full items-start justify-center after:h-1 after:w-24"

        FixedSide Left ->
            "order-last h-full w-3 items-center justify-start after:h-24 after:w-1"

        FixedSide Right ->
            "h-full w-3 items-center justify-end after:h-24 after:w-1"

        DownBelowMd side ->
            "max-md:h-3 max-md:w-full max-md:items-end max-md:justify-center max-md:after:h-1 max-md:after:w-24 "
                ++ (case side of
                        Up ->
                            "md:order-last md:h-3 md:w-full md:items-start md:justify-center md:after:h-1 md:after:w-24"

                        Left ->
                            "md:order-last md:h-full md:w-3 md:items-center md:justify-start md:after:h-24 md:after:w-1"

                        _ ->
                            "md:h-full md:w-3 md:items-center md:justify-end md:after:h-24 md:after:w-1"
                   )


{-| A vertical drawer centres its header on small screens.
-}
headerClass : Layout -> String
headerClass layout =
    case layout of
        FixedSide Down ->
            "text-center"

        FixedSide Up ->
            "text-center"

        FixedSide _ ->
            ""

        DownBelowMd _ ->
            "max-md:text-center"


{-| With snap points, the drawer opens with its first one in view, and the backdrop is at full
strength by then.
-}
backdropRange : List Float -> List (Attribute msg)
backdropRange points =
    case points of
        first :: _ ->
            [ attribute "style" ("animation-range: entry 0% entry " ++ snapPointCss first) ]

        [] ->
            []


{-| The browser scrolls whatever is focused on opening into view, and the scroller smooths that
scroll, which is the whole entrance. The stage is what is brought into view, or the first snap
point inside the panel.
-}
entrance : List (Attribute msg)
entrance =
    [ attribute "autofocus" "", Html.Attributes.tabindex -1 ]
