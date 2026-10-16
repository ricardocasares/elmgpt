module UI.MessageScroller exposing
    ( MessageScroller, Ready, NeedsId
    , new
    , id
    , Item, item, anchor
    , items
    , label, busy, padding, gap
    , peek, scrollMargin
    , holdTurn
    , openAtStart, openAtLastAnchor
    , endButton, startButton
    , onScroll, scrollable
    , toHtml
    , Behavior(..), scrollToEnd, scrollToStart, scrollToMessage
    , Place, markPlace, restorePlace
    )

{-| A chat scroll container that anchors turns, opens saved transcripts, follows streamed responses, loads history without jumping, and jumps to any message.

A scroller is built with `new`, given the `id` of its viewport, filled with rows, and rendered with `toHtml`. It fills its parent, so give the parent a height:

    MessageScroller.new
        |> MessageScroller.id "chat"
        |> MessageScroller.padding "var(--card-spacing)"
        |> MessageScroller.busy model.streaming
        |> MessageScroller.items []
            (List.map viewMessage model.messages)
        |> MessageScroller.holdTurn
        |> MessageScroller.endButton ScrollToEnd
        |> MessageScroller.onScroll Scrolled
        |> MessageScroller.toHtml []

Every row is an [`Item`](#Item): a message, a marker, a typing indicator, anything that belongs in the transcript. A row that starts a new exchange, usually the user's message, is an [`anchor`](#anchor):

    MessageScroller.item message.id [] [ viewMessage message ]
        |> MessageScroller.anchor

The viewport is a focusable `region` named "Messages" and the rows sit in a `role="log"` element that announces additions, so a new row is read out but streamed text is not, word by word. Set [`busy`](#busy) while a reply streams to have it announced when it is done.

There is no state and no JavaScript, only CSS and `Browser.Dom` tasks. The browser does what CSS can:

  - It opens at the end of the transcript, at its start or at its last turn, without a flash of the wrong position.
  - It keeps the reader's place when content above changes size, or when rows are added above, and follows the end of the transcript as it grows while the reader is there. This is scroll anchoring, so the rows are keyed by their `id`.
  - It shows the scroll buttons only when there is something to scroll toward, and fades the bottom edge while there is more to read.
  - It skips rows that are off screen (`content-visibility: auto`), which keeps long transcripts cheap. The last 24 rows are always rendered, so the end of a transcript has its true height, and the scroller opens exactly there, wherever it is mounted.

What it can't do is move the reader, so your app asks for a scroll, with three rules:

1.  After you append a turn, run `scrollToEnd Instant` with [`holdTurn`](#holdTurn) set. The new turn lands `scrollMargin + peek` pixels below the top of the viewport, with room under it for the reply, which then streams in without moving the reader.
2.  While a reply streams, follow the end where the browser doesn't: in the first turn of an empty transcript, where there is no scroll offset to anchor, and in iOS Safari, which has no scroll anchoring. After a chunk, measure the viewport, and if the last [`onScroll`](#onScroll) said that the reader is at the end (`end = False`) and the viewport has fallen short of it, run `scrollToEnd Instant`. Decide when the measurement arrives, not when the chunk does: a `scroll` event comes a frame after the reader has moved, and a scroll that is already on its way would override them.
3.  When the button is pressed, run `scrollToEnd Smooth`.

In an `update` that looks like this:

    update msg model =
        case msg of
            Sent turn ->
                ( { model | messages = model.messages ++ [ turn ], holding = True }
                , scroll MessageScroller.Instant
                )

            Chunk text ->
                ( { model | reply = model.reply ++ text }
                , Task.attempt Measured (Browser.Dom.getViewportOf "chat")
                )

            Measured (Ok { scene, viewport }) ->
                ( model
                , if model.atEnd && scene.height - viewport.y - viewport.height > 4 then
                    scroll MessageScroller.Instant

                  else
                    Cmd.none
                )

            Measured (Err _) ->
                ( model, Cmd.none )

            Scrolled edges ->
                ( { model | atEnd = not edges.end }, Cmd.none )

            ScrollToEnd ->
                ( model, scroll MessageScroller.Smooth )

    scroll behavior =
        Task.attempt (\_ -> NoOp) (MessageScroller.scrollToEnd behavior "chat")

The opening position applies to a freshly rendered scroller. To open a different transcript, wrap the scroller in `Html.Keyed.node` and change the key.

The scroller needs the utilities in `main.css`: the scroll button states, the bottom fade and `scrollbar-thin`. Add the theme again if you installed it before the scroller.

Browsers: Chrome, Firefox and Safari do everything above. iOS Safari has no scroll anchoring, so it needs rule 2, and [`markPlace`](#markPlace) with [`restorePlace`](#restorePlace) when you load history. Firefox has no scroll-driven animations, so its buttons follow [`scrollable`](#scrollable), which you set from `onScroll`.

Not ported from shadcn: the `data-autoscrolling` state, the visibility hook, and a configurable edge threshold (it is 8px). [`scrollToMessage`](#scrollToMessage) aligns a row to the edge of the viewport, where shadcn adds the padding of the transcript.

@docs MessageScroller, Ready, NeedsId


# Constructor

@docs new


# Required

`toHtml` won't accept a scroller until it has an `id`.

@docs id


# Items

@docs Item, item, anchor

@docs items


# Setters

@docs label, busy, padding, gap

The reading line is where a new turn lands: `scrollMargin + peek` pixels below the top of the viewport.

@docs peek, scrollMargin

@docs holdTurn


# Opening position

A scroller opens at the end by default. Last wins.

@docs openAtStart, openAtLastAnchor


# Buttons

A button is only rendered when you give it a message. It is shown while there is something to scroll toward.

@docs endButton, startButton


# Scroll state

@docs onScroll, scrollable


# Rendering

@docs toHtml


# Commands

The commands are tasks that move the viewport with `Browser.Dom`. They find it by its `id`, wait for the next frame so they see what Elm just rendered, and fail with `Browser.Dom.Error` if it isn't there.

@docs Behavior, scrollToEnd, scrollToStart, scrollToMessage


## Loading history

Rows added above the reader are kept in place by the browser. Where it doesn't, measure a row that stays with `markPlace` before you add the rows, and put it back with `restorePlace` in the same `update` that adds them, since the command waits for them to be rendered:

    update msg model =
        case msg of
            LoadHistory ->
                ( model
                , Task.attempt PlaceMarked
                    (MessageScroller.markPlace { viewport = "chat", message = model.firstRow })
                )

            PlaceMarked (Ok place) ->
                ( { model | messages = model.older ++ model.messages }
                , Task.attempt (\_ -> NoOp) (MessageScroller.restorePlace place)
                )

            PlaceMarked (Err _) ->
                ( { model | messages = model.older ++ model.messages }, Cmd.none )

@docs Place, markPlace, restorePlace

-}

import Browser.Dom
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, tabindex)
import Html.Events
import Html.Keyed
import Json.Decode as Decode
import Process
import Task exposing (Task)
import UI.Button as Button


{-| A scroller being built.

`status` is [`Ready`](#Ready) once the scroller has an [`id`](#id).

-}
type MessageScroller status msg
    = MessageScroller
        { id : String
        , label : String
        , busy : Bool
        , padding : Maybe String
        , gap : Maybe String
        , peek : Maybe Int
        , margin : Maybe Int
        , hold : Bool
        , open : Open
        , content : List (Attribute msg)
        , items : List (Item msg)
        , endButton : Maybe msg
        , startButton : Maybe msg
        , onScroll : Maybe ({ start : Bool, end : Bool } -> msg)
        , scrollable : { start : Bool, end : Bool }
        }


{-| The scroller can be rendered.
-}
type Ready
    = Ready


{-| A scroller that still needs an [`id`](#id) before it can be rendered.
-}
type NeedsId
    = NeedsId


{-| A row of the transcript, made with [`item`](#item).
-}
type Item msg
    = Item
        { id : String
        , anchor : Bool
        , tail : Bool
        , attrs : List (Attribute msg)
        , children : List (Html msg)
        }


type Open
    = AtEnd
    | AtStart
    | AtLastAnchor


type Direction
    = ToEnd
    | ToStart


{-| How the viewport gets there: `Instant` jumps, `Smooth` eases over a few hundred milliseconds.
-}
type Behavior
    = Instant
    | Smooth


{-| Where a row was in the viewport. Made by `markPlace`, used by `restorePlace`.
-}
type Place
    = Place
        { viewport : String
        , message : String
        , offset : Float
        }



-- CONSTRUCTOR


{-| An empty scroller. Give it an [`id`](#id) to make it renderable.
-}
new : MessageScroller NeedsId msg
new =
    MessageScroller
        { id = ""
        , label = "Messages"
        , busy = False
        , padding = Nothing
        , gap = Nothing
        , peek = Nothing
        , margin = Nothing
        , hold = False
        , open = AtEnd
        , content = []
        , items = []
        , endButton = Nothing
        , startButton = Nothing
        , onScroll = Nothing
        , scrollable = { start = False, end = False }
        }



-- REQUIRED


{-| The `id` of the viewport, the element that scrolls. It must be unique on the page: the commands find the viewport by it.
-}
id : String -> MessageScroller NeedsId msg -> MessageScroller Ready msg
id string (MessageScroller config) =
    MessageScroller { config | id = string }



-- ITEMS


{-| A row with its `id` and its content: a message, a marker, a typing indicator. The attributes come after the row's own, so they can override them.

The `id` becomes the `id` of the row's element, which [`scrollToMessage`](#scrollToMessage) and [`markPlace`](#markPlace) look for, and its key, which lets the browser keep the reader's place when rows are added around it. It must be unique on the page.

-}
item : String -> List (Attribute msg) -> List (Html msg) -> Item msg
item string attrs children =
    Item { id = string, anchor = False, tail = False, attrs = attrs, children = children }


{-| Makes the row the start of a turn, the part of the conversation that starts a new exchange. Any row can be an anchor: a user's message, a marker like "Marcus joined", a handoff.

Rows are grouped in turns, each starting at an anchor, and [`holdTurn`](#holdTurn) and [`openAtLastAnchor`](#openAtLastAnchor) work with the newest one.

-}
anchor : Item msg -> Item msg
anchor (Item config) =
    Item { config | anchor = True }


{-| The rows, in order, with attributes for the element they sit in, a `role="log"` region. Replaces the rows given before.
-}
items : List (Attribute msg) -> List (Item msg) -> MessageScroller status msg -> MessageScroller status msg
items attrs rows (MessageScroller config) =
    MessageScroller { config | content = attrs, items = rows }



-- SETTERS


{-| The accessible name of the viewport. `"Messages"` by default.
-}
label : String -> MessageScroller status msg -> MessageScroller status msg
label string (MessageScroller config) =
    MessageScroller { config | label = string }


{-| Sets `aria-busy` on the transcript while a reply streams, so that assistive technology waits for the finished row.
-}
busy : Bool -> MessageScroller status msg -> MessageScroller status msg
busy bool (MessageScroller config) =
    MessageScroller { config | busy = bool }


{-| A CSS length for the padding around the rows, such as `"16px"` or `"var(--card-spacing)"`. There is none by default.

This is a setter, rather than a class, because the room under a held turn is worked out from it.

-}
padding : String -> MessageScroller status msg -> MessageScroller status msg
padding length (MessageScroller config) =
    MessageScroller { config | padding = Just length }


{-| A CSS length for the space between rows and between turns, such as `"calc(var(--spacing) * 4)"`. It is `calc(var(--spacing) * 6)` by default.
-}
gap : String -> MessageScroller status msg -> MessageScroller status msg
gap length (MessageScroller config) =
    MessageScroller { config | gap = Just length }


{-| How many pixels of what came before stay in view above a new turn. 64 by default.
-}
peek : Int -> MessageScroller status msg -> MessageScroller status msg
peek pixels (MessageScroller config) =
    MessageScroller { config | peek = Just pixels }


{-| The space in pixels above the reading line, where a new turn lands, on top of the [`peek`](#peek). 0 by default.

The commands take their own margin, since they don't know the scroller.

-}
scrollMargin : Int -> MessageScroller status msg -> MessageScroller status msg
scrollMargin pixels (MessageScroller config) =
    MessageScroller { config | margin = Just pixels }


{-| Keeps room under the newest turn, so that it can sit at the reading line while its reply streams in.

Set it once a turn has been sent, together with `scrollToEnd`, and leave it unset when opening a saved transcript. Only a turn that starts at an [`anchor`](#anchor) is held.

-}
holdTurn : MessageScroller status msg -> MessageScroller status msg
holdTurn (MessageScroller config) =
    MessageScroller { config | hold = True }



-- OPENING POSITION


{-| Opens at the first row.
-}
openAtStart : MessageScroller status msg -> MessageScroller status msg
openAtStart (MessageScroller config) =
    MessageScroller { config | open = AtStart }


{-| Opens with the last anchor at the reading line, and the turn below it, which is often the last thing the reader needs to see.

A last turn that fits in the viewport opens at the end, and so does a transcript without [`anchor`](#anchor)s.

-}
openAtLastAnchor : MessageScroller status msg -> MessageScroller status msg
openAtLastAnchor (MessageScroller config) =
    MessageScroller { config | open = AtLastAnchor }



-- BUTTONS


{-| Renders a button that sends the message when it is clicked. Run `scrollToEnd Smooth` in response.
-}
endButton : msg -> MessageScroller status msg -> MessageScroller status msg
endButton msg (MessageScroller config) =
    MessageScroller { config | endButton = Just msg }


{-| Renders a button at the top that sends the message when it is clicked. Run `scrollToStart Smooth` in response.
-}
startButton : msg -> MessageScroller status msg -> MessageScroller status msg
startButton msg (MessageScroller config) =
    MessageScroller { config | startButton = Just msg }



-- SCROLL STATE


{-| Called with the edges the viewport can still scroll toward, whenever it scrolls: `start` when it is more than 8px from the top, `end` when it is more than 8px from the bottom.

`not edges.end` means the reader is at the end.

-}
onScroll : ({ start : Bool, end : Bool } -> msg) -> MessageScroller status msg -> MessageScroller status msg
onScroll toMsg (MessageScroller config) =
    MessageScroller { config | onScroll = Just toMsg }


{-| Tells the buttons which edges there is something to scroll toward, as `data-scrollable="start end"` on the scroller. Pass what `onScroll` last reported.

Only Firefox needs it, since it can't tell the buttons by itself. It also styles the scroller: `group-data-[scrollable~=end]/message-scroller:…`.

-}
scrollable : { start : Bool, end : Bool } -> MessageScroller status msg -> MessageScroller status msg
scrollable edges (MessageScroller config) =
    MessageScroller { config | scrollable = edges }



-- RENDERING


{-| Renders the scroller with the given attributes after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> MessageScroller Ready msg -> Html msg
toHtml attrs (MessageScroller config) =
    Html.div
        ([ attribute "data-slot" "message-scroller"
         , attribute "data-open" (openName config.open)
         , class "group/message-scroller relative flex size-full min-h-0 flex-col overflow-hidden"
         , class "[timeline-scope:--message-scroller] [container-name:message-scroller]"
         , class "[--message-scroller-peek:64px] [--message-scroller-margin:0px] [--message-scroller-padding:0px] [--message-scroller-gap:--spacing(6)]"
         , class "supports-[animation-timeline:scroll()]:[animation:message-scroller-end_1ms_steps(2,jump-none)_both,message-scroller-start_1ms_steps(2,jump-none)_both] supports-[animation-timeline:scroll()]:[animation-timeline:--message-scroller,--message-scroller] supports-[animation-timeline:scroll()]:[animation-range:calc(100%-16px)_100%,0_16px]"
         ]
            ++ (if config.hold then
                    [ attribute "data-hold" "" ]

                else
                    []
               )
            ++ scrollableAttributes config.scrollable
            ++ styleAttributes config
            ++ attrs
        )
        (List.filterMap identity
            [ Just (viewViewport config)
            , Maybe.map (viewButton ToEnd) config.endButton
            , Maybe.map (viewButton ToStart) config.startButton
            ]
        )


viewViewport :
    { config
        | id : String
        , label : String
        , busy : Bool
        , hold : Bool
        , open : Open
        , content : List (Attribute msg)
        , items : List (Item msg)
        , onScroll : Maybe ({ start : Bool, end : Bool } -> msg)
    }
    -> Html msg
viewViewport config =
    let
        firstTail =
            List.length config.items - tailRows

        rows =
            List.indexedMap
                (\index (Item row) -> Item { row | tail = index >= firstTail })
                config.items

        turns =
            groupTurns rows

        lastAnchor =
            rows
                |> List.filter (\(Item row) -> row.anchor)
                |> List.reverse
                |> List.head
                |> Maybe.map (\(Item row) -> row.id)

        lastTurn =
            List.length turns - 1
    in
    Html.div
        ([ attribute "data-slot" "message-scroller-viewport"
         , Html.Attributes.id config.id
         , attribute "role" "region"
         , attribute "aria-label" config.label
         , tabindex 0
         , class "message-scroller-viewport size-full min-h-0 min-w-0 scrollbar-thin scrollbar-gutter-stable overflow-y-auto overscroll-contain contain-content [container-type:size] [scroll-timeline:--message-scroller_block]"
         , class "group-data-[open=last-anchor]/message-scroller:[scroll-padding-top:calc(var(--message-scroller-margin)+var(--message-scroller-peek))]"
         ]
            ++ (case config.onScroll of
                    Just toMsg ->
                        [ Html.Events.on "scroll" (scrollDecoder toMsg) ]

                    Nothing ->
                        []
               )
        )
        [ Html.Keyed.node "div"
            ([ attribute "data-slot" "message-scroller-content"
             , attribute "role" "log"
             , attribute "aria-relevant" "additions"
             , class "relative flex h-max min-h-full flex-col gap-(--message-scroller-gap) p-(--message-scroller-padding)"
             ]
                ++ (if config.busy then
                        [ attribute "aria-busy" "true" ]

                    else
                        []
                   )
                ++ config.content
            )
            (( "end", viewEnd config.open )
                :: List.indexedMap
                    (\index turn ->
                        viewTurn lastAnchor (config.hold && index == lastTurn && List.any (\(Item row) -> row.anchor) turn) turn
                    )
                    turns
            )
        ]


{-| The end of the transcript. It is the first child of the transcript and sits at its bottom, so while it is in view the browser keeps it there as the transcript grows, which is how the scroller follows the end. The opening snap lands on it.
-}
viewEnd : Open -> Html msg
viewEnd open =
    Html.div
        [ attribute "data-slot" "message-scroller-end"
        , attribute "aria-hidden" "true"
        , class "pointer-events-none absolute inset-x-0 bottom-0 h-2"
        , class
            (case open of
                AtStart ->
                    ""

                _ ->
                    "snap-end"
            )
        ]
        []


{-| Rows before the first anchor are a turn of their own, and each anchor starts another.
-}
groupTurns : List (Item msg) -> List (List (Item msg))
groupTurns rows =
    let
        step ((Item row) as current) ( open, done ) =
            if row.anchor then
                ( [], (current :: open) :: done )

            else
                ( current :: open, done )

        ( leading, turns ) =
            List.foldr step ( [], [] ) rows
    in
    if List.isEmpty leading then
        turns

    else
        leading :: turns


{-| A turn is keyed by its anchor, and the turn before the first anchor by a name of its own, so that rows added above the first row go into the same element.
-}
viewTurn : Maybe String -> Bool -> List (Item msg) -> ( String, Html msg )
viewTurn lastAnchor held turn =
    ( case turn of
        (Item first) :: _ ->
            if first.anchor then
                "turn:" ++ first.id

            else
                "lead"

        [] ->
            "lead"
    , Html.Keyed.node "div"
        [ attribute "data-slot" "message-scroller-turn"
        , class "flex min-w-0 flex-none flex-col gap-(--message-scroller-gap)"
        , class
            (if held then
                "min-h-[calc(100cqh-var(--message-scroller-margin)-var(--message-scroller-peek)-var(--message-scroller-padding))]"

             else
                ""
            )
        ]
        (List.map (viewItem lastAnchor) turn)
    )


viewItem : Maybe String -> Item msg -> ( String, Html msg )
viewItem lastAnchor (Item config) =
    ( config.id
    , Html.div
        ([ attribute "data-slot" "message-scroller-item"
         , attribute "data-scroll-anchor"
            (if config.anchor then
                "true"

             else
                "false"
            )
         , Html.Attributes.id config.id
         , class "min-w-0 shrink-0"
         ]
            ++ (if config.tail then
                    []

                else
                    [ class "[contain-intrinsic-size:auto_10rem] [content-visibility:auto]" ]
               )
            ++ (if config.anchor && lastAnchor == Just config.id then
                    [ class "group-data-[open=last-anchor]/message-scroller:snap-start" ]

                else
                    []
               )
            ++ config.attrs
        )
        config.children
    )


viewButton : Direction -> msg -> Html msg
viewButton direction msg =
    Button.new
        |> Button.icon (viewArrow direction)
        |> Button.label
            (case direction of
                ToEnd ->
                    "Scroll to end"

                ToStart ->
                    "Scroll to start"
            )
        |> Button.secondary
        |> Button.small
        |> Button.toHtml
            [ attribute "data-slot" "message-scroller-button"
            , attribute "data-direction"
                (case direction of
                    ToEnd ->
                        "end"

                    ToStart ->
                        "start"
                )
            , Html.Events.onClick msg

            -- The colors, the transitions and the hidden state are in `message-scroller-button`.
            , class "message-scroller-button absolute inset-s-1/2 -translate-x-1/2 duration-200 ease-[cubic-bezier(0.23,1,0.32,1)] data-[direction=end]:bottom-4 data-[direction=start]:top-4 rtl:translate-x-1/2"
            ]


{-| An arrow drawn with a stem and a chevron, so that the scroller needs no icon package. It points down, and up when turned.
-}
viewArrow : Direction -> Html msg
viewArrow direction =
    Html.span
        [ attribute "aria-hidden" "true"
        , class "relative block size-4 shrink-0 before:absolute before:top-[2.7px] before:left-1/2 before:h-[9.8px] before:w-[1.5px] before:-translate-x-1/2 before:rounded-full before:bg-current after:absolute after:top-1/2 after:left-1/2 after:size-[7.5px] after:-translate-1/2 after:rotate-45 after:rounded-br-[1px] after:border-r-[1.5px] after:border-b-[1.5px] after:border-current"
        , class
            (case direction of
                ToEnd ->
                    ""

                ToStart ->
                    "rotate-180"
            )
        ]
        []



-- ATTRIBUTES


openName : Open -> String
openName open =
    case open of
        AtEnd ->
            "end"

        AtStart ->
            "start"

        AtLastAnchor ->
            "last-anchor"


scrollableAttributes : { start : Bool, end : Bool } -> List (Attribute msg)
scrollableAttributes edges =
    case ( edges.start, edges.end ) of
        ( True, True ) ->
            [ attribute "data-scrollable" "start end" ]

        ( True, False ) ->
            [ attribute "data-scrollable" "start" ]

        ( False, True ) ->
            [ attribute "data-scrollable" "end" ]

        ( False, False ) ->
            []


{-| The setters that were used become custom properties on the root. They are set inline, since a class couldn't override the defaults, and not with `Html.Attributes.style`, which doesn't take custom properties.
-}
styleAttributes :
    { config
        | peek : Maybe Int
        , margin : Maybe Int
        , padding : Maybe String
        , gap : Maybe String
    }
    -> List (Attribute msg)
styleAttributes config =
    case
        List.filterMap identity
            [ Maybe.map (\pixels -> "--message-scroller-peek: " ++ String.fromInt pixels ++ "px") config.peek
            , Maybe.map (\pixels -> "--message-scroller-margin: " ++ String.fromInt pixels ++ "px") config.margin
            , Maybe.map (\length -> "--message-scroller-padding: " ++ length) config.padding
            , Maybe.map (\length -> "--message-scroller-gap: " ++ length) config.gap
            ]
    of
        [] ->
            []

        declarations ->
            [ attribute "style" (String.join "; " declarations) ]


{-| The rows at the end of the transcript are always rendered. The others are skipped by the browser while they are off screen (`content-visibility: auto`), which keeps long transcripts cheap, but a skipped row only has a guessed height. Where the scroller is mounted out of sight, the end of the transcript would then be a guess too, and the browsers that can't keep the reader's place when a row is measured, iOS Safari, would open short of it.
-}
tailRows : Int
tailRows =
    24


{-| How far from an edge the viewport counts as being at it.
-}
edgeThreshold : Float
edgeThreshold =
    8


scrollDecoder : ({ start : Bool, end : Bool } -> msg) -> Decode.Decoder msg
scrollDecoder toMsg =
    Decode.map3
        (\top height client ->
            toMsg
                { start = top > edgeThreshold
                , end = height - top - client > edgeThreshold
                }
        )
        (Decode.at [ "target", "scrollTop" ] Decode.float)
        (Decode.at [ "target", "scrollHeight" ] Decode.float)
        (Decode.at [ "target", "clientHeight" ] Decode.float)



-- COMMANDS


{-| Scrolls to the end of the transcript. Takes the `id` of the viewport.

With [`holdTurn`](#holdTurn) set, the end is the newest turn at the reading line.

-}
scrollToEnd : Behavior -> String -> Task Browser.Dom.Error ()
scrollToEnd behavior viewport =
    Browser.Dom.getViewportOf viewport
        |> Task.andThen (\v -> scrollTo behavior viewport v (v.scene.height - v.viewport.height))


{-| Scrolls to the start of the transcript. Takes the `id` of the viewport.
-}
scrollToStart : Behavior -> String -> Task Browser.Dom.Error ()
scrollToStart behavior viewport =
    Browser.Dom.getViewportOf viewport
        |> Task.andThen (\v -> scrollTo behavior viewport v 0)


{-| Scrolls the row with the given `id` so that its top is `margin` pixels below the top of the viewport, or as close as the transcript allows. The `id` is the one given to [`item`](#item).

    MessageScroller.scrollToMessage MessageScroller.Smooth
        { viewport = "chat"
        , message = "message-42"
        , margin = 0
        }

-}
scrollToMessage : Behavior -> { viewport : String, message : String, margin : Float } -> Task Browser.Dom.Error ()
scrollToMessage behavior { viewport, message, margin } =
    Task.map3 (\v ve me -> ( v, v.viewport.y + (me.element.y - ve.element.y) - margin ))
        (Browser.Dom.getViewportOf viewport)
        (Browser.Dom.getElement viewport)
        (Browser.Dom.getElement message)
        |> Task.andThen (\( v, target ) -> scrollTo behavior viewport v (clamp 0 (v.scene.height - v.viewport.height) target))


scrollTo : Behavior -> String -> Browser.Dom.Viewport -> Float -> Task Browser.Dom.Error ()
scrollTo behavior viewport v target =
    case behavior of
        Instant ->
            Browser.Dom.setViewportOf viewport v.viewport.x target

        Smooth ->
            ease viewport v.viewport.x v.viewport.y target 1


{-| The viewport moves in a dozen steps, a frame apart, slowing down as it arrives.
-}
ease : String -> Float -> Float -> Float -> Int -> Task Browser.Dom.Error ()
ease viewport x from target step =
    if step > easeSteps then
        Task.succeed ()

    else
        Process.sleep 16
            |> Task.andThen (\_ -> Browser.Dom.setViewportOf viewport x (from + (target - from) * easeOut (toFloat step / toFloat easeSteps)))
            |> Task.andThen (\_ -> ease viewport x from target (step + 1))


easeSteps : Int
easeSteps =
    12


easeOut : Float -> Float
easeOut progress =
    1 - (1 - progress) ^ 3


{-| Measures where a row is in the viewport, to put it back there after rows are added above it. Takes the `id` of the viewport and of a row that stays.
-}
markPlace : { viewport : String, message : String } -> Task Browser.Dom.Error Place
markPlace { viewport, message } =
    Task.map2
        (\ve me -> Place { viewport = viewport, message = message, offset = me.element.y - ve.element.y })
        (Browser.Dom.getElement viewport)
        (Browser.Dom.getElement message)


{-| Scrolls so that the row is where it was when it was measured. Run it once the new rows have rendered. It changes nothing where the browser has already kept the row in place.

The place comes from [`markPlace`](#markPlace).

-}
restorePlace : Place -> Task Browser.Dom.Error ()
restorePlace (Place place) =
    Task.map3 (\v ve me -> ( v, v.viewport.y + (me.element.y - ve.element.y) - place.offset ))
        (Browser.Dom.getViewportOf place.viewport)
        (Browser.Dom.getElement place.viewport)
        (Browser.Dom.getElement place.message)
        |> Task.andThen (\( v, target ) -> Browser.Dom.setViewportOf place.viewport v.viewport.x target)
