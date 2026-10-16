module UI.Tabs exposing
    ( Tabs, Ready, NeedsId, NeedsHandler
    , new
    , id, onSelect
    , Tab, tab, disabled
    , tabs
    , value
    , line, vertical
    , toHtml
    )

{-| A set of layered sections of content—known as tab panels—that are displayed one at a time.

Tabs are built with `new`, given an `id` and an `onSelect` handler, filled with tabs, and rendered with `toHtml`:

    Tabs.new
        |> Tabs.id "account-tabs"
        |> Tabs.onSelect TabChosen
        |> Tabs.value model.tab
        |> Tabs.tabs
            [ Tabs.tab Account [ text "Account" ] [ text "Make changes to your account here." ]
            , Tabs.tab Password [ text "Password" ] [ text "Change your password here." ]
            ]
        |> Tabs.toHtml [ class "w-100" ]

The selected tab lives in your model, and `onSelect` tells you when the user picks another one. Values can be any type that supports `==`. Without a `value`, the first tab that isn't `disabled` is shown.

Each tab is a `<button role="tab">` and only the selected tab's panel is rendered. The tab list is a `focusgroup`, so the arrow keys move between the tabs, selecting as they go, with no JavaScript, ports or component state. The `id` is required because the tabs and their panels point at each other by it.

@docs Tabs, Ready, NeedsId, NeedsHandler


# Constructor

@docs new


# Required

`toHtml` won't accept tabs until they have both.

@docs id, onSelect


# Tabs

@docs Tab, tab, disabled

@docs tabs


# Value

@docs value


# Setters

@docs line, vertical


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, tabindex, type_)
import Html.Events exposing (onClick)


{-| Tabs being built.

`id` is [`Ready`](#Ready) once the tabs have an [`id`](#id), and `handler` is [`Ready`](#Ready) once they have an [`onSelect`](#onSelect) handler. `a` is the type of the tab values.

-}
type Tabs a id handler msg
    = Tabs
        { id : String
        , onSelect : Maybe (a -> msg)
        , value : Maybe a
        , variant : Variant
        , orientation : Orientation
        , tabs : List (Tab a msg)
        }


{-| The tabs can be rendered.
-}
type Ready
    = Ready


{-| Tabs that still need an [`id`](#id) before they can be rendered.
-}
type NeedsId
    = NeedsId


{-| Tabs that still need an [`onSelect`](#onSelect) handler before they can be rendered.
-}
type NeedsHandler
    = NeedsHandler


{-| A tab and its panel, made with [`tab`](#tab).
-}
type Tab a msg
    = Tab
        { value : a
        , label : List (Html msg)
        , content : List (Html msg)
        , disabled : Bool
        }


type Variant
    = Default
    | Line


type Orientation
    = Horizontal
    | Vertical



-- CONSTRUCTOR


{-| Empty tabs. Give them an [`id`](#id) and an [`onSelect`](#onSelect) handler to make them renderable.
-}
new : Tabs a NeedsId NeedsHandler msg
new =
    Tabs
        { id = ""
        , onSelect = Nothing
        , value = Nothing
        , variant = Default
        , orientation = Horizontal
        , tabs = []
        }



-- REQUIRED


{-| The `id` of the root element. It must be unique on the page: the tabs and panels are named after it.
-}
id : String -> Tabs a NeedsId handler msg -> Tabs a Ready handler msg
id string (Tabs config) =
    Tabs { config | id = string }


{-| Called with the value of the tab the user picks.
-}
onSelect : (a -> msg) -> Tabs a id NeedsHandler msg -> Tabs a id Ready msg
onSelect toMsg (Tabs config) =
    Tabs { config | onSelect = Just toMsg }



-- TABS


{-| A tab with the value it stands for, its label and the content of its panel. A tab with no content has no panel, for tabs that switch something elsewhere on the page.
-}
tab : a -> List (Html msg) -> List (Html msg) -> Tab a msg
tab v label content =
    Tab { value = v, label = label, content = content, disabled = False }


{-| Shows the tab dimmed and makes it unselectable.
-}
disabled : Tab a msg -> Tab a msg
disabled (Tab config) =
    Tab { config | disabled = True }


{-| The tabs, in order.
-}
tabs : List (Tab a msg) -> Tabs a id handler msg -> Tabs a id handler msg
tabs list (Tabs config) =
    Tabs { config | tabs = list }



-- VALUE


{-| The selected tab, usually from your model. Its panel is shown. Without a value, the first tab that isn't [`disabled`](#disabled) is selected.
-}
value : a -> Tabs a id handler msg -> Tabs a id handler msg
value v (Tabs config) =
    Tabs { config | value = Just v }



-- SETTERS


{-| Underlines the selected tab instead of raising it on a muted background.
-}
line : Tabs a id handler msg -> Tabs a id handler msg
line (Tabs config) =
    Tabs { config | variant = Line }


{-| Stacks the tabs and puts the panel beside them.
-}
vertical : Tabs a id handler msg -> Tabs a id handler msg
vertical (Tabs config) =
    Tabs { config | orientation = Vertical }



-- RENDERING


{-| Renders the root with the given attributes after the styles, so they can override them, holding the tab list and the selected tab's panel. Pass a width such as `class "w-100"`.
-}
toHtml : List (Attribute msg) -> Tabs a Ready Ready msg -> Html msg
toHtml attrs (Tabs config) =
    let
        selected =
            case config.value of
                Just v ->
                    Just v

                Nothing ->
                    config.tabs
                        |> List.filter (\(Tab t) -> not t.disabled)
                        |> List.head
                        |> Maybe.map (\(Tab t) -> t.value)

        indexed =
            List.indexedMap Tuple.pair config.tabs

        orientation =
            case config.orientation of
                Horizontal ->
                    "horizontal"

                Vertical ->
                    "vertical"
    in
    Html.div
        ([ Html.Attributes.id config.id
         , attribute "data-slot" "tabs"
         , attribute "data-orientation" orientation
         , attribute ("data-" ++ orientation) ""
         , class "group/tabs flex gap-2 data-horizontal:flex-col"
         ]
            ++ attrs
        )
        (Html.div
            [ attribute "role" "tablist"
            , attribute "aria-orientation" orientation
            , attribute "data-slot" "tabs-list"
            , attribute "data-variant" (variantName config.variant)

            -- Arrow keys move between the tabs, selecting as they go, and the list is one
            -- Tab stop.
            , attribute "focusgroup"
                (case config.orientation of
                    Horizontal ->
                        "tablist inline wrap"

                    Vertical ->
                        "tablist block wrap"
                )
            , class "group/tabs-list inline-flex w-fit items-center justify-center p-[3px] text-muted-foreground group-data-horizontal/tabs:h-8 group-data-vertical/tabs:h-fit group-data-vertical/tabs:flex-col"
            , class (variantClass config.variant)
            ]
            (List.map (viewTab config.id config.onSelect selected) indexed)
            :: List.filterMap (viewPanel config.id selected) indexed
        )


viewTab : String -> Maybe (a -> msg) -> Maybe a -> ( Int, Tab a msg ) -> Html msg
viewTab rootId handler selected ( index, Tab config ) =
    let
        isSelected =
            Just config.value == selected
    in
    Html.button
        ([ type_ "button"
         , Html.Attributes.id (tabId rootId index)
         , attribute "role" "tab"
         , attribute "aria-selected" (boolean isSelected)
         , attribute "data-slot" "tabs-trigger"
         , Html.Attributes.disabled config.disabled
         , tabindex
            (if isSelected then
                0

             else
                -1
            )
         , class "relative inline-flex h-[calc(100%-1px)] flex-1 items-center justify-center gap-1.5 rounded-md border border-transparent px-1.5 py-0.5 text-sm font-medium whitespace-nowrap text-foreground/60 transition-all group-data-vertical/tabs:w-full group-data-vertical/tabs:justify-start hover:text-foreground focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:outline-1 focus-visible:outline-ring disabled:pointer-events-none disabled:opacity-50 aria-disabled:pointer-events-none aria-disabled:opacity-50 dark:text-muted-foreground dark:hover:text-foreground [&_svg]:pointer-events-none [&_svg]:shrink-0 [&_svg:not([class*='size-'])]:size-4 has-data-[icon=inline-end]:pr-1 has-data-[icon=inline-start]:pl-1"
         , class "group-data-[variant=line]/tabs-list:bg-transparent group-data-[variant=line]/tabs-list:data-active:bg-transparent dark:group-data-[variant=line]/tabs-list:data-active:border-transparent dark:group-data-[variant=line]/tabs-list:data-active:bg-transparent"
         , class "data-active:bg-background data-active:text-foreground dark:data-active:border-input dark:data-active:bg-input/30 dark:data-active:text-foreground group-data-[variant=default]/tabs-list:data-active:shadow-sm group-data-[variant=line]/tabs-list:data-active:shadow-none"
         , class "after:absolute after:bg-foreground after:opacity-0 after:transition-opacity group-data-horizontal/tabs:after:inset-x-0 group-data-horizontal/tabs:after:bottom-[-5px] group-data-horizontal/tabs:after:h-0.5 group-data-vertical/tabs:after:inset-y-0 group-data-vertical/tabs:after:-right-1 group-data-vertical/tabs:after:w-0.5 group-data-[variant=line]/tabs-list:data-active:after:opacity-100"
         ]
            ++ (if isSelected then
                    [ attribute "data-active" "" ]

                else
                    []
               )
            ++ (if List.isEmpty config.content then
                    []

                else
                    [ attribute "aria-controls" (panelId rootId index) ]
               )
            ++ (case handler of
                    Just toMsg ->
                        [ onClick (toMsg config.value) ]

                    Nothing ->
                        []
               )
        )
        config.label


viewPanel : String -> Maybe a -> ( Int, Tab a msg ) -> Maybe (Html msg)
viewPanel rootId selected ( index, Tab config ) =
    if Just config.value == selected && not (List.isEmpty config.content) then
        Just
            (Html.div
                [ Html.Attributes.id (panelId rootId index)
                , attribute "role" "tabpanel"
                , attribute "aria-labelledby" (tabId rootId index)
                , attribute "data-slot" "tabs-content"
                , tabindex 0
                , class "flex-1 text-sm outline-none"
                ]
                config.content
            )

    else
        Nothing


tabId : String -> Int -> String
tabId rootId index =
    rootId ++ "-tab-" ++ String.fromInt index


panelId : String -> Int -> String
panelId rootId index =
    rootId ++ "-panel-" ++ String.fromInt index



-- shadcn merges its base and variant classes with tailwind-merge, which drops the base
-- classes a variant overrides. Elm keeps every class, so the list's radius and
-- background live only in `variantClass`.


variantClass : Variant -> String
variantClass v =
    case v of
        Default ->
            "rounded-lg bg-muted"

        Line ->
            "gap-1 rounded-none bg-transparent"


variantName : Variant -> String
variantName v =
    case v of
        Default ->
            "default"

        Line ->
            "line"


boolean : Bool -> String
boolean b =
    if b then
        "true"

    else
        "false"
