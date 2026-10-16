module UI.Item exposing
    ( Item, Ready, NeedsTitle
    , new
    , title, description, icon, media, image, detail, actions, header, footer
    , outline, muted
    , small, extraSmall
    , toHtml, toAnchor
    , group, separator
    )

{-| Displays content with media, a title, a description and actions.

An item is built with `new`, filled in with setters, and rendered with `toHtml`:

    Item.new
        |> Item.icon (Phosphor.shieldWarning Regular |> Phosphor.toHtml [])
        |> Item.title [ text "Security Alert" ]
        |> Item.description [ text "New login detected from unknown device." ]
        |> Item.actions
            [ Button.new
                |> Button.text "Review"
                |> Button.outline
                |> Button.small
                |> Button.toHtml []
            ]
        |> Item.outline
        |> Item.toHtml []

Every item needs a title. `toHtml` won't accept one until it has it:

    Item.new
        |> Item.description [ text "No title yet" ]
        |> Item.toHtml []
        -- compile error: the item is still `NeedsTitle`

Render an item as a link with `toAnchor`. The hover and focus states apply to the anchor:

    Item.new
        |> Item.title [ text "Visit our documentation" ]
        |> Item.description [ text "Learn how to get started with our components." ]
        |> Item.toAnchor [ href "/docs" ]

Group related items with `group`, and put a `separator` between them:

    Item.group []
        [ Item.new
            |> Item.title [ text "First" ]
            |> Item.outline
            |> Item.toHtml []
        , Item.separator
        , Item.new
            |> Item.title [ text "Second" ]
            |> Item.outline
            |> Item.toHtml []
        ]

@docs Item, Ready, NeedsTitle


# Constructor

@docs new


# Content

The media before the title is an `icon`, any `media` such as an avatar, or an `image`. They fill the same slot, so the last one given wins.

@docs title, description, icon, media, image, detail, actions, header, footer


# Variants

The last variant given wins.

@docs outline, muted


# Sizes

The last size given wins.

@docs small, extraSmall


# Rendering

@docs toHtml, toAnchor


# Group

@docs group, separator

-}

import Html exposing (Attribute, Html, div, p)
import Html.Attributes exposing (attribute, class)
import UI.Separator as Separator


{-| An item being built.

`status` is [`Ready`](#Ready) once the item can be rendered, or [`NeedsTitle`](#NeedsTitle) while it still lacks its title.

-}
type Item status msg
    = Item
        { variant : Variant
        , size : Size
        , media : Maybe (Media msg)
        , title : List (Html msg)
        , description : List (Html msg)
        , detail : List (Html msg)
        , actions : List (Html msg)
        , header : List (Html msg)
        , footer : List (Html msg)
        }


{-| The item can be rendered.
-}
type Ready
    = Ready


{-| An item that still needs a [`title`](#title) before it can be rendered.
-}
type NeedsTitle
    = NeedsTitle


type Variant
    = Default
    | Outline
    | Muted


type Size
    = Regular
    | Small
    | ExtraSmall


type Media msg
    = Icon (Html msg)
    | Custom (List (Html msg))
    | Image (List (Attribute msg))



-- CONSTRUCTOR


{-| A blank item. Give it a [`title`](#title) to make it renderable.
-}
new : Item NeedsTitle msg
new =
    Item
        { variant = Default
        , size = Regular
        , media = Nothing
        , title = []
        , description = []
        , detail = []
        , actions = []
        , header = []
        , footer = []
        }



-- CONTENT


{-| The heading of the item. Required.
-}
title : List (Html msg) -> Item status msg -> Item Ready msg
title children (Item config) =
    Item { config | title = children }


{-| Supporting text below the title. Links inside it are underlined.
-}
description : List (Html msg) -> Item status msg -> Item status msg
description children (Item config) =
    Item { config | description = children }


{-| An icon before the content, sized to `size-4` unless it sets its own size.
-}
icon : Html msg -> Item status msg -> Item status msg
icon child (Item config) =
    Item { config | media = Just (Icon child) }


{-| Anything before the content, such as an avatar or a larger icon, shown as is.
-}
media : List (Html msg) -> Item status msg -> Item status msg
media children (Item config) =
    Item { config | media = Just (Custom children) }


{-| An image before the content, cropped to a rounded square that follows the item's size. The attributes go on the `<img>`.
-}
image : List (Attribute msg) -> Item status msg -> Item status msg
image attrs (Item config) =
    Item { config | media = Just (Image attrs) }


{-| A second, muted column after the content, such as a duration.
-}
detail : List (Html msg) -> Item status msg -> Item status msg
detail children (Item config) =
    Item { config | detail = children }


{-| Actions at the end of the item, such as buttons or icons.
-}
actions : List (Html msg) -> Item status msg -> Item status msg
actions children (Item config) =
    Item { config | actions = children }


{-| A full-width row above everything else.
-}
header : List (Html msg) -> Item status msg -> Item status msg
header children (Item config) =
    Item { config | header = children }


{-| A full-width row below everything else.
-}
footer : List (Html msg) -> Item status msg -> Item status msg
footer children (Item config) =
    Item { config | footer = children }



-- VARIANTS


{-| A visible border.
-}
outline : Item status msg -> Item status msg
outline (Item config) =
    Item { config | variant = Outline }


{-| A muted background.
-}
muted : Item status msg -> Item status msg
muted (Item config) =
    Item { config | variant = Muted }



-- SIZES


{-| A compact item.
-}
small : Item status msg -> Item status msg
small (Item config) =
    Item { config | size = Small }


{-| The most compact item.
-}
extraSmall : Item status msg -> Item status msg
extraSmall (Item config) =
    Item { config | size = ExtraSmall }



-- RENDERING


{-| Renders the item as a `<div>` with the given attributes after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> Item Ready msg -> Html msg
toHtml =
    render div


{-| Renders the item as an `<a>`, which gets hover and focus styles. Pass `href` with the attributes.
-}
toAnchor : List (Attribute msg) -> Item Ready msg -> Html msg
toAnchor =
    render Html.a


render : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> Item Ready msg -> Html msg
render element attrs (Item config) =
    element
        ([ attribute "data-slot" "item"
         , attribute "data-variant" (variantName config.variant)
         , attribute "data-size" (sizeName config.size)
         , class "group/item flex w-full flex-wrap items-center rounded-lg border text-sm transition-colors duration-100 outline-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 [a]:transition-colors [a]:hover:bg-muted"
         , class (variantClass config.variant)
         , class (sizeClass config.size)
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ optional (slot "item-header" "flex basis-full items-center justify-between gap-2") config.header
            , Maybe.map viewMedia config.media
            , Just
                (slot "item-content"
                    contentClass
                    (List.filterMap identity
                        [ Just (slot "item-title" "line-clamp-1 flex w-fit items-center gap-2 text-sm leading-snug font-medium underline-offset-4" config.title)
                        , optional viewDescription config.description
                        ]
                    )
                )
            , optional (\children -> slot "item-content" (contentClass ++ " flex-none text-center") [ viewDescription children ]) config.detail
            , optional (slot "item-actions" "flex items-center gap-2") config.actions
            , optional (slot "item-footer" "flex basis-full items-center justify-between gap-2") config.footer
            ]
        )


optional : (List (Html msg) -> Html msg) -> List (Html msg) -> Maybe (Html msg)
optional view children =
    if List.isEmpty children then
        Nothing

    else
        Just (view children)


viewMedia : Media msg -> Html msg
viewMedia value =
    let
        ( variant, extra, children ) =
            case value of
                Custom items ->
                    ( "default", "bg-transparent", items )

                Icon child ->
                    ( "icon", "[&_svg:not([class*='size-'])]:size-4", [ child ] )

                Image attrs ->
                    ( "image", "size-10 overflow-hidden rounded-sm group-data-[size=sm]/item:size-8 group-data-[size=xs]/item:size-6 [&_img]:size-full [&_img]:object-cover", [ Html.img attrs [] ] )
    in
    div
        [ attribute "data-slot" "item-media"
        , attribute "data-variant" variant
        , class "flex shrink-0 items-center justify-center gap-2 group-has-data-[slot=item-description]/item:translate-y-0.5 group-has-data-[slot=item-description]/item:self-start [&_svg]:pointer-events-none"
        , class extra
        ]
        children


viewDescription : List (Html msg) -> Html msg
viewDescription children =
    p
        [ attribute "data-slot" "item-description"
        , class "line-clamp-2 text-left text-sm leading-normal font-normal text-muted-foreground group-data-[size=xs]/item:text-xs [&>a]:underline [&>a]:underline-offset-4 [&>a:hover]:text-primary"
        ]
        children


contentClass : String
contentClass =
    "flex flex-1 flex-col gap-1 group-data-[size=xs]/item:gap-0 [&+[data-slot=item-content]]:flex-none"


slot : String -> String -> List (Html msg) -> Html msg
slot name classes children =
    div [ attribute "data-slot" name, class classes ] children


variantName : Variant -> String
variantName variant =
    case variant of
        Default ->
            "default"

        Outline ->
            "outline"

        Muted ->
            "muted"


variantClass : Variant -> String
variantClass variant =
    case variant of
        Default ->
            "border-transparent"

        Outline ->
            "border-border"

        Muted ->
            "border-transparent bg-muted/50"


sizeName : Size -> String
sizeName size =
    case size of
        Regular ->
            "default"

        Small ->
            "sm"

        ExtraSmall ->
            "xs"


sizeClass : Size -> String
sizeClass size =
    case size of
        Regular ->
            "gap-2.5 px-3 py-2.5"

        Small ->
            "gap-2.5 px-3 py-2.5"

        ExtraSmall ->
            "gap-2 px-2.5 py-2 in-data-[slot=dropdown-menu-content]:p-0"



-- GROUP


{-| A list of items, spaced according to the size of the items inside. The attributes go after the styles.
-}
group : List (Attribute msg) -> List (Html msg) -> Html msg
group attrs children =
    div
        ([ attribute "role" "list"
         , attribute "data-slot" "item-group"
         , class "group/item-group flex w-full flex-col gap-4 has-data-[size=sm]:gap-2.5 has-data-[size=xs]:gap-2"
         ]
            ++ attrs
        )
        children


{-| A horizontal line between items in a [`group`](#group).
-}
separator : Html msg
separator =
    Separator.new
        |> Separator.toHtml [ attribute "data-slot" "item-separator", class "my-2" ]
