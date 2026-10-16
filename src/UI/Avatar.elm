module UI.Avatar exposing
    ( Avatar, Single, Group, Ready, NeedsFallback
    , new
    , fallback, img, badge
    , small, large
    , group, groupCount
    , toHtml
    )

{-| An image element with a fallback for representing the user.

An avatar is built with `new`, filled in with setters, and rendered with `toHtml`:

    Avatar.new
        |> Avatar.fallback [ text "CN" ]
        |> Avatar.img [ src "https://github.com/shadcn.png", alt "@shadcn" ]
        |> Avatar.toHtml []

Every avatar needs a fallback. `toHtml` won't accept an avatar until it has one:

    Avatar.new
        |> Avatar.img [ src "https://github.com/shadcn.png", alt "@shadcn" ]
        |> Avatar.toHtml []
        -- compile error: the avatar is still `NeedsFallback`

The image covers the fallback once it loads. There is no load-error handling: if the
image URL may be missing, leave the image out.

A badge is a small status dot in the bottom-right corner:

    Avatar.new
        |> Avatar.fallback [ text "ER" ]
        |> Avatar.badge [ class "bg-green-600!" ] []
        |> Avatar.toHtml []

Avatars can be stacked in a group, optionally ending with a count of how many more there are:

    Avatar.group
        (Avatar.new
            |> Avatar.fallback [ text "AB" ]
        )
        [ Avatar.new
            |> Avatar.fallback [ text "CD" ]
        ]
        |> Avatar.groupCount [ text "+3" ]
        |> Avatar.toHtml []

@docs Avatar, Single, Group, Ready, NeedsFallback


# Constructor

@docs new


# Setters

@docs fallback, img, badge


# Sizes

The default size is medium. When several sizes are given, the last one wins.

@docs small, large


# Groups

@docs group, groupCount


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


{-| An avatar or a group of them being built.

`shape` is [`Single`](#Single) for one avatar or [`Group`](#Group) for a stack of them.
`status` is [`Ready`](#Ready) once it can be rendered, or [`NeedsFallback`](#NeedsFallback)
while a single avatar still lacks its fallback. A group is never empty, so it is always `Ready`.

-}
type Avatar shape status msg
    = Avatar (Content msg)


type Content msg
    = One (Config msg)
    | Many
        { members : List (Config msg)
        , count : Maybe (List (Html msg))
        }


type alias Config msg =
    { size : Size
    , fallback : List (Html msg)
    , image : Maybe (List (Attribute msg))
    , badge : Maybe (Html msg)
    }


{-| One avatar, made with [`new`](#new).
-}
type Single
    = Single


{-| A stack of avatars, made with [`group`](#group).
-}
type Group
    = Group


{-| The avatar can be rendered.
-}
type Ready
    = Ready


{-| An avatar that still needs [`fallback`](#fallback) before it can be rendered.
-}
type NeedsFallback
    = NeedsFallback


type Size
    = Medium
    | Small
    | Large



-- CONSTRUCTOR


{-| An empty avatar. Give it a [`fallback`](#fallback) to make it renderable.
-}
new : Avatar Single NeedsFallback msg
new =
    Avatar
        (One
            { size = Medium
            , fallback = []
            , image = Nothing
            , badge = Nothing
            }
        )



-- SETTERS


{-| Shown while the image loads, or when there is no image. Usually initials. Required.
-}
fallback : List (Html msg) -> Avatar Single status msg -> Avatar Single Ready msg
fallback children =
    update (\config -> { config | fallback = children })


{-| The picture, covering the fallback once it loads. Pass `src` and `alt`; the attributes come after the styles, so they can override them.
-}
img : List (Attribute msg) -> Avatar Single status msg -> Avatar Single status msg
img attrs =
    update (\config -> { config | image = Just attrs })


{-| A small status dot in the bottom-right corner. The attributes come after the styles,
so they can override them, and the children are typically empty or a single icon.
-}
badge : List (Attribute msg) -> List (Html msg) -> Avatar Single status msg -> Avatar Single status msg
badge attrs children =
    update (\config -> { config | badge = Just (viewBadge attrs children) })


{-| -}
small : Avatar Single status msg -> Avatar Single status msg
small =
    update (\config -> { config | size = Small })


{-| -}
large : Avatar Single status msg -> Avatar Single status msg
large =
    update (\config -> { config | size = Large })


update : (Config msg -> Config msg) -> Avatar Single status msg -> Avatar Single other msg
update change (Avatar content) =
    case content of
        One config ->
            Avatar (One (change config))

        Many _ ->
            Avatar content



-- GROUPS


{-| Stacks the given avatars so they overlap, in order. The first one is separate so a group is never empty.
-}
group : Avatar Single Ready msg -> List (Avatar Single Ready msg) -> Avatar Group Ready msg
group first rest =
    Avatar (Many { members = List.filterMap single (first :: rest), count = Nothing })


single : Avatar Single Ready msg -> Maybe (Config msg)
single (Avatar content) =
    case content of
        One config ->
            Just config

        Many _ ->
            Nothing


{-| Shows how many more avatars there are at the end of the group, as text such as `+3` or an icon.
-}
groupCount : List (Html msg) -> Avatar Group Ready msg -> Avatar Group Ready msg
groupCount children (Avatar content) =
    case content of
        Many stack ->
            Avatar (Many { stack | count = Just children })

        One _ ->
            Avatar content



-- RENDERING


{-| Renders the avatar, or the group, with the given attributes after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> Avatar shape Ready msg -> Html msg
toHtml attrs (Avatar content) =
    case content of
        One config ->
            viewAvatar attrs config

        Many stack ->
            Html.div
                (attribute "data-slot" "avatar-group"
                    :: class "group/avatar-group flex -space-x-2 *:data-[slot=avatar]:ring-2 *:data-[slot=avatar]:ring-background"
                    :: attrs
                )
                (List.map (viewAvatar []) stack.members
                    ++ List.filterMap identity [ Maybe.map viewCount stack.count ]
                )


viewAvatar : List (Attribute msg) -> Config msg -> Html msg
viewAvatar attrs config =
    Html.span
        ([ attribute "data-slot" "avatar"
         , attribute "data-size" (sizeName config.size)
         , class "group/avatar relative flex size-8 shrink-0 rounded-full select-none after:absolute after:inset-0 after:rounded-full after:border after:border-border after:mix-blend-darken data-[size=lg]:size-10 data-[size=sm]:size-6 dark:after:mix-blend-lighten"
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Just (viewFallback config.fallback)
            , Maybe.map viewImage config.image
            , config.badge
            ]
        )


viewImage : List (Attribute msg) -> Html msg
viewImage attrs =
    Html.img
        (attribute "data-slot" "avatar-image"
            :: class "absolute inset-0 aspect-square size-full rounded-full object-cover"
            :: attrs
        )
        []


viewFallback : List (Html msg) -> Html msg
viewFallback children =
    Html.span
        [ attribute "data-slot" "avatar-fallback"
        , class "flex size-full items-center justify-center rounded-full bg-muted text-sm text-muted-foreground group-data-[size=sm]/avatar:text-xs"
        ]
        children


viewBadge : List (Attribute msg) -> List (Html msg) -> Html msg
viewBadge attrs children =
    Html.span
        ([ attribute "data-slot" "avatar-badge"
         , class "absolute right-0 bottom-0 z-10 inline-flex items-center justify-center rounded-full bg-primary text-primary-foreground bg-blend-color ring-2 ring-background select-none"
         , class "group-data-[size=sm]/avatar:size-2 group-data-[size=sm]/avatar:[&>svg]:hidden"
         , class "group-data-[size=default]/avatar:size-2.5 group-data-[size=default]/avatar:[&>svg]:size-2"
         , class "group-data-[size=lg]/avatar:size-3 group-data-[size=lg]/avatar:[&>svg]:size-2"
         ]
            ++ attrs
        )
        children


viewCount : List (Html msg) -> Html msg
viewCount children =
    Html.div
        [ attribute "data-slot" "avatar-group-count"
        , class "relative flex size-8 shrink-0 items-center justify-center rounded-full bg-muted text-sm text-muted-foreground ring-2 ring-background group-has-data-[size=lg]/avatar-group:size-10 group-has-data-[size=sm]/avatar-group:size-6 [&>svg]:size-4 group-has-data-[size=lg]/avatar-group:[&>svg]:size-5 group-has-data-[size=sm]/avatar-group:[&>svg]:size-3"
        ]
        children


sizeName : Size -> String
sizeName size =
    case size of
        Medium ->
            "default"

        Small ->
            "sm"

        Large ->
            "lg"
