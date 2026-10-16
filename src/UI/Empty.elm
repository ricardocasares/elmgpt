module UI.Empty exposing
    ( Empty, Ready, NeedsTitle
    , new
    , title, description, icon, media, content
    , toHtml
    )

{-| Displays an empty state.

An empty state is built with `new`, filled in with setters, and rendered with `toHtml`:

    Empty.new
        |> Empty.icon (Phosphor.folder Regular |> Phosphor.toHtml [])
        |> Empty.title [ text "No projects yet" ]
        |> Empty.description [ text "Get started by creating your first project." ]
        |> Empty.content
            [ Button.new
                |> Button.text "Create project"
                |> Button.toHtml []
            ]
        |> Empty.toHtml [ class "border border-dashed" ]

Every empty state needs a title. `toHtml` won't accept one until it has it:

    Empty.new
        |> Empty.description [ text "No title yet" ]
        |> Empty.toHtml []
        -- compile error: the empty state is still `NeedsTitle`

The media above the title is either an `icon`, placed on a muted square, or any `media`
such as an avatar or an image, shown as is:

    Empty.new
        |> Empty.media
            [ Avatar.new
                |> Avatar.fallback [ text "LR" ]
                |> Avatar.toHtml [ class "size-12" ]
            ]
        |> Empty.title [ text "User offline" ]
        |> Empty.toHtml []

@docs Empty, Ready, NeedsTitle


# Constructor

@docs new


# Setters

`icon` and `media` fill the same slot, so the last one given wins.

@docs title, description, icon, media, content


# Rendering

@docs toHtml

The root is a dashed, borderless box. Add `border` for an outline, or a `bg-*` class for a background.

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, class)


{-| An empty state being built.

`status` is [`Ready`](#Ready) once the empty state can be rendered, or [`NeedsTitle`](#NeedsTitle) while it still lacks its title.

-}
type Empty status msg
    = Empty
        { media : Maybe (Media msg)
        , title : List (Html msg)
        , description : List (Html msg)
        , content : List (Html msg)
        }


{-| The empty state can be rendered.
-}
type Ready
    = Ready


{-| An empty state that still needs a [`title`](#title) before it can be rendered.
-}
type NeedsTitle
    = NeedsTitle


type Media msg
    = Icon (Html msg)
    | Media (List (Html msg))



-- CONSTRUCTOR


{-| A blank empty state. Give it a [`title`](#title) to make it renderable.
-}
new : Empty NeedsTitle msg
new =
    Empty
        { media = Nothing
        , title = []
        , description = []
        , content = []
        }



-- SETTERS


{-| The heading of the empty state. Required.
-}
title : List (Html msg) -> Empty status msg -> Empty Ready msg
title children (Empty config) =
    Empty { config | title = children }


{-| Supporting text below the title. Links inside it are underlined.
-}
description : List (Html msg) -> Empty status msg -> Empty status msg
description children (Empty config) =
    Empty { config | description = children }


{-| An icon above the title, placed on a muted, rounded square.
-}
icon : Html msg -> Empty status msg -> Empty status msg
icon child (Empty config) =
    Empty { config | media = Just (Icon child) }


{-| Anything above the title, such as an avatar or an image, shown as is.
-}
media : List (Html msg) -> Empty status msg -> Empty status msg
media children (Empty config) =
    Empty { config | media = Just (Media children) }


{-| Actions below the header, such as buttons, inputs or links, stacked in a column.
-}
content : List (Html msg) -> Empty status msg -> Empty status msg
content children (Empty config) =
    Empty { config | content = children }



-- RENDERING


{-| Renders the empty state with the given attributes after the styles, so they can override them.
-}
toHtml : List (Attribute msg) -> Empty Ready msg -> Html msg
toHtml attrs (Empty config) =
    div
        ([ attribute "data-slot" "empty"
         , class "flex w-full min-w-0 flex-1 flex-col items-center justify-center gap-4 rounded-xl border-dashed p-6 text-center text-balance"
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Just
                (slot "empty-header"
                    "flex max-w-sm flex-col items-center gap-2"
                    (List.filterMap identity
                        [ Maybe.map viewMedia config.media
                        , Just (slot "empty-title" "text-sm font-medium tracking-tight" config.title)
                        , if List.isEmpty config.description then
                            Nothing

                          else
                            Just (slot "empty-description" descriptionClass config.description)
                        ]
                    )
                )
            , if List.isEmpty config.content then
                Nothing

              else
                Just (slot "empty-content" "flex w-full max-w-sm min-w-0 flex-col items-center gap-2.5 text-sm text-balance" config.content)
            ]
        )


viewMedia : Media msg -> Html msg
viewMedia value =
    let
        ( variant, extra, children ) =
            case value of
                Icon child ->
                    ( "icon", "size-8 rounded-lg bg-muted text-foreground [&_svg:not([class*='size-'])]:size-4", [ child ] )

                Media items ->
                    ( "default", "bg-transparent", items )
    in
    div
        [ attribute "data-slot" "empty-icon"
        , attribute "data-variant" variant
        , class "mb-2 flex shrink-0 items-center justify-center [&_svg]:pointer-events-none [&_svg]:shrink-0"
        , class extra
        ]
        children


descriptionClass : String
descriptionClass =
    "text-sm/relaxed text-muted-foreground [&>a]:underline [&>a]:underline-offset-4 [&>a:hover]:text-primary"


slot : String -> String -> List (Html msg) -> Html msg
slot name classes children =
    div [ attribute "data-slot" name, class classes ] children
