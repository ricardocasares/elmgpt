module UI.Separator exposing
    ( Separator
    , new
    , vertical
    , toHtml
    )

{-| Visually separates content.

    Separator.new
        |> Separator.toHtml [ class "my-4" ]

    Separator.new
        |> Separator.vertical
        |> Separator.toHtml []

The separator is decorative and hidden from assistive technology. For a meaningful
separator, pass `attribute "role" "separator"`.

@docs Separator


# Constructor

@docs new


# Orientation

@docs vertical


# Rendering

@docs toHtml

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


{-| A separator being built.
-}
type Separator
    = Separator { orientation : Orientation }


type Orientation
    = Horizontal
    | Vertical



-- CONSTRUCTOR


{-| A horizontal separator.
-}
new : Separator
new =
    Separator { orientation = Horizontal }



-- ORIENTATION


{-| -}
vertical : Separator -> Separator
vertical (Separator config) =
    Separator { config | orientation = Vertical }



-- RENDERING


{-| Renders the separator. The attributes go on the root, after the styles, so classes can override them.
-}
toHtml : List (Attribute msg) -> Separator -> Html msg
toHtml attrs (Separator config) =
    Html.div
        ([ attribute "data-slot" "separator"
         , attribute "role" "none"
         , attribute "data-orientation" (orientationName config.orientation)
         , class "shrink-0 bg-border data-[orientation=horizontal]:h-px data-[orientation=horizontal]:w-full data-[orientation=vertical]:w-px data-[orientation=vertical]:self-stretch"
         ]
            ++ attrs
        )
        []


orientationName : Orientation -> String
orientationName orientation =
    case orientation of
        Horizontal ->
            "horizontal"

        Vertical ->
            "vertical"
