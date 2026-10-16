module Icons exposing
    ( arrowUp, check, chevronDown, cloud, copy, monitor, moon, newChat, panelLeft, square, sun, thermometer, umbrella, wind
    )

{-| Line icons from [Lucide](https://lucide.dev), drawn at 24×24 in the current color. Their size
comes from the component they sit in.
-}

import Svg exposing (Svg)
import Svg.Attributes as A


icon : List (Svg msg) -> Svg msg
icon =
    Svg.svg
        [ A.viewBox "0 0 24 24"
        , A.fill "none"
        , A.stroke "currentColor"
        , A.strokeWidth "2"
        , A.strokeLinecap "round"
        , A.strokeLinejoin "round"
        ]


paths : List String -> Svg msg
paths ds =
    icon (List.map (\d -> Svg.path [ A.d d ] []) ds)


arrowUp : Svg msg
arrowUp =
    paths [ "m5 12 7-7 7 7", "M12 19V5" ]


check : Svg msg
check =
    paths [ "M20 6 9 17l-5-5" ]


chevronDown : Svg msg
chevronDown =
    paths [ "m6 9 6 6 6-6" ]


cloud : Svg msg
cloud =
    paths [ "M17.5 19H9a7 7 0 1 1 6.71-9h1.79a4.5 4.5 0 1 1 0 9Z" ]


copy : Svg msg
copy =
    icon
        [ Svg.rect [ A.x "8", A.y "8", A.width "14", A.height "14", A.rx "2" ] []
        , Svg.path [ A.d "M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2" ] []
        ]


monitor : Svg msg
monitor =
    icon
        [ Svg.rect [ A.x "2", A.y "3", A.width "20", A.height "14", A.rx "2" ] []
        , Svg.path [ A.d "M8 21h8" ] []
        , Svg.path [ A.d "M12 17v4" ] []
        ]


moon : Svg msg
moon =
    paths [ "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z" ]


newChat : Svg msg
newChat =
    paths
        [ "M12 3H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"
        , "M18.375 2.625a1 1 0 0 1 3 3l-9.013 9.014a2 2 0 0 1-.853.505l-2.873.84a.5.5 0 0 1-.62-.62l.84-2.873a2 2 0 0 1 .506-.852z"
        ]


panelLeft : Svg msg
panelLeft =
    icon
        [ Svg.rect [ A.x "3", A.y "3", A.width "18", A.height "18", A.rx "2" ] []
        , Svg.path [ A.d "M9 3v18" ] []
        ]


{-| A filled square, the stop sign of a reply in progress.
-}
square : Svg msg
square =
    Svg.svg [ A.viewBox "0 0 24 24", A.fill "currentColor" ]
        [ Svg.rect [ A.x "6", A.y "6", A.width "12", A.height "12", A.rx "2" ] [] ]


sun : Svg msg
sun =
    icon
        (Svg.circle [ A.cx "12", A.cy "12", A.r "4" ] []
            :: List.map (\d -> Svg.path [ A.d d ] [])
                [ "M12 2v2", "M12 20v2", "m4.93 4.93 1.41 1.41", "m17.66 17.66 1.41 1.41", "M2 12h2", "M20 12h2", "m6.34 17.66-1.41 1.41", "m19.07 4.93-1.41 1.41" ]
        )


thermometer : Svg msg
thermometer =
    paths [ "M14 4v10.54a4 4 0 1 1-4 0V4a2 2 0 0 1 4 0Z" ]


umbrella : Svg msg
umbrella =
    paths [ "M22 12a10.06 10.06 1 0 0-20 0Z", "M12 12v8a2 2 0 0 0 4 0", "M12 2v1" ]


wind : Svg msg
wind =
    paths [ "M12.8 19.6A2 2 0 1 0 14 16H2", "M17.5 8a2.5 2.5 0 1 1 2 4H2", "M9.8 4.4A2 2 0 1 1 11 8H2" ]
