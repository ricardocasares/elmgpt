module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, code, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import InteropDefinitions as Interop exposing (Theme)
import InteropPorts
import Json.Decode as Decode
import Process
import Task
import UI.Button as Button


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.map GotInterop InteropPorts.toElm
        }



-- MODEL


type alias Model =
    { theme : Theme
    , systemDark : Bool
    , copied : Bool
    , error : Maybe String
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        { theme, systemDark } =
            InteropPorts.decodeFlags flags
                |> Result.withDefault { theme = Nothing, systemDark = False }
    in
    ( { theme = Maybe.withDefault Interop.System theme
      , systemDark = systemDark
      , copied = False
      , error = Nothing
      }
    , Cmd.none
    )


isDark : Model -> Bool
isDark model =
    case model.theme of
        Interop.Light ->
            False

        Interop.Dark ->
            True

        Interop.System ->
            model.systemDark



-- UPDATE


type Msg
    = ThemeSelected Theme
    | CopyClicked
    | CopiedReset
    | GotInterop (Result Decode.Error Interop.ToElm)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Save the choice in localStorage, so the flags carry it back on the next load.
        ThemeSelected theme ->
            ( { model | theme = theme }
            , InteropPorts.fromElm
                (Interop.WriteToLocalStorage
                    { key = "theme"
                    , value = Interop.themeToString theme
                    }
                )
            )

        CopyClicked ->
            ( { model | copied = True }
            , Cmd.batch
                [ InteropPorts.fromElm (Interop.CopyToClipboard "bunx --bun shadcn@latest add ricardocasares/elmcn/button")
                , Task.perform (\_ -> CopiedReset) (Process.sleep 1500)
                ]
            )

        CopiedReset ->
            ( { model | copied = False }, Cmd.none )

        -- The browser tells us when the operating system switches between light and dark.
        GotInterop (Ok (Interop.SystemThemeChanged systemDark)) ->
            ( { model | systemDark = systemDark }, Cmd.none )

        GotInterop (Ok (Interop.LocalStorageError error)) ->
            ( { model | error = Just (error.key ++ ": " ++ error.message) }, Cmd.none )

        GotInterop (Ok (Interop.ClipboardError message)) ->
            ( { model | copied = False, error = Just message }, Cmd.none )

        GotInterop (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class
            (if isDark model then
                "dark"

             else
                ""
            )
        ]
        [ div [ class "flex min-h-screen flex-col items-center justify-center gap-6 bg-background p-6 text-foreground" ]
            [ h1 [ class "text-2xl font-semibold" ] [ text "elmcn" ]
            , div [ class "flex gap-2" ]
                [ themeButton model Interop.Light "Light"
                , themeButton model Interop.Dark "Dark"
                , themeButton model Interop.System "System"
                ]
            , p [ class "text-sm text-muted-foreground" ]
                [ text "Your choice is saved in localStorage." ]
            , Button.new
                |> Button.text
                    (if model.copied then
                        "Copied"

                     else
                        "Copy install command"
                    )
                |> Button.secondary
                |> Button.toHtml [ onClick CopyClicked ]
            , case model.error of
                Just message ->
                    code [ class "text-sm text-destructive" ] [ text message ]

                Nothing ->
                    text ""
            ]
        ]


themeButton : Model -> Theme -> String -> Html Msg
themeButton model theme label =
    Button.new
        |> Button.text label
        |> (if model.theme == theme then
                identity

            else
                Button.outline
           )
        |> Button.toHtml [ onClick (ThemeSelected theme) ]
