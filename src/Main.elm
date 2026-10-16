module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
import Html exposing (Attribute, Html, a, aside, div, h1, header, main_, nav, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, href, name, placeholder, rel, target, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import InteropDefinitions as Interop exposing (AbortController, Message, Role(..), Theme)
import InteropPorts
import Json.Decode as Decode
import Phosphor exposing (IconWeight(..))
import Process
import Task
import UI.Alert as Alert
import UI.DropdownMenu as DropdownMenu
import UI.Button as Button
import UI.InputGroup as InputGroup
import UI.Item as Item
import UI.MessageScroller as MessageScroller
import UI.Progress as Progress
import UI.Spinner as Spinner
import UI.Tabs as Tabs


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.map GotInterop InteropPorts.toElm
        }


{-| The system prompt. It is part of the conversation, so it lives here with the rest of it.
-}
instructions : String
instructions =
    "You are a weather assistant."



-- MODEL


type alias Model =
    { theme : Theme
    , systemDark : Bool
    , status : Status
    , messages : List Message
    , draft : String
    , request : Request
    , notice : Maybe String
    , copied : Maybe Int
    , holding : Bool
    , edges : { start : Bool, end : Bool }
    , sidebarOpen : Bool
    , recents : List Conversation
    }


{-| A conversation put aside by a new chat, kept until the page is reloaded.
-}
type alias Conversation =
    { id : Int
    , messages : List Message
    }


{-| Where the browser's built-in model is on its way to answering.
-}
type Status
    = Checking
    | Unsupported
    | Unavailable
    | NeedsDownload
    | Downloading Float
    | Loading
    | Ready
    | Failed String


{-| The reply being produced, if any. `Starting` waits for the abort controller that the reply is
requested with.
-}
type Request
    = Idle
    | Starting
    | Running Reply


type alias Reply =
    { controller : AbortController
    , text : String
    , stopping : Bool
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
      , status = Checking
      , messages = []
      , draft = ""
      , request = Idle
      , notice = Nothing
      , copied = Nothing
      , holding = False
      , edges = { start = False, end = False }
      , sidebarOpen = True
      , recents = []
      }
    , InteropPorts.fromElm Interop.CheckAvailability
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


canSend : Model -> Bool
canSend model =
    model.status == Ready && not (isRunning model.request) && not (String.isEmpty (String.trim model.draft))


isRunning : Request -> Bool
isRunning request =
    case request of
        Idle ->
            False

        _ ->
            True



-- UPDATE


type Msg
    = ThemeSelected Theme
    | DraftChanged String
    | SendClicked
    | SuggestionClicked String
    | NewChatClicked
    | RecentClicked Int
    | SidebarToggled
    | StopClicked
    | DownloadClicked
    | RetryClicked
    | NoticeDismissed
    | CopyClicked Int String
    | CopiedReset
    | Scrolled { start : Bool, end : Bool }
    | ScrollToEndClicked
    | Measured (Result Browser.Dom.Error Browser.Dom.Viewport)
    | NoOp
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

        DraftChanged draft ->
            ( { model | draft = draft }, Cmd.none )

        -- The turn is sent once the abort controller for its reply arrives, see `AbortControllerCreated`.
        SendClicked ->
            if canSend model then
                ( { model
                    | messages = model.messages ++ [ { role = User, content = String.trim model.draft } ]
                    , draft = ""
                    , request = Starting
                    , notice = Nothing
                    , holding = True
                  }
                , Cmd.batch
                    [ InteropPorts.fromElm Interop.CreateAbortController
                    , scroll MessageScroller.Instant
                    ]
                )

            else
                ( model, Cmd.none )

        SuggestionClicked suggestion ->
            update SendClicked { model | draft = suggestion }

        NewChatClicked ->
            if isRunning model.request then
                ( model, Cmd.none )

            else
                ( open [] (shelve model), Cmd.none )

        RecentClicked id ->
            if isRunning model.request then
                ( model, Cmd.none )

            else
                let
                    shelved =
                        shelve model
                in
                case List.filter (\conversation -> conversation.id == id) shelved.recents of
                    conversation :: _ ->
                        ( open conversation.messages
                            { shelved | recents = List.filter (\other -> other.id /= id) shelved.recents }
                        , scroll MessageScroller.Instant
                        )

                    [] ->
                        ( model, Cmd.none )

        SidebarToggled ->
            ( { model | sidebarOpen = not model.sidebarOpen }, Cmd.none )

        -- The reply ends with the error that aborting raises, which `finish` then keeps quiet about.
        StopClicked ->
            case model.request of
                Running reply ->
                    ( { model | request = Running { reply | stopping = True } }
                    , InteropPorts.fromElm (Interop.Abort reply.controller)
                    )

                _ ->
                    ( model, Cmd.none )

        DownloadClicked ->
            ( { model | status = Downloading 0 }, InteropPorts.fromElm Interop.CreateSession )

        RetryClicked ->
            ( { model | status = Checking }, InteropPorts.fromElm Interop.CheckAvailability )

        NoticeDismissed ->
            ( { model | notice = Nothing }, Cmd.none )

        CopyClicked index content ->
            ( { model | copied = Just index }
            , Cmd.batch
                [ InteropPorts.fromElm (Interop.CopyToClipboard content)
                , Task.perform (\_ -> CopiedReset) (Process.sleep 1500)
                ]
            )

        CopiedReset ->
            ( { model | copied = Nothing }, Cmd.none )

        Scrolled edges ->
            ( { model | edges = edges }, Cmd.none )

        ScrollToEndClicked ->
            ( model, scroll MessageScroller.Smooth )

        -- A reply grew: follow the end where the browser doesn't, if the reader was there.
        Measured (Ok { scene, viewport }) ->
            ( model
            , if not model.edges.end && scene.height - viewport.y - viewport.height > 4 then
                scroll MessageScroller.Instant

              else
                Cmd.none
            )

        Measured (Err _) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GotInterop (Ok event) ->
            fromInterop event model

        GotInterop (Err _) ->
            ( model, Cmd.none )


fromInterop : Interop.ToElm -> Model -> ( Model, Cmd Msg )
fromInterop event model =
    case event of
        -- The browser tells us when the operating system switches between light and dark.
        Interop.SystemThemeChanged systemDark ->
            ( { model | systemDark = systemDark }, Cmd.none )

        -- Nothing asks for a read yet: the theme comes with the flags.
        Interop.LocalStorageRead _ ->
            ( model, Cmd.none )

        Interop.LocalStorageError { key, message } ->
            ( { model | notice = Just (key ++ ": " ++ message) }, Cmd.none )

        Interop.ClipboardError message ->
            ( { model | copied = Nothing, notice = Just message }, Cmd.none )

        Interop.AvailabilityChecked availability ->
            case availability of
                Interop.Unsupported ->
                    ( { model | status = Unsupported }, Cmd.none )

                Interop.Unavailable ->
                    ( { model | status = Unavailable }, Cmd.none )

                -- Chrome only starts, or joins, a download on a user gesture, so these wait for a click.
                Interop.Downloadable ->
                    ( { model | status = NeedsDownload }, Cmd.none )

                Interop.Downloading ->
                    ( { model | status = NeedsDownload }, Cmd.none )

                -- A session now loads the model into memory, so the first reply doesn't wait for it.
                Interop.Available ->
                    ( { model | status = Loading }, InteropPorts.fromElm Interop.CreateSession )

        Interop.AvailabilityError message ->
            ( { model | status = Failed message }, Cmd.none )

        Interop.DownloadProgress progress ->
            ( { model | status = Downloading progress }, Cmd.none )

        Interop.SessionCreated ->
            ( { model | status = Ready }, Cmd.none )

        Interop.SessionError message ->
            ( { model | status = Failed message }, Cmd.none )

        -- The controller the turn was waiting for: the reply is requested with the whole conversation.
        Interop.AbortControllerCreated controller ->
            case model.request of
                Starting ->
                    let
                        request =
                            { instructions = instructions
                            , messages = model.messages
                            , controller = controller
                            }
                    in
                    ( { model | request = Running { controller = controller, text = "", stopping = False } }
                    , InteropPorts.fromElm (Interop.Stream request)
                    )

                _ ->
                    ( model, Cmd.none )

        Interop.Chunk chunk ->
            case model.request of
                Running reply ->
                    ( { model | request = Running { reply | text = reply.text ++ chunk } }, measure )

                _ ->
                    ( model, Cmd.none )

        Interop.Generated text ->
            finish { text = Just text, error = Nothing } model

        Interop.StreamEnded ->
            finish { text = Nothing, error = Nothing } model

        Interop.GenerationError message ->
            finish { text = Nothing, error = Just message } model

        Interop.StreamError message ->
            finish { text = Nothing, error = Just message } model

        Interop.ContextOverflow ->
            ( { model | notice = Just "The conversation no longer fits the model's context window, so its oldest messages were left out of this reply." }
            , Cmd.none
            )


{-| Ends the reply in progress. Its text, or the whole text of a generated reply, becomes the
assistant's turn, unless there is none. An error is shown, unless the reply was being stopped: then
it is the abort, which is expected.
-}
finish : { text : Maybe String, error : Maybe String } -> Model -> ( Model, Cmd Msg )
finish outcome model =
    case model.request of
        Running reply ->
            let
                content =
                    Maybe.withDefault reply.text outcome.text
            in
            ( { model
                | request = Idle
                , messages =
                    if String.isEmpty content then
                        model.messages

                    else
                        model.messages ++ [ { role = Assistant, content = content } ]
                , notice =
                    case ( outcome.error, reply.stopping ) of
                        ( Just message, False ) ->
                            Just message

                        _ ->
                            model.notice
              }
            , measure
            )

        _ ->
            ( model, Cmd.none )


{-| Puts the current conversation at the top of the recents, unless it has no messages.
-}
shelve : Model -> Model
shelve model =
    if List.isEmpty model.messages then
        model

    else
        { model
            | recents =
                { id = 1 + (List.map .id model.recents |> List.maximum |> Maybe.withDefault 0)
                , messages = model.messages
                }
                    :: model.recents
        }


open : List Message -> Model -> Model
open messages model =
    { model | messages = messages, draft = "", notice = Nothing, holding = False, edges = { start = False, end = False } }


{-| A recent conversation is named after its first question.
-}
conversationTitle : Conversation -> String
conversationTitle conversation =
    conversation.messages
        |> List.filter (\message -> message.role == User)
        |> List.head
        |> Maybe.map .content
        |> Maybe.withDefault "Untitled"


chatId : String
chatId =
    "chat"


scroll : MessageScroller.Behavior -> Cmd Msg
scroll behavior =
    Task.attempt (\_ -> NoOp) (MessageScroller.scrollToEnd behavior chatId)


measure : Cmd Msg
measure =
    Task.attempt Measured (Browser.Dom.getViewportOf chatId)



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
        [ div [ class "flex h-dvh bg-background text-foreground" ]
            [ viewRail model
            , if model.sidebarOpen then
                viewSidebar model

              else
                text ""
            , main_ [ class "relative flex min-w-0 flex-1 flex-col" ]
                (viewHeader model
                    :: (if isEmpty model then
                            [ viewWelcome model ]

                        else
                            [ div [ class "min-h-0 flex-1" ] [ viewTranscript model ]
                            , div [ class "mx-auto flex w-full max-w-3xl flex-col gap-2 px-4 pb-3" ]
                                [ viewStatus model.status
                                , viewComposer model
                                , p [ class "text-center text-xs text-muted-foreground" ]
                                    [ text "Answers come from a model that runs in this browser. It can make mistakes." ]
                                ]
                            ]
                       )
                )
            ]
        ]


{-| A conversation that hasn't started: the composer waits in the middle of the page.
-}
isEmpty : Model -> Bool
isEmpty model =
    List.isEmpty model.messages && not (isRunning model.request)


{-| The narrow column of icons on the left, with the theme behind the avatar at the bottom.
-}
viewRail : Model -> Html Msg
viewRail model =
    nav [ class "hidden w-16 shrink-0 flex-col items-center gap-2 py-3 sm:flex" ]
        [ if model.sidebarOpen then
            text ""

          else
            sidebarToggle model
        , Button.new
            |> Button.icon (Phosphor.notePencil Regular |> Phosphor.toHtml [])
            |> Button.label "New chat"
            |> Button.ghost
            |> Button.large
            |> Button.toHtml
                [ onClick NewChatClicked
                , disabled (isRunning model.request)
                , class "text-sidebar-foreground [&_svg]:size-5!"
                ]
        , div [ class "mt-auto" ]
            [ DropdownMenu.new
                |> DropdownMenu.id "user-menu"
                |> DropdownMenu.trigger
                    (attribute "aria-label" "Settings"
                        :: class "text-sidebar-foreground [&_svg]:size-5!"
                        :: (Button.new
                                |> Button.ghost
                                |> Button.large
                                |> Button.toAttributes
                           )
                    )
                    [ Phosphor.slidersHorizontal Regular |> Phosphor.toHtml [] ]
                |> DropdownMenu.right
                |> DropdownMenu.alignEnd
                |> DropdownMenu.content
                    [ DropdownMenu.group
                        [ DropdownMenu.label [ text "Theme" ]
                        , themeItem model Interop.Light (Phosphor.sun Regular |> Phosphor.toHtml []) "Light"
                        , themeItem model Interop.Dark (Phosphor.moon Regular |> Phosphor.toHtml []) "Dark"
                        , themeItem model Interop.System (Phosphor.monitor Regular |> Phosphor.toHtml []) "System"
                        ]
                    , DropdownMenu.separator
                    , DropdownMenu.item [ onClick NewChatClicked ] [ Phosphor.notePencil Regular |> Phosphor.toHtml [], text "New chat" ]
                        |> (if isRunning model.request then
                                DropdownMenu.disabled

                            else
                                identity
                           )
                    ]
                |> DropdownMenu.toHtml []
            ]
        ]


sidebarToggle : Model -> Html Msg
sidebarToggle model =
    Button.new
        |> Button.icon (Phosphor.sidebarSimple Regular |> Phosphor.toHtml [])
        |> Button.label
            (if model.sidebarOpen then
                "Close sidebar"

             else
                "Open sidebar"
            )
        |> Button.ghost
        |> Button.large
        |> Button.toHtml [ onClick SidebarToggled, class "text-sidebar-foreground [&_svg]:size-5!" ]


{-| The panel beside the rail, with the conversations a new chat put aside.
-}
viewSidebar : Model -> Html Msg
viewSidebar model =
    aside [ class "my-2 hidden w-72 shrink-0 flex-col overflow-hidden rounded-2xl border bg-sidebar text-sidebar-foreground sm:flex dark:border-transparent" ]
        [ div [ class "flex items-center justify-between px-4 pt-3 pb-2" ]
            [ span [ class "text-lg font-medium" ] [ text "elmgpt" ]
            , sidebarToggle model
            ]
        , div [ class "px-2" ]
            [ Button.new
                |> Button.text "New chat"
                |> Button.startIcon (Phosphor.notePencil Regular |> Phosphor.toHtml [])
                |> Button.ghost
                |> Button.large
                |> Button.toHtml [ onClick NewChatClicked, disabled (isRunning model.request), class "w-full justify-start px-3! text-[0.95rem] hover:bg-sidebar-accent! [&_svg]:size-4.5!" ]
            ]
        , p [ class "px-5 pt-6 pb-2 text-sm text-muted-foreground" ] [ text "Recents" ]
        , div [ class "flex min-h-0 flex-1 flex-col gap-0.5 overflow-y-auto px-2 pb-3" ]
            (if List.isEmpty model.recents then
                [ p [ class "px-3 text-sm text-muted-foreground/70" ] [ text "Start a new chat to keep this one here." ] ]

             else
                List.map
                    (\conversation ->
                        Html.button
                            (type_ "button"
                                :: onClick (RecentClicked conversation.id)
                                :: disabled (isRunning model.request)
                                :: Html.Attributes.title (conversationTitle conversation)
                                :: class "w-full justify-start px-3! text-[0.95rem] font-normal hover:bg-sidebar-accent!"
                                :: (Button.new
                                        |> Button.ghost
                                        |> Button.large
                                        |> Button.toAttributes
                                   )
                            )
                            [ span [ class "truncate" ] [ text (conversationTitle conversation) ] ]
                    )
                    model.recents
            )
        ]


themeItem : Model -> Theme -> Html Msg -> String -> DropdownMenu.Item Msg
themeItem model theme glyph caption =
    DropdownMenu.radioItem (model.theme == theme) [ onClick (ThemeSelected theme) ] [ glyph, text caption ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "grid grid-cols-[1fr_auto_1fr] items-center px-3 py-2.5" ]
        [ h1
            [ class "px-2 text-lg font-medium"
            , class
                (if model.sidebarOpen then
                    "sm:invisible"

                 else
                    ""
                )
            ]
            [ text "elmgpt" ]
        , Tabs.new
            |> Tabs.id "mode"
            |> Tabs.onSelect (\_ -> NoOp)
            |> Tabs.value "chat"
            |> Tabs.tabs
                [ Tabs.tab "chat" [ text "Chat" ] []
                , Tabs.tab "work" [ text "Work" ] []
                    |> Tabs.disabled
                ]
            |> Tabs.toHtml
                [ class "[&_[role=tablist]]:h-10! [&_[role=tablist]]:rounded-full! [&_[role=tablist]]:p-1! [&_[role=tab]]:rounded-full! [&_[role=tab]]:px-5! [&_[role=tab]]:text-[0.95rem]!" ]
        , div [ class "flex justify-end" ]
            [ if isEmpty model then
                text ""

              else
                Button.new
                    |> Button.icon (Phosphor.notePencil Regular |> Phosphor.toHtml [])
                    |> Button.label "New chat"
                    |> Button.ghost
                    |> Button.large
                    |> Button.rounded
                    |> Button.toHtml [ onClick NewChatClicked, disabled (isRunning model.request), class "sm:hidden" ]
            ]
        ]


viewWelcome : Model -> Html Msg
viewWelcome model =
    div [ class "flex min-h-0 flex-1 flex-col items-center justify-center gap-6 overflow-y-auto px-4 pb-[12vh]" ]
        [ p [ class "text-center text-2xl font-normal sm:text-[1.75rem]" ] [ text "Hey there. Ready to check the sky?" ]
        , div [ class "flex w-full max-w-3xl flex-col gap-4" ]
            [ viewComposer model
            , viewStatus model.status
            , viewSuggestions (model.status == Ready)
            ]
        ]


suggestions : List ( Html Msg, String )
suggestions =
    [ ( Phosphor.umbrella Regular |> Phosphor.toHtml [], "Do I need an umbrella in London tomorrow?" )
    , ( Phosphor.thermometer Regular |> Phosphor.toHtml [], "What should I wear for 5°C and windy?" )
    , ( Phosphor.cloud Regular |> Phosphor.toHtml [], "Explain why clouds form in plain words" )
    , ( Phosphor.wind Regular |> Phosphor.toHtml [], "Is it a good day to go sailing with 20-knot winds?" )
    ]


viewSuggestions : Bool -> Html Msg
viewSuggestions enabled =
    div [ class "flex flex-col px-2" ]
        (List.map
            (\( glyph, suggestion ) ->
                Html.button
                    [ type_ "button"
                    , onClick (SuggestionClicked suggestion)
                    , disabled (not enabled)
                    , class "rounded-xl text-start transition-colors outline-none hover:bg-muted/60 focus-visible:ring-3 focus-visible:ring-ring/50 disabled:pointer-events-none disabled:opacity-50"
                    ]
                    [ Item.new
                        |> Item.icon glyph
                        |> Item.title [ span [ class "font-normal" ] [ text suggestion ] ]
                        |> Item.toHtml [ class "text-muted-foreground [&_svg]:size-5!" ]
                    ]
            )
            suggestions
        )


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Checking ->
            statusLine [ Spinner.view [ class "size-4" ], text "Checking the built-in model…" ]

        Unsupported ->
            statusLine
                [ text "This browser has no Prompt API. See "
                , a
                    [ href "https://github.com/webmachinelearning/prompt-api"
                    , target "_blank"
                    , rel "noreferrer"
                    , class "underline underline-offset-4"
                    ]
                    [ text "webmachinelearning/prompt-api" ]
                , text "."
                ]

        Unavailable ->
            statusLine [ text "The built-in model is not available on this device." ]

        NeedsDownload ->
            statusLine
                [ text "The model has to be downloaded before it can answer."
                , Button.new
                    |> Button.text "Download model"
                    |> Button.small
                    |> Button.rounded
                    |> Button.toHtml [ onClick DownloadClicked ]
                ]

        Downloading progress ->
            div [ class "px-4" ]
                [ Progress.new (progress * 100)
                    |> Progress.label [ text "Downloading the model" ]
                    |> Progress.formatValue (\percentage -> String.fromInt (round percentage) ++ "%")
                    |> Progress.toHtml []
                ]

        Loading ->
            statusLine [ Spinner.view [ class "size-4" ], text "Loading the model…" ]

        Ready ->
            text ""

        Failed message ->
            Alert.new
                |> Alert.destructive
                |> Alert.title [ text "The model could not be loaded" ]
                |> Alert.description [ text message ]
                |> Alert.action
                    (Button.new
                        |> Button.text "Retry"
                        |> Button.outline
                        |> Button.small
                        |> Button.toHtml [ onClick RetryClicked ]
                    )
                |> Alert.toHtml []


statusLine : List (Html Msg) -> Html Msg
statusLine children =
    div [ class "flex flex-wrap items-center justify-center gap-2 text-sm text-muted-foreground" ] children


viewTranscript : Model -> Html Msg
viewTranscript model =
    MessageScroller.new
        |> MessageScroller.id chatId
        |> MessageScroller.padding "calc(var(--spacing) * 6)"
        |> MessageScroller.busy (isRunning model.request)
        |> MessageScroller.items [ class "mx-auto w-full max-w-3xl gap-6 px-4" ]
            (List.indexedMap (viewMessage model.copied) model.messages ++ viewReply model.request)
        |> (if model.holding then
                MessageScroller.holdTurn

            else
                identity
           )
        |> MessageScroller.endButton ScrollToEndClicked
        |> MessageScroller.onScroll Scrolled
        |> MessageScroller.scrollable model.edges
        |> MessageScroller.toHtml []


viewMessage : Maybe Int -> Int -> Message -> MessageScroller.Item Msg
viewMessage copied index message =
    let
        id =
            "message-" ++ String.fromInt index
    in
    case message.role of
        User ->
            MessageScroller.item id
                [ class "flex justify-end" ]
                [ div [ class "max-w-[70%] rounded-3xl bg-muted px-4 py-2.5 whitespace-pre-wrap" ]
                    [ text message.content ]
                ]
                |> MessageScroller.anchor

        Assistant ->
            MessageScroller.item id
                [ class "flex flex-col items-start gap-1" ]
                [ p [ class "leading-7 whitespace-pre-wrap" ] [ text message.content ]
                , Button.new
                    |> Button.icon
                        (if copied == Just index then
                            Phosphor.check Regular |> Phosphor.toHtml []

                         else
                            Phosphor.copy Regular |> Phosphor.toHtml []
                        )
                    |> Button.label
                        (if copied == Just index then
                            "Copied"

                         else
                            "Copy"
                        )
                    |> Button.ghost
                    |> Button.toHtml [ onClick (CopyClicked index message.content), class "-ms-2 text-muted-foreground" ]
                ]


{-| The reply as it comes in, under the turn it answers. A pulsing dot stands in until the first words.
-}
viewReply : Request -> List (MessageScroller.Item Msg)
viewReply request =
    case request of
        Idle ->
            []

        Starting ->
            [ MessageScroller.item "reply" [] [ thinking ] ]

        Running reply ->
            [ MessageScroller.item "reply"
                []
                [ if String.isEmpty reply.text then
                    thinking

                  else
                    p [ class "leading-7 whitespace-pre-wrap" ] [ text reply.text ]
                ]
            ]


thinking : Html msg
thinking =
    span [ class "block size-3.5 animate-pulse rounded-full bg-foreground", attribute "aria-label" "Thinking" ] []


viewComposer : Model -> Html Msg
viewComposer model =
    div [ class "flex flex-col gap-3" ]
        [ case model.notice of
            Just message ->
                Alert.new
                    |> Alert.destructive
                    |> Alert.title [ text "Something went wrong" ]
                    |> Alert.description [ text message ]
                    |> Alert.action
                        (Button.new
                            |> Button.text "Dismiss"
                            |> Button.ghost
                            |> Button.extraSmall
                            |> Button.toHtml [ onClick NoticeDismissed ]
                        )
                    |> Alert.toHtml []

            Nothing ->
                text ""
        , InputGroup.view
            [ class "items-end! gap-2 rounded-[1.75rem]! border-transparent! bg-muted p-2.5 shadow-sm ring-0! dark:bg-muted!" ]
            [ InputGroup.textarea
                [ name "prompt"
                , placeholder "Ask elmgpt"
                , value model.draft
                , onInput DraftChanged
                , onEnter SendClicked
                , class "max-h-52 min-h-0! self-center px-0! py-1.5! ps-2.5! leading-6 md:text-base!"
                ]
            , InputGroup.end [ class "me-0! gap-1! py-0! pe-0!" ]
                [ viewSendButton model ]
            ]
        ]


viewSendButton : Model -> Html Msg
viewSendButton model =
    case model.request of
        Idle ->
            Button.new
                |> Button.icon (Phosphor.arrowUp Bold |> Phosphor.toHtml [])
                |> Button.label "Send"
                |> Button.large
                |> Button.rounded
                |> Button.toHtml [ onClick SendClicked, disabled (not (canSend model)), class "[&_svg]:size-5!" ]

        Starting ->
            stopButton [ disabled True ]

        Running reply ->
            stopButton [ onClick StopClicked, disabled reply.stopping ]


stopButton : List (Attribute Msg) -> Html Msg
stopButton attrs =
    Button.new
        |> Button.icon (Phosphor.stop Fill |> Phosphor.toHtml [])
        |> Button.label "Stop"
        |> Button.large
        |> Button.rounded
        |> Button.toHtml attrs


{-| Enter sends. Shift+Enter makes a new line, and Enter while composing with an input method only
confirms the composition.
-}
onEnter : msg -> Attribute msg
onEnter msg =
    preventDefaultOn "keydown"
        (Decode.map3 (\key shift composing -> ( key, shift, composing ))
            (Decode.field "key" Decode.string)
            (Decode.field "shiftKey" Decode.bool)
            (Decode.field "isComposing" Decode.bool)
            |> Decode.andThen
                (\( key, shift, composing ) ->
                    if key == "Enter" && not shift && not composing then
                        Decode.succeed ( msg, True )

                    else
                        Decode.fail "Not a plain Enter"
                )
        )
