module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
import File exposing (File)
import File.Select
import Html exposing (Attribute, Html, a, aside, audio, div, h1, header, img, main_, nav, p, span, text)
import Html.Attributes exposing (alt, attribute, class, controls, disabled, href, name, placeholder, rel, src, target, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import InteropDefinitions as Interop exposing (AbortController, Attachment, Context, Message, Recorder, Role(..), Theme)
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown.Parser
import Markdown.Renderer
import Phosphor exposing (IconWeight(..))
import Process
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Task
import UI.Alert as Alert
import UI.Attachment as AttachmentView
import UI.Button as Button
import UI.DropdownMenu as DropdownMenu
import UI.InputGroup as InputGroup
import UI.Item as Item
import UI.MessageScroller as MessageScroller
import UI.Progress as Progress
import UI.Separator as Separator
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
    , conversation : Int
    , nextConversation : Int
    , messages : List Turn
    , compacted : Int
    , compactionDue : Bool
    , draft : String
    , attachments : List Attachment
    , context : Maybe Context
    , recording : Recording
    , request : Request
    , notice : Maybe String
    , copied : Maybe Int
    , holding : Bool
    , edges : { start : Bool, end : Bool }
    , sidebarOpen : Bool
    , recents : List Conversation
    , expanded : Set String
    }


{-| A conversation put aside by a new chat, kept until the page is reloaded.
-}
type alias Conversation =
    { id : Int
    , messages : List Turn
    , compacted : Int
    , context : Maybe Context
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


{-| A voice message on its way into the composer. `Asking` waits for the microphone, and `Finishing`
for the clip of a recorder that was stopped.
-}
type Recording
    = NotRecording
    | Asking
    | Recording Recorder
    | Finishing


{-| The reply being produced, if any. `Starting` waits for the abort controller that the reply is
requested with.
-}
type Request
    = Idle
    | Starting
    | Running Reply


type alias Reply =
    { controller : AbortController
    , parts : List Part
    , stopping : Bool
    }


{-| A piece of a reply, in the order the model produced it.
-}
type Part
    = Text String
    | Reasoning { id : String, text : String, done : Bool }
    | Tool ToolCall


{-| A call the model made to a tool. `input` waits for the call to be parsed.
-}
type alias ToolCall =
    { id : String
    , name : String
    , input : Maybe Decode.Value
    , state : ToolState
    }


{-| Where a tool call is. `Errored` is a call that could not be parsed or run, with the reason.
-}
type ToolState
    = Calling
    | Succeeded Decode.Value
    | Errored String


{-| A turn of the conversation as the app holds it: the message the model sees, and the parts of a
reply (reasoning and tool calls) that are shown but not sent back.
-}
type alias Turn =
    { role : Role
    , content : String
    , attachments : List Attachment
    , parts : List Part
    }


{-| The message the model is given for a turn.
-}
toMessage : Turn -> Message
toMessage turn =
    { role = turn.role, content = turn.content, attachments = turn.attachments }


{-| What the model said, without the reasoning and tool calls. Text before and after a part that
interrupted it are separate paragraphs.
-}
replyText : List Part -> String
replyText parts =
    parts
        |> List.filterMap
            (\part ->
                case part of
                    Text text ->
                        Just text

                    _ ->
                        Nothing
            )
        |> String.join "\n\n"


isText : Part -> Bool
isText part =
    case part of
        Text _ ->
            True

        _ ->
            False


{-| A text chunk goes on the text it continues, or starts a new one.
-}
appendText : String -> List Part -> List Part
appendText chunk parts =
    case List.reverse parts of
        (Text existing) :: before ->
            List.reverse (Text (existing ++ chunk) :: before)

        _ ->
            parts ++ [ Text chunk ]


{-| A reasoning chunk goes on the reasoning it continues, if that is the last part and still going.
-}
appendReasoning : String -> String -> List Part -> List Part
appendReasoning id chunk parts =
    case List.reverse parts of
        (Reasoning reasoning) :: before ->
            if reasoning.id == id && not reasoning.done then
                List.reverse (Reasoning { reasoning | text = reasoning.text ++ chunk } :: before)

            else
                parts ++ [ Reasoning { id = id, text = chunk, done = False } ]

        _ ->
            parts ++ [ Reasoning { id = id, text = chunk, done = False } ]


endReasoning : String -> List Part -> List Part
endReasoning id =
    List.map
        (\part ->
            case part of
                Reasoning reasoning ->
                    if reasoning.id == id then
                        Reasoning { reasoning | done = True }

                    else
                        part

                _ ->
                    part
        )


{-| Changes the tool call with this id, or adds it first, since an event can come for a call that was
never started.
-}
upsertTool : String -> String -> (ToolCall -> ToolCall) -> List Part -> List Part
upsertTool id name change parts =
    if List.any (isTool id) parts then
        List.map
            (\part ->
                case part of
                    Tool call ->
                        if call.id == id then
                            Tool (change call)

                        else
                            part

                    _ ->
                        part
            )
            parts

    else
        parts ++ [ Tool (change { id = id, name = name, input = Nothing, state = Calling }) ]


isTool : String -> Part -> Bool
isTool id part =
    case part of
        Tool call ->
            call.id == id

        _ ->
            False


{-| A reply that ended can no longer be going on: its reasoning is done, and a call that never got
an answer was interrupted.
-}
settle : List Part -> List Part
settle =
    List.map
        (\part ->
            case part of
                Reasoning reasoning ->
                    Reasoning { reasoning | done = True }

                Tool call ->
                    case call.state of
                        Calling ->
                            Tool { call | state = Errored "Interrupted" }

                        _ ->
                            part

                Text _ ->
                    part
        )


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        { theme, systemDark, now } =
            InteropPorts.decodeFlags flags
                |> Result.withDefault { theme = Nothing, systemDark = False, now = 0 }
    in
    ( { theme = Maybe.withDefault Interop.System theme
      , systemDark = systemDark
      , status = Checking
      , conversation = now
      , nextConversation = now + 1
      , messages = []
      , compacted = 0
      , compactionDue = False
      , draft = ""
      , attachments = []
      , context = Nothing
      , recording = NotRecording
      , request = Idle
      , notice = Nothing
      , copied = Nothing
      , holding = False
      , edges = { start = False, end = False }
      , sidebarOpen = True
      , recents = []
      , expanded = Set.empty
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
    model.status
        == Ready
        && not (isRunning model.request)
        && (model.recording == NotRecording)
        && not (String.isEmpty (String.trim model.draft) && List.isEmpty model.attachments)


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
    | AttachClicked
    | FilesSelected File (List File)
    | FileRead Attachment
    | AttachmentRemoved Int
    | RecordClicked
    | RecordStopClicked
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
    | PartToggled String
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
                    | messages = model.messages ++ [ { role = User, content = String.trim model.draft, attachments = model.attachments, parts = [] } ]
                    , draft = ""
                    , attachments = []
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

        AttachClicked ->
            ( model, File.Select.files [ "image/*", "audio/*" ] FilesSelected )

        -- The picker only offers images and audio, but a drop of another kind of file can still get here.
        FilesSelected file files ->
            let
                ( readable, others ) =
                    List.partition (File.mime >> isReadable) (file :: files)
            in
            ( { model
                | notice =
                    if List.isEmpty others then
                        model.notice

                    else
                        Just ("The model reads images and audio only, so these were left out: " ++ String.join ", " (List.map File.name others) ++ ".")
              }
            , Cmd.batch (List.map read readable)
            )

        FileRead attachment ->
            ( { model | attachments = model.attachments ++ [ attachment ] }, Cmd.none )

        AttachmentRemoved index ->
            ( { model | attachments = List.take index model.attachments ++ List.drop (index + 1) model.attachments }, Cmd.none )

        RecordClicked ->
            if model.recording == NotRecording then
                ( { model | recording = Asking }, InteropPorts.fromElm Interop.StartRecording )

            else
                ( model, Cmd.none )

        -- The clip arrives as `Recorded` once the recorder has stopped.
        RecordStopClicked ->
            case model.recording of
                Recording recorder ->
                    ( { model | recording = Finishing }, InteropPorts.fromElm (Interop.StopRecording recorder) )

                _ ->
                    ( model, Cmd.none )

        SuggestionClicked suggestion ->
            update SendClicked { model | draft = suggestion }

        NewChatClicked ->
            if isRunning model.request then
                ( model, Cmd.none )

            else
                let
                    shelved =
                        shelve model
                in
                ( open
                    { id = shelved.nextConversation, messages = [], compacted = 0, context = Nothing }
                    { shelved | nextConversation = shelved.nextConversation + 1 }
                , Cmd.none
                )

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
                        ( open conversation
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

        PartToggled key ->
            ( { model
                | expanded =
                    if Set.member key model.expanded then
                        Set.remove key model.expanded

                    else
                        Set.insert key model.expanded
              }
            , Cmd.none
            )

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
                            , messages = inContext model
                            , controller = controller
                            }
                    in
                    ( { model | request = Running { controller = controller, parts = [], stopping = False } }
                    , InteropPorts.fromElm (Interop.Stream request)
                    )

                _ ->
                    ( model, Cmd.none )

        Interop.Chunk chunk ->
            updateParts (appendText chunk) model

        Interop.ReasoningChunk reasoning ->
            updateParts (appendReasoning reasoning.id reasoning.text) model

        Interop.ReasoningEnded id ->
            updateParts (endReasoning id) model

        Interop.ToolStarted tool ->
            updateParts (upsertTool tool.id tool.name identity) model

        Interop.ToolCalled tool ->
            updateParts (upsertTool tool.id tool.name (\call -> { call | input = Just tool.input })) model

        Interop.ToolSucceeded tool ->
            updateParts (upsertTool tool.id tool.name (\call -> { call | state = Succeeded tool.output })) model

        Interop.ToolFailed tool ->
            updateParts (upsertTool tool.id tool.name (\call -> { call | state = Errored tool.error })) model

        Interop.Generated text ->
            finish { text = Just text, error = Nothing } model

        Interop.StreamEnded ->
            finish { text = Nothing, error = Nothing } model

        Interop.GenerationError message ->
            finish { text = Nothing, error = Just message } model

        Interop.StreamError message ->
            finish { text = Nothing, error = Just message } model

        Interop.RecordingStarted recorder ->
            ( { model | recording = Recording recorder }, Cmd.none )

        Interop.Recorded attachment ->
            ( { model | recording = NotRecording, attachments = model.attachments ++ [ attachment ] }, Cmd.none )

        Interop.RecordingError message ->
            ( { model | recording = NotRecording, notice = Just message }, Cmd.none )

        -- Near the end of the window, the oldest messages leave the context before the next turn needs
        -- the room. The reply is measured before it ends, so `finish` compacts once it is in the conversation.
        Interop.ContextMeasured context ->
            ( { model
                | context = Just context
                , compactionDue = context.window > 0 && context.used / context.window >= compactAt
              }
            , Cmd.none
            )

        -- The conversation outgrew the window before it was measured near the end: it is compacted, and
        -- the turn asked again. Once only the last messages are left, there is nothing more to leave out.
        Interop.InputTooLarge ->
            case model.request of
                Running reply ->
                    if reply.stopping then
                        finish { text = Nothing, error = Nothing } model

                    else if List.length (inContext model) > keepInContext then
                        let
                            ( compacted, archived ) =
                                compact { model | request = Starting }
                        in
                        ( compacted
                        , Cmd.batch [ archived, InteropPorts.fromElm Interop.CreateAbortController ]
                        )

                    else
                        finish
                            { text = Nothing
                            , error =
                                Just
                                    ("This message doesn't fit the model's context window, even with only the last "
                                        ++ String.fromInt keepInContext
                                        ++ " messages of the conversation. Try a shorter message, fewer attachments or a new chat."
                                    )
                            }
                            { model | request = Running { reply | parts = [] } }

                _ ->
                    ( model, Cmd.none )

        Interop.ContextOverflow ->
            ( { model | notice = Just "The conversation no longer fits the model's context window, so its oldest messages were left out of this reply." }
            , Cmd.none
            )


{-| A reply grew: its parts change, and the transcript follows the end if the reader was there.
-}
updateParts : (List Part -> List Part) -> Model -> ( Model, Cmd Msg )
updateParts change model =
    case model.request of
        Running reply ->
            ( { model | request = Running { reply | parts = change reply.parts } }, measure )

        _ ->
            ( model, Cmd.none )


{-| Ends the reply in progress. Its parts, or the whole text of a generated reply, become the
assistant's turn, unless there are none. An error is shown, unless the reply was being stopped: then
it is the abort, which is expected.
-}
finish : { text : Maybe String, error : Maybe String } -> Model -> ( Model, Cmd Msg )
finish outcome model =
    case model.request of
        Running reply ->
            let
                content =
                    Maybe.withDefault (replyText reply.parts) outcome.text

                -- A generated reply has its text whole, in place of any text that streamed.
                parts =
                    case outcome.text of
                        Just generated ->
                            settle (List.filter (isText >> not) reply.parts) ++ [ Text generated ]

                        Nothing ->
                            settle reply.parts

                finished =
                    { model
                        | request = Idle
                        , messages =
                            if String.isEmpty content && List.isEmpty parts then
                                model.messages

                            else
                                model.messages ++ [ { role = Assistant, content = content, attachments = [], parts = parts } ]
                        , notice =
                            case ( outcome.error, reply.stopping ) of
                                ( Just message, False ) ->
                                    Just message

                                _ ->
                                    model.notice
                        , compactionDue = False
                    }

                ( compacted, archived ) =
                    if model.compactionDue then
                        compact finished

                    else
                        ( finished, Cmd.none )
            in
            ( compacted, Cmd.batch [ measure, archived ] )

        _ ->
            ( model, Cmd.none )


isReadable : String -> Bool
isReadable mediaType =
    isImage mediaType || String.startsWith "audio/" mediaType


isImage : String -> Bool
isImage =
    String.startsWith "image/"


{-| Reads a picked file into a `data:` URL, which is what the attachment carries to the model.
-}
read : File -> Cmd Msg
read file =
    File.toUrl file
        |> Task.perform (\url -> FileRead { name = File.name file, mediaType = File.mime file, url = url })


{-| How many of the latest messages the model is given once a conversation has been compacted, and
the share of the context window that compacts it. See TODO.md for compaction that keeps more.
-}
keepInContext : Int
keepInContext =
    10


compactAt : Float
compactAt =
    0.8


{-| The part of the conversation the model is given: what comes after the compacted messages.
-}
inContext : Model -> List Message
inContext model =
    List.drop model.compacted model.messages
        |> List.map toMessage


{-| Leaves all but the last `keepInContext` messages out of the context. They stay in the transcript,
and are saved in localStorage under the conversation's id, ready for a memory to read them back.
-}
compact : Model -> ( Model, Cmd Msg )
compact model =
    let
        upTo =
            max model.compacted (List.length model.messages - keepInContext)
    in
    if upTo > model.compacted then
        ( { model | compacted = upTo }
        , InteropPorts.fromElm
            (Interop.WriteToLocalStorage
                { key = "archive:" ++ String.fromInt model.conversation
                , value = Encode.encode 0 (Encode.list encodeArchived (List.take upTo model.messages))
                }
            )
        )

    else
        ( model, Cmd.none )


{-| An archived message keeps its attachments' names and types, not their data, which would soon
fill localStorage.
-}
encodeArchived : Turn -> Encode.Value
encodeArchived message =
    Encode.object
        [ ( "role"
          , Encode.string
                (case message.role of
                    User ->
                        "user"

                    Assistant ->
                        "assistant"
                )
          )
        , ( "content", Encode.string message.content )
        , ( "attachments"
          , Encode.list
                (\attachment -> Encode.object [ ( "name", Encode.string attachment.name ), ( "mediaType", Encode.string attachment.mediaType ) ])
                message.attachments
          )
        ]


{-| Puts the current conversation at the top of the recents, unless it has no messages.
-}
shelve : Model -> Model
shelve model =
    if List.isEmpty model.messages then
        model

    else
        { model
            | recents =
                { id = model.conversation
                , messages = model.messages
                , compacted = model.compacted
                , context = model.context
                }
                    :: model.recents
        }


open : Conversation -> Model -> Model
open conversation model =
    { model
        | conversation = conversation.id
        , messages = conversation.messages
        , compacted = conversation.compacted
        , context = conversation.context
        , draft = ""
        , attachments = []
        , notice = Nothing
        , holding = False
        , edges = { start = False, end = False }
        , expanded = Set.empty
    }


{-| A recent conversation is named after its first question.
-}
conversationTitle : Conversation -> String
conversationTitle conversation =
    case List.filter (\message -> message.role == User) conversation.messages of
        first :: _ ->
            if String.isEmpty first.content then
                case first.attachments of
                    attachment :: _ ->
                        attachmentKind attachment

                    [] ->
                        "Untitled"

            else
                first.content

        [] ->
            "Untitled"


attachmentKind : Attachment -> String
attachmentKind attachment =
    if isImage attachment.mediaType then
        "Image"

    else
        "Audio"


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
                                , div [ class "grid grid-cols-[1fr_auto_1fr] items-center gap-2 px-1" ]
                                    [ span [] []
                                    , p [ class "text-center text-xs text-muted-foreground" ]
                                        [ text "Answers come from a model that runs in this browser. It can make mistakes." ]
                                    , div [ class "flex justify-end" ]
                                        [ Maybe.map viewContext model.context |> Maybe.withDefault (text "") ]
                                    ]
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
            (List.concat (List.indexedMap (viewTurn model) model.messages) ++ viewReply model.expanded model.request)
        |> (if model.holding then
                MessageScroller.holdTurn

            else
                identity
           )
        |> MessageScroller.endButton ScrollToEndClicked
        |> MessageScroller.onScroll Scrolled
        |> MessageScroller.scrollable model.edges
        |> MessageScroller.toHtml []


{-| A message, after the line where the compacted part of the conversation ends.
-}
viewTurn : Model -> Int -> Turn -> List (MessageScroller.Item Msg)
viewTurn model index message =
    if index == model.compacted && index > 0 then
        [ viewCompacted, viewMessage model.copied model.expanded index message ]

    else
        [ viewMessage model.copied model.expanded index message ]


viewCompacted : MessageScroller.Item Msg
viewCompacted =
    MessageScroller.item "compacted"
        [ class "flex items-center gap-3 text-xs text-muted-foreground" ]
        [ Separator.new |> Separator.toHtml [ class "flex-1" ]
        , span [ class "text-center" ] [ text "The model no longer sees the messages above. They are saved in this browser." ]
        , Separator.new |> Separator.toHtml [ class "flex-1" ]
        ]


viewMessage : Maybe Int -> Set String -> Int -> Turn -> MessageScroller.Item Msg
viewMessage copied expanded index message =
    let
        id =
            "message-" ++ String.fromInt index
    in
    case message.role of
        User ->
            MessageScroller.item id
                [ class "flex flex-col items-end gap-2" ]
                (List.map viewSentAttachment message.attachments
                    ++ (if String.isEmpty message.content then
                            []

                        else
                            [ div [ class "max-w-[70%] rounded-3xl bg-muted px-4 py-2.5 whitespace-pre-wrap" ]
                                [ text message.content ]
                            ]
                       )
                )
                |> MessageScroller.anchor

        Assistant ->
            MessageScroller.item id
                [ class "flex flex-col items-start gap-1" ]
                (viewParts expanded ("message-" ++ String.fromInt index ++ "-") message.parts
                    ++ [ Button.new
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
                )


{-| A reply, formatted from its markdown. A reply still coming in can stop in the middle of a block,
and until the parser makes sense of it again the text is shown as it is.
-}
viewMarkdown : String -> Html msg
viewMarkdown source =
    case
        Markdown.Parser.parse source
            |> Result.mapError (\_ -> ())
            |> Result.andThen (Markdown.Renderer.render markdownRenderer >> Result.mapError (\_ -> ()))
    of
        Ok blocks ->
            div [ class "markdown w-full" ] blocks

        Err () ->
            p [ class "leading-7 whitespace-pre-wrap" ] [ text source ]


{-| Links open beside the conversation, not in its place.
-}
markdownRenderer : Markdown.Renderer.Renderer (Html msg)
markdownRenderer =
    let
        default =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { default
        | link =
            \link content ->
                a
                    (href link.destination
                        :: target "_blank"
                        :: rel "noreferrer"
                        :: (link.title |> Maybe.map (Html.Attributes.title >> List.singleton) |> Maybe.withDefault [])
                    )
                    content
    }


viewSentAttachment : Attachment -> Html msg
viewSentAttachment attachment =
    if isImage attachment.mediaType then
        img [ src attachment.url, alt attachment.name, class "max-h-72 max-w-[70%] rounded-3xl object-cover" ] []

    else
        audio [ src attachment.url, controls True, class "w-72 max-w-[70%]" ] []


{-| The reply as it comes in, under the turn it answers. A pulsing dot stands in until the first part.
-}
viewReply : Set String -> Request -> List (MessageScroller.Item Msg)
viewReply expanded request =
    case request of
        Idle ->
            []

        Starting ->
            [ MessageScroller.item "reply" [] [ thinking ] ]

        Running reply ->
            [ MessageScroller.item "reply"
                []
                (if List.isEmpty reply.parts then
                    [ thinking ]

                 else
                    viewParts expanded "reply-" reply.parts
                )
            ]


{-| The parts of a reply, in order. Each part that can be opened is keyed by the prefix and its id, in
`expanded`.
-}
viewParts : Set String -> String -> List Part -> List (Html Msg)
viewParts expanded keyPrefix parts =
    List.map
        (\part ->
            case part of
                Text source ->
                    viewMarkdown source

                Reasoning reasoning ->
                    viewReasoning (Set.member (keyPrefix ++ reasoning.id) expanded) (keyPrefix ++ reasoning.id) reasoning

                Tool call ->
                    viewTool (Set.member (keyPrefix ++ call.id) expanded) (keyPrefix ++ call.id) call
        )
        parts


{-| What the model thought, as a line that opens to the whole of it.
-}
viewReasoning : Bool -> String -> { id : String, text : String, done : Bool } -> Html Msg
viewReasoning isOpen key reasoning =
    Item.new
        |> Item.icon (Phosphor.brain Regular |> Phosphor.toHtml [])
        |> Item.title
            [ text
                (if reasoning.done then
                    "Reasoning"

                 else
                    "Thinking…"
                )
            ]
        |> (if isOpen || String.isEmpty reasoning.text then
                identity

            else
                Item.description [ text reasoning.text ]
           )
        |> Item.actions
            ((if reasoning.done then
                []

              else
                [ Spinner.view [ class "size-4 text-muted-foreground" ] ]
             )
                ++ (if String.isEmpty reasoning.text then
                        []

                    else
                        [ toggleButton key isOpen "Show reasoning" "Hide reasoning" ]
                   )
            )
        |> (if isOpen then
                Item.footer [ div [ class "w-full text-sm text-muted-foreground" ] [ viewMarkdown reasoning.text ] ]

            else
                identity
           )
        |> Item.muted
        |> Item.small
        |> Item.toHtml [ class "my-1" ]


{-| A call to a tool, with how it went and, when opened, what went in and came out.
-}
viewTool : Bool -> String -> ToolCall -> Html Msg
viewTool isOpen key call =
    let
        geolocation =
            call.name == "geolocation"

        succeeded =
            case call.state of
                Succeeded _ ->
                    True

                _ ->
                    False

        hasInput =
            case call.input of
                Just _ ->
                    True

                Nothing ->
                    False

        failed =
            case call.state of
                Errored _ ->
                    True

                _ ->
                    False
    in
    Item.new
        |> Item.icon (toolGlyph call.name)
        |> Item.title [ text (toolLabel call.name) ]
        |> Item.description
            (case call.state of
                Calling ->
                    [ text
                        (if geolocation then
                            "Asking the browser…"

                         else
                            "Running…"
                        )
                    ]

                Succeeded _ ->
                    [ text
                        (if geolocation then
                            "Found your location"

                         else
                            "Done"
                        )
                    ]

                Errored message ->
                    [ span [ class "text-destructive" ] [ text message ] ]
            )
        |> Item.actions
            ([ case call.state of
                Calling ->
                    Spinner.view [ class "size-4 text-muted-foreground" ]

                Succeeded _ ->
                    span [ class "text-muted-foreground" ] [ Phosphor.checkCircle Regular |> Phosphor.toHtml [] ]

                Errored _ ->
                    span [ class "text-destructive" ] [ Phosphor.warningCircle Regular |> Phosphor.toHtml [] ]
             ]
                ++ (if hasInput || succeeded || failed then
                        [ toggleButton key isOpen "Show details" "Hide details" ]

                    else
                        []
                   )
            )
        |> (if isOpen then
                Item.footer
                    [ div [ class "flex w-full flex-col gap-2" ]
                        (List.concat
                            [ case call.input of
                                Just input ->
                                    viewJson "Input" input

                                Nothing ->
                                    []
                            , case call.state of
                                Succeeded output ->
                                    viewJson "Output" output

                                Errored message ->
                                    viewDetail "Error" message

                                Calling ->
                                    []
                            ]
                        )
                    ]

            else
                identity
           )
        |> Item.outline
        |> Item.small
        |> Item.toHtml
            [ class "my-1"
            , class
                (if failed then
                    "border-destructive/40!"

                 else
                    ""
                )
            ]


viewJson : String -> Decode.Value -> List (Html msg)
viewJson caption value =
    viewDetail caption (Encode.encode 2 value)


{-| A captioned block of a tool call's details, as it is.
-}
viewDetail : String -> String -> List (Html msg)
viewDetail caption content =
    [ span [ class "text-xs font-medium text-muted-foreground" ] [ text caption ]
    , Html.pre [ class "overflow-x-auto rounded-md bg-muted p-2 text-xs font-mono whitespace-pre-wrap" ] [ text content ]
    ]


{-| The caret that opens or closes a part.
-}
toggleButton : String -> Bool -> String -> String -> Html Msg
toggleButton key isOpen showLabel hideLabel =
    Button.new
        |> Button.icon
            (if isOpen then
                Phosphor.caretUp Regular |> Phosphor.toHtml []

             else
                Phosphor.caretDown Regular |> Phosphor.toHtml []
            )
        |> Button.label
            (if isOpen then
                hideLabel

             else
                showLabel
            )
        |> Button.ghost
        |> Button.extraSmall
        |> Button.toHtml
            [ onClick (PartToggled key)
            , attribute "aria-expanded"
                (if isOpen then
                    "true"

                 else
                    "false"
                )
            ]


toolGlyph : String -> Html msg
toolGlyph name =
    case name of
        "geolocation" ->
            Phosphor.mapPin Regular |> Phosphor.toHtml []

        _ ->
            Phosphor.wrench Regular |> Phosphor.toHtml []


toolLabel : String -> String
toolLabel name =
    case name of
        "geolocation" ->
            "Your location"

        _ ->
            name


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
        , div
            [ class "flex flex-col bg-muted p-2.5 shadow-sm"
            , class
                (if List.isEmpty model.attachments then
                    "rounded-[1.75rem]"

                 else
                    "gap-2 rounded-3xl"
                )
            ]
            [ if List.isEmpty model.attachments then
                text ""

              else
                AttachmentView.group [] (List.indexedMap viewPendingAttachment model.attachments)
            , InputGroup.view
                [ class "items-end! gap-1 border-0! bg-transparent! ring-0!" ]
                [ InputGroup.textarea
                    [ name "prompt"
                    , placeholder
                        (if model.recording == NotRecording then
                            "Ask elmgpt"

                         else
                            "Listening…"
                        )
                    , value model.draft
                    , onInput DraftChanged
                    , onEnter SendClicked
                    , class "max-h-52 min-h-0! self-center px-0! py-1.5! ps-1! leading-6 md:text-base!"
                    ]
                , InputGroup.start [ class "ms-0! py-0! ps-0!" ]
                    [ Button.new
                        |> Button.icon (Phosphor.plus Regular |> Phosphor.toHtml [])
                        |> Button.label "Add images or audio"
                        |> Button.ghost
                        |> Button.large
                        |> Button.rounded
                        |> Button.toHtml [ onClick AttachClicked, class "text-foreground [&_svg]:size-5!" ]
                    ]
                , InputGroup.end [ class "me-0! gap-1! py-0! pe-0!" ]
                    [ viewRecordButton model.recording
                    , viewSendButton model
                    ]
                ]
            ]
        ]


{-| How much of the context window the conversation takes, as a ring that fills up. It warns as the
window runs out, since the oldest messages are left out from then on.
-}
viewContext : Context -> Html msg
viewContext context =
    let
        share =
            if context.window > 0 then
                clamp 0 1 (context.used / context.window)

            else
                0

        percentage =
            String.fromInt (round (share * 100)) ++ "%"

        description =
            percentage
                ++ " of the context used: "
                ++ String.fromInt (round context.used)
                ++ " of "
                ++ String.fromInt (round context.window)
                ++ " tokens"

        -- The ring's circumference, for a radius of 8.
        length =
            2 * pi * 8
    in
    span
        [ attribute "role" "img"
        , attribute "aria-label" description
        , Html.Attributes.title description
        , class "flex items-center gap-1.5 text-xs text-muted-foreground"
        ]
        [ text percentage
        , Svg.svg [ SvgA.viewBox "0 0 20 20", SvgA.class "size-4 -rotate-90" ]
            [ Svg.circle
                [ SvgA.cx "10", SvgA.cy "10", SvgA.r "8", SvgA.fill "none", SvgA.strokeWidth "3", SvgA.class "stroke-muted-foreground/30" ]
                []
            , Svg.circle
                [ SvgA.cx "10"
                , SvgA.cy "10"
                , SvgA.r "8"
                , SvgA.fill "none"
                , SvgA.strokeWidth "3"
                , SvgA.strokeLinecap "round"
                , SvgA.strokeDasharray (String.fromFloat length)
                , SvgA.strokeDashoffset (String.fromFloat (length * (1 - share)))
                , SvgA.class
                    (if share >= 0.9 then
                        "stroke-destructive"

                     else if share >= 0.75 then
                        "stroke-amber-500"

                     else
                        "stroke-blue-500"
                    )
                , SvgA.class "transition-[stroke-dashoffset] duration-500"
                ]
                []
            ]
        ]


{-| An attachment waiting in the composer, with a button to take it out again.
-}
viewPendingAttachment : Int -> Attachment -> Html Msg
viewPendingAttachment index attachment =
    AttachmentView.new
        |> AttachmentView.title attachment.name
        |> AttachmentView.description (attachmentKind attachment ++ " · " ++ subtype attachment.mediaType)
        |> (if isImage attachment.mediaType then
                AttachmentView.image [ src attachment.url, alt attachment.name ]

            else
                AttachmentView.icon (Phosphor.waveform Regular |> Phosphor.toHtml [])
           )
        |> AttachmentView.actions
            [ Button.new
                |> Button.icon (Phosphor.x Regular |> Phosphor.toHtml [])
                |> Button.label ("Remove " ++ attachment.name)
                |> AttachmentView.action [ onClick (AttachmentRemoved index) ]
            ]
        |> AttachmentView.small
        |> AttachmentView.toHtml [ class "max-w-56 bg-background/60" ]


{-| `PNG` for `image/png`.
-}
subtype : String -> String
subtype mediaType =
    mediaType
        |> String.split "/"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault mediaType
        |> String.toUpper


viewRecordButton : Recording -> Html Msg
viewRecordButton recording =
    case recording of
        NotRecording ->
            Button.new
                |> Button.icon (Phosphor.microphone Regular |> Phosphor.toHtml [])
                |> Button.label "Record a voice message"
                |> Button.ghost
                |> Button.large
                |> Button.rounded
                |> Button.toHtml [ onClick RecordClicked, class "text-foreground [&_svg]:size-5!" ]

        Recording _ ->
            Button.new
                |> Button.text "Stop recording"
                |> Button.startIcon (span [ class "size-2 animate-pulse rounded-full bg-destructive" ] [])
                |> Button.destructive
                |> Button.large
                |> Button.rounded
                |> Button.toHtml [ onClick RecordStopClicked ]

        _ ->
            Button.new
                |> Button.icon (Spinner.view [])
                |> Button.label "Preparing the voice message"
                |> Button.ghost
                |> Button.large
                |> Button.rounded
                |> Button.toHtml [ disabled True ]


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
