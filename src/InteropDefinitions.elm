module InteropDefinitions exposing
    ( AbortController
    , Attachment
    , Availability(..)
    , Context
    , Flags
    , FromElm(..)
    , Message
    , Recorder
    , Request
    , Role(..)
    , Theme(..)
    , ToElm(..)
    , interop
    , themeToString
    )

{-| The messages that cross the ports, and the TypeScript types that `npm run types` derives from them.

TypeScript is a wrapper: every `FromElm` message is one browser or library call, and every `ToElm`
message is one of its results or events. The decisions are all made in Elm.

-}

import Json.Decode as Decode
import Json.Encode as Encode
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }


type Theme
    = Light
    | Dark
    | System


{-| A JavaScript `AbortController`, held by Elm and handed back as it came. Elm never looks inside:
TypeScript makes one on `CreateAbortController` and aborts it on `Abort`.
-}
type AbortController
    = AbortController Decode.Value


{-| A JavaScript `MediaRecorder` taking a voice message, held by Elm like an `AbortController`.
TypeScript makes one on `StartRecording` and stops it on `StopRecording`.
-}
type Recorder
    = Recorder Decode.Value


{-| Whether the browser's built-in model can be used, as the Prompt API reports it. `Unsupported` is
a browser without the API at all.
-}
type Availability
    = Unsupported
    | Unavailable
    | Downloadable
    | Downloading
    | Available


type Role
    = User
    | Assistant


{-| A turn of the conversation. TypeScript turns it into the AI SDK's shape, with the attachments
as file parts.
-}
type alias Message =
    { role : Role
    , content : String
    , attachments : List Attachment
    }


{-| How many tokens of the model's context window the conversation takes, measured after each reply.
-}
type alias Context =
    { used : Float
    , window : Float
    }


{-| An image or an audio clip the user sent, as a `data:` URL. The model reads both.
-}
type alias Attachment =
    { name : String
    , mediaType : String
    , url : String
    }


{-| Everything one reply needs: the instructions, the whole conversation so far, and the controller
that can abort it.
-}
type alias Request =
    { instructions : String
    , messages : List Message
    , controller : AbortController
    }


type FromElm
    = CopyToClipboard String
    | ReadFromLocalStorage String
    | WriteToLocalStorage { key : String, value : String }
    | CheckAvailability
    | CreateSession
    | CreateAbortController
    | Generate Request
    | Stream Request
    | Abort AbortController
    | StartRecording
    | StopRecording Recorder


{-| `ToElm` is every result and event TypeScript sends. Among them, `Chunk` is the text of a streamed
reply, and `ReasoningChunk` through `ToolFailed` are the other parts of it. `id` is the AI SDK's id for
the part: a reasoning id, or the id of the tool call. A tool goes `ToolStarted`, `ToolCalled`, then
`ToolSucceeded` or `ToolFailed`. `ToolFailed` can also come straight after `ToolStarted`, when the
model's call could not be parsed. Elm pretty-prints `input` and `output` itself.
-}
type ToElm
    = SystemThemeChanged Bool
    | LocalStorageRead { key : String, value : Maybe String }
    | LocalStorageError { key : String, message : String }
    | ClipboardError String
    | AvailabilityChecked Availability
    | AvailabilityError String
    | DownloadProgress Float
    | SessionCreated
    | SessionError String
    | AbortControllerCreated AbortController
    | Generated String
    | GenerationError String
    | Chunk String
    | ReasoningChunk { id : String, text : String }
    | ReasoningEnded String
    | ToolStarted { id : String, name : String }
    | ToolCalled { id : String, name : String, input : Decode.Value }
    | ToolSucceeded { id : String, name : String, output : Decode.Value }
    | ToolFailed { id : String, name : String, error : String }
    | StreamEnded
    | StreamError String
    | ContextOverflow
    | RecordingStarted Recorder
    | Recorded Attachment
    | RecordingError String
    | ContextMeasured Context
    | InputTooLarge


{-| `theme` is what the user last chose, if anything. `now` is when the page loaded, in milliseconds,
which the conversations take their ids from.
-}
type alias Flags =
    { theme : Maybe Theme
    , systemDark : Bool
    , now : Int
    }


themeToString : Theme -> String
themeToString theme =
    case theme of
        Light ->
            "light"

        Dark ->
            "dark"

        System ->
            "system"



-- FROM ELM


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vCopyToClipboard vReadFromLocalStorage vWriteToLocalStorage vCheckAvailability vCreateSession vCreateAbortController vGenerate vStream vAbort vStartRecording vStopRecording value ->
            case value of
                CopyToClipboard text ->
                    vCopyToClipboard text

                ReadFromLocalStorage key ->
                    vReadFromLocalStorage key

                WriteToLocalStorage entry ->
                    vWriteToLocalStorage entry

                CheckAvailability ->
                    vCheckAvailability ()

                CreateSession ->
                    vCreateSession ()

                CreateAbortController ->
                    vCreateAbortController ()

                Generate request ->
                    vGenerate request

                Stream request ->
                    vStream request

                Abort controller ->
                    vAbort controller

                StartRecording ->
                    vStartRecording ()

                StopRecording recorder ->
                    vStopRecording recorder
        )
        |> TsEncode.variantTagged "CopyToClipboard" TsEncode.string
        |> TsEncode.variantTagged "ReadFromLocalStorage" TsEncode.string
        |> TsEncode.variantTagged "WriteToLocalStorage"
            (TsEncode.object
                [ TsEncode.required "key" .key TsEncode.string
                , TsEncode.required "value" .value TsEncode.string
                ]
            )
        |> TsEncode.variantObject "CheckAvailability" []
        |> TsEncode.variantObject "CreateSession" []
        |> TsEncode.variantObject "CreateAbortController" []
        |> TsEncode.variantTagged "Generate" requestEncoder
        |> TsEncode.variantTagged "Stream" requestEncoder
        |> TsEncode.variantTagged "Abort" abortControllerEncoder
        |> TsEncode.variantObject "StartRecording" []
        |> TsEncode.variantTagged "StopRecording" (TsEncode.map (\(Recorder value) -> value) TsEncode.value)
        |> TsEncode.buildUnion


requestEncoder : Encoder Request
requestEncoder =
    TsEncode.object
        [ TsEncode.required "instructions" .instructions TsEncode.string
        , TsEncode.required "messages" .messages (TsEncode.list messageEncoder)
        , TsEncode.required "controller" .controller abortControllerEncoder
        ]


messageEncoder : Encoder Message
messageEncoder =
    TsEncode.object
        [ TsEncode.required "role" .role roleEncoder
        , TsEncode.required "content" .content TsEncode.string
        , TsEncode.required "attachments" .attachments (TsEncode.list attachmentEncoder)
        ]


attachmentEncoder : Encoder Attachment
attachmentEncoder =
    TsEncode.object
        [ TsEncode.required "name" .name TsEncode.string
        , TsEncode.required "mediaType" .mediaType TsEncode.string
        , TsEncode.required "url" .url TsEncode.string
        ]


roleEncoder : Encoder Role
roleEncoder =
    TsEncode.union
        (\vUser vAssistant value ->
            case value of
                User ->
                    vUser

                Assistant ->
                    vAssistant
        )
        |> TsEncode.variantLiteral (Encode.string "user")
        |> TsEncode.variantLiteral (Encode.string "assistant")
        |> TsEncode.buildUnion


abortControllerEncoder : Encoder AbortController
abortControllerEncoder =
    TsEncode.map (\(AbortController value) -> value) TsEncode.value



-- TO ELM


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "SystemThemeChanged", TsDecode.map SystemThemeChanged (TsDecode.field "dark" TsDecode.bool) )
        , ( "LocalStorageRead"
          , TsDecode.map2 (\key value -> LocalStorageRead { key = key, value = value })
                (TsDecode.field "key" TsDecode.string)
                (TsDecode.field "value" (TsDecode.nullable TsDecode.string))
          )
        , ( "LocalStorageError"
          , TsDecode.map2 (\key message -> LocalStorageError { key = key, message = message })
                (TsDecode.field "key" TsDecode.string)
                messageField
          )
        , ( "ClipboardError", TsDecode.map ClipboardError messageField )
        , ( "AvailabilityChecked", TsDecode.map AvailabilityChecked (TsDecode.field "availability" availabilityDecoder) )
        , ( "AvailabilityError", TsDecode.map AvailabilityError messageField )
        , ( "DownloadProgress", TsDecode.map DownloadProgress (TsDecode.field "progress" TsDecode.float) )
        , ( "SessionCreated", TsDecode.succeed SessionCreated )
        , ( "SessionError", TsDecode.map SessionError messageField )
        , ( "AbortControllerCreated", TsDecode.map AbortControllerCreated (TsDecode.field "controller" abortControllerDecoder) )
        , ( "Generated", TsDecode.map Generated textField )
        , ( "GenerationError", TsDecode.map GenerationError messageField )
        , ( "Chunk", TsDecode.map Chunk textField )
        , ( "ReasoningChunk"
          , TsDecode.map2 (\id text -> ReasoningChunk { id = id, text = text })
                idField
                textField
          )
        , ( "ReasoningEnded", TsDecode.map ReasoningEnded idField )
        , ( "ToolStarted"
          , TsDecode.map2 (\id name -> ToolStarted { id = id, name = name })
                idField
                nameField
          )
        , ( "ToolCalled"
          , TsDecode.map3 (\id name input -> ToolCalled { id = id, name = name, input = input })
                idField
                nameField
                (TsDecode.field "input" TsDecode.value)
          )
        , ( "ToolSucceeded"
          , TsDecode.map3 (\id name output -> ToolSucceeded { id = id, name = name, output = output })
                idField
                nameField
                (TsDecode.field "output" TsDecode.value)
          )
        , ( "ToolFailed"
          , TsDecode.map3 (\id name error -> ToolFailed { id = id, name = name, error = error })
                idField
                nameField
                (TsDecode.field "error" TsDecode.string)
          )
        , ( "StreamEnded", TsDecode.succeed StreamEnded )
        , ( "StreamError", TsDecode.map StreamError messageField )
        , ( "ContextOverflow", TsDecode.succeed ContextOverflow )
        , ( "RecordingStarted", TsDecode.map (Recorder >> RecordingStarted) (TsDecode.field "recorder" TsDecode.value) )
        , ( "Recorded"
          , TsDecode.map3 (\name mediaType url -> Recorded { name = name, mediaType = mediaType, url = url })
                (TsDecode.field "name" TsDecode.string)
                (TsDecode.field "mediaType" TsDecode.string)
                (TsDecode.field "url" TsDecode.string)
          )
        , ( "RecordingError", TsDecode.map RecordingError messageField )
        , ( "InputTooLarge", TsDecode.succeed InputTooLarge )
        , ( "ContextMeasured"
          , TsDecode.map2 (\used window -> ContextMeasured { used = used, window = window })
                (TsDecode.field "used" TsDecode.float)
                (TsDecode.field "window" TsDecode.float)
          )
        ]


messageField : Decoder String
messageField =
    TsDecode.field "message" TsDecode.string


textField : Decoder String
textField =
    TsDecode.field "text" TsDecode.string


idField : Decoder String
idField =
    TsDecode.field "id" TsDecode.string


nameField : Decoder String
nameField =
    TsDecode.field "name" TsDecode.string


availabilityDecoder : Decoder Availability
availabilityDecoder =
    TsDecode.stringUnion
        [ ( "unsupported", Unsupported )
        , ( "unavailable", Unavailable )
        , ( "downloadable", Downloadable )
        , ( "downloading", Downloading )
        , ( "available", Available )
        ]


abortControllerDecoder : Decoder AbortController
abortControllerDecoder =
    TsDecode.map AbortController TsDecode.value



-- FLAGS


themeDecoder : Decoder Theme
themeDecoder =
    TsDecode.stringUnion [ ( "light", Light ), ( "dark", Dark ), ( "system", System ) ]


flags : Decoder Flags
flags =
    TsDecode.map3 Flags
        (TsDecode.field "theme" (TsDecode.nullable themeDecoder))
        (TsDecode.field "systemDark" TsDecode.bool)
        (TsDecode.field "now" TsDecode.int)
