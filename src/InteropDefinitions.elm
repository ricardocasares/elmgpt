module InteropDefinitions exposing
    ( AbortController
    , Availability(..)
    , Flags
    , FromElm(..)
    , Message
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


{-| A turn of the conversation, in the shape the AI SDK takes.
-}
type alias Message =
    { role : Role
    , content : String
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
    | StreamEnded
    | StreamError String
    | ContextOverflow


{-| `theme` is what the user last chose, if anything.
-}
type alias Flags =
    { theme : Maybe Theme
    , systemDark : Bool
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
        (\vCopyToClipboard vReadFromLocalStorage vWriteToLocalStorage vCheckAvailability vCreateSession vCreateAbortController vGenerate vStream vAbort value ->
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
        , ( "StreamEnded", TsDecode.succeed StreamEnded )
        , ( "StreamError", TsDecode.map StreamError messageField )
        , ( "ContextOverflow", TsDecode.succeed ContextOverflow )
        ]


messageField : Decoder String
messageField =
    TsDecode.field "message" TsDecode.string


textField : Decoder String
textField =
    TsDecode.field "text" TsDecode.string


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
    TsDecode.map2 Flags
        (TsDecode.field "theme" (TsDecode.nullable themeDecoder))
        (TsDecode.field "systemDark" TsDecode.bool)
