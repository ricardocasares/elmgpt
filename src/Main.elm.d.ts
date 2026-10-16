export type JsonObject = { [Key in string]?: JsonValue };
export type JsonArray = JsonValue[];

/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonObject
  | JsonArray;

export interface ElmApp {
  ports: {
    interopFromElm: PortFromElm<FromElm>;
    interopToElm: PortToElm<ToElm>;
    [key: string]: UnknownPort;
  };
}

export type FromElm = { data : JsonValue; tag : "StopRecording" } | { tag : "StartRecording" } | { data : JsonValue; tag : "Abort" } | { data : { controller : JsonValue; instructions : string; messages : { attachments : { mediaType : string; name : string; url : string }[]; content : string; role : "assistant" | "user" }[] }; tag : "Stream" } | { data : { controller : JsonValue; instructions : string; messages : { attachments : { mediaType : string; name : string; url : string }[]; content : string; role : "assistant" | "user" }[] }; tag : "Generate" } | { tag : "CreateAbortController" } | { tag : "CreateSession" } | { tag : "CheckAvailability" } | { data : { key : string; value : string }; tag : "WriteToLocalStorage" } | { data : string; tag : "ReadFromLocalStorage" } | { data : string; tag : "CopyToClipboard" };

export type ToElm = { dark : boolean; tag : "SystemThemeChanged" } | { key : string; tag : "LocalStorageRead"; value : string | null } | { key : string; message : string; tag : "LocalStorageError" } | { message : string; tag : "ClipboardError" } | { availability : "unsupported" | "unavailable" | "downloadable" | "downloading" | "available"; tag : "AvailabilityChecked" } | { message : string; tag : "AvailabilityError" } | { progress : number; tag : "DownloadProgress" } | { tag : "SessionCreated" } | { message : string; tag : "SessionError" } | { controller : JsonValue; tag : "AbortControllerCreated" } | { tag : "Generated"; text : string } | { message : string; tag : "GenerationError" } | { tag : "Chunk"; text : string } | { tag : "StreamEnded" } | { message : string; tag : "StreamError" } | { tag : "ContextOverflow" } | { recorder : JsonValue; tag : "RecordingStarted" } | { mediaType : string; name : string; tag : "Recorded"; url : string } | { message : string; tag : "RecordingError" } | { tag : "InputTooLarge" } | { tag : "ContextMeasured"; used : number; window : number };

export type Flags = { now : number; systemDark : boolean; theme : ("light" | "dark" | "system") | null };

export namespace Main {
  function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

export as namespace Elm;

export { Elm };

export type UnknownPort = PortFromElm<unknown> | PortToElm<unknown> | undefined;

export type PortFromElm<Data> = {
  subscribe(callback: (fromElm: Data) => void): void;
  unsubscribe(callback: (fromElm: Data) => void): void;
};

export type PortToElm<Data> = { send(data: Data): void };
