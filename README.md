# elmgpt

A chat app that runs its language model inside your browser. The app is written in Elm. The model
is Chrome's built-in Gemini Nano, reached through the
[Prompt API](https://developer.chrome.com/docs/ai/prompt-api). There is no server, and the app
makes no network requests of its own.

All the logic lives in Elm. TypeScript is a thin wrapper: it turns each message from Elm into one
browser or library call, and sends back the result.

## Features

- Replies stream in as they are written. Stop ends one and keeps what has arrived.
- Replies are formatted as markdown, and each has a Copy button.
- Messages can carry images and audio, picked from disk or recorded as a voice message.
- After each reply, a ring at the bottom right shows how much of the model's context window the
  conversation takes. When the window fills up, the oldest messages leave the model's context. See
  [Context](#context).
- A new chat puts the current one in Recents, which lasts until the page is reloaded.
- The theme is light, dark or the system's, and is remembered.
- The assistant is prompted as a weather assistant and has one tool, `geolocation`, which asks the
  browser for your position. No weather service sits behind it.

## Requirements

The model runs on your machine, so the machine has to be able to run it. These are Chrome's
[documented requirements](https://developer.chrome.com/docs/ai/get-started) for the Prompt API:

- Desktop Chrome 148 or later, the first version with the Prompt API on by default. The system can
  be Windows 10 or 11, macOS 13 or later, Linux, or a Chromebook Plus. Android and iOS are not
  supported.
- At least 22 GB of free space on the volume that holds your Chrome profile.
- A GPU with more than 4 GB of VRAM, or a CPU with at least 16 GB of RAM and 4 cores. Audio input
  needs a GPU.
- An unmetered connection for the first run, which downloads the model. After that the model works
  offline.

To work on the code you also need Node.js 20.19 or newer, or 22.12 or newer, which is what Vite 8
asks for.

## Run it

```sh
npm install
npm run dev
```

Open the address Vite prints, `http://localhost:5173` by default, in Chrome.

The first time, the model is not on the machine. The app says so and shows a **Download model**
button. Chrome only starts a download after a click, so the app waits for one. A bar shows the
progress, and sending unlocks once the model has loaded.

| Script            | What it does                                                          |
| ----------------- | --------------------------------------------------------------------- |
| `npm run dev`     | Generates the TypeScript types, then starts the Vite dev server.      |
| `npm run build`   | Generates the types, then builds the app into `dist/`.                |
| `npm run preview` | Serves the build in `dist/`.                                          |
| `npm run types`   | Writes `src/Main.elm.d.ts` from `src/InteropDefinitions.elm`.         |

The microphone, geolocation and clipboard need a secure context, so a deployed build has to be
served over HTTPS. `localhost` counts as secure.

### When the model is not ready

| The app says                                          | What it means                                                                |
| ----------------------------------------------------- | ---------------------------------------------------------------------------- |
| Checking the built-in model…                          | Asking Chrome whether the model can be used.                                 |
| This browser has no Prompt API.                       | `LanguageModel` does not exist here. Use desktop Chrome 148 or later.        |
| The built-in model is not available on this device.   | Chrome has the API but reports the model as unavailable. Check the requirements. |
| The model has to be downloaded before it can answer.  | Click **Download model**.                                                    |
| The model could not be loaded                         | Chrome returned an error. The message is shown, and **Retry** asks again.    |

To see what Chrome thinks, run `await LanguageModel.availability()` in the DevTools console, and
open `chrome://on-device-internals`.

## How it works

Elm holds the state: whether the model is ready, the conversation, the system prompt, the reply in
progress, and the rules for the context window. When it needs something from the browser it sends a
message through a port, and the result comes back as another message. Even the system prompt
travels with each request from Elm.

`src/main.ts` receives every message from Elm, makes one call to `src/agent.ts` or to a browser
API, and sends the outcome back. It keeps no state. Two things only JavaScript can make, the
`AbortController` of a reply and the `MediaRecorder` of a voice message, are created when Elm asks
for them. They cross the ports as opaque values, and Elm hands them back when it wants to abort or
stop. Elm never looks inside them.

`src/agent.ts` puts the [Vercel AI SDK](https://ai-sdk.dev) in front of
[browser-ai](https://www.browser-ai.dev), its provider for the Prompt API. Every reply gets a model
instance of its own, and so a new session that starts with the instructions and the whole
conversation Elm holds. What Elm holds is exactly what the model sees.

The messages are defined in `src/InteropDefinitions.elm`:

| Elm sends                                     | TypeScript replies with                                                                                      |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `CheckAvailability`                           | `AvailabilityChecked`, or `AvailabilityError`.                                                               |
| `CreateSession`                               | `DownloadProgress` while the model downloads, then `SessionCreated` or `SessionError`.                       |
| `CreateAbortController`                       | `AbortControllerCreated`.                                                                                    |
| `Stream`                                      | `Chunk` for each piece of text, `ContextMeasured` at the end, then `StreamEnded`, `InputTooLarge` or `StreamError`. |
| `Generate`                                    | `Generated`, `InputTooLarge` or `GenerationError`. The UI always streams, so nothing sends this yet.         |
| `Abort`                                       | Nothing. The `Stream` in flight fails with the abort error, which Elm ignores because it asked for it.      |
| `StartRecording`, `StopRecording`             | `RecordingStarted`, then `Recorded` with the clip, or `RecordingError`.                                      |
| `CopyToClipboard`                             | `ClipboardError`, if it fails.                                                                               |
| `ReadFromLocalStorage`, `WriteToLocalStorage` | `LocalStorageRead`, or `LocalStorageError`.                                                                  |

Two messages arrive unasked. `SystemThemeChanged` says the operating system switched between light
and dark. `ContextOverflow` says Chrome had to leave the oldest messages out of a reply.

[elm-ts-interop](https://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest/) turns the
definitions into TypeScript types. `npm run types` writes them to `src/Main.elm.d.ts`, which is
committed and not edited by hand.

### Context

The built-in model has a small context window. After each reply the app measures how much of it the
conversation takes. From 80% on, all but the last 10 messages leave the model's context. They stay
in the transcript under a line that says the model no longer sees them, and are saved in
localStorage as `archive:<conversation id>`. If the Prompt API refuses a turn as too large, the
conversation is compacted the same way and the turn is asked again, once.

Nothing reads the archive back yet. [TODO.md](TODO.md) lists what comes next: summaries instead of
dropped messages, a fixed context budget, search over the archive, and storage in IndexedDB.

## Project layout

| Path                         | What is in it                                                      |
| ---------------------------- | ------------------------------------------------------------------ |
| `src/Main.elm`               | The app: model, update and view.                                   |
| `src/InteropDefinitions.elm` | Every message across the ports, with its encoder or decoder.       |
| `src/InteropPorts.elm`       | The two ports, and the functions that wrap them.                   |
| `src/main.ts`                | Starts Elm with its flags, and maps each message from Elm to a call. |
| `src/agent.ts`               | The AI SDK and browser-ai: availability, download, generate, stream. |
| `src/Main.elm.d.ts`          | Generated TypeScript types for the ports.                          |
| `src/UI/`                    | The [elmcn](https://github.com/ricardocasares/elmcn) components.   |
| `src/main.css`               | The one stylesheet: Tailwind and the theme.                        |
| `scripts/elm.mjs`            | The Elm compiler, wrapped for Vite. See below.                     |
| `TODO.md`                    | Plans for the context window.                                      |
| `AGENTS.md`                  | Conventions for coding agents, and how to add components.          |

Vite compiles Elm through `scripts/elm.mjs`. elm-markdown puts the characters U+2028 and U+2029 into
string literals as they are, and the plugin parses the output as ES2015, where they end a string.
The wrapper runs the compiler and writes them as escapes.

## Developing

### Adding a message

1. In `src/InteropDefinitions.elm`, add the variant to `FromElm` or `ToElm`, with its encoder or
   decoder.
2. Run `npm run types`.
3. For a `FromElm` variant, add its `case` in `src/main.ts`. Until then `npx tsc --noEmit` fails,
   because the switch ends with `msg satisfies never`.
4. For a `ToElm` variant, handle it in `fromInterop` in `src/Main.elm`. The compiler refuses to
   build until you do.

### UI components

The modules in `src/UI/` are [elmcn](https://github.com/ricardocasares/elmcn), shadcn/ui for Elm.
They are copied into the project, so they are ours to change. Add one with the shadcn CLI, and
search the registry the same way:

```sh
npx shadcn@latest add ricardocasares/elmcn/card
npx shadcn@latest search ricardocasares/elmcn
```

Components are builders: `Button.new`, then setters, then `toHtml`. `AGENTS.md` has the details.

### Without the model

On a machine that cannot run the model, the app stops at its status message and sending stays
disabled. To work on the chat anyway, give the page a stand-in `window.LanguageModel` before its
scripts run. One with `availability()`, `create()` and a session whose `promptStreaming()` returns a
stream is enough to exercise the download, streaming and stop flows.

## Built with

- [Elm](https://elm-lang.org) 0.19.3, with elm-markdown and phosphor-elm
- Vercel AI SDK 7 and browser-ai 3
- Tailwind CSS 4, Vite 8 and TypeScript 7
