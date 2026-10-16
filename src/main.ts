import { init } from "./agent";
import { Elm, type Flags, type JsonValue, type ToElm } from "./Main.elm";

//-- FLAGS

const media = window.matchMedia("(prefers-color-scheme: dark)");
const getStoredTheme = () => {
  try {
    return (localStorage.getItem("theme") as Flags["theme"]) ?? null;
  } catch {
    return null;
  }
};

//-- INIT

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    theme: getStoredTheme(),
    systemDark: media.matches,
  },
});

const send = (msg: ToElm) => app.ports.interopToElm.send(msg);
const err = (e: unknown) => e instanceof Error ? e.message : String(e);

//-- AGENT

const ai = init({ onContextOverflow: () => send({ tag: "ContextOverflow" }) });

// An AbortController crosses the ports as an opaque value: Elm holds it and hands it back as it
// came, so these casts only name what it is.
const opaque = (ctrl: AbortController) => ctrl as unknown as JsonValue;
const controller = (value: JsonValue) => value as unknown as AbortController;

//-- PORTS:SEND

media.addEventListener("change", (event) =>
  send({ tag: "SystemThemeChanged", dark: event.matches }),
);

//-- PORTS:SUBSCRIBE

app.ports.interopFromElm.subscribe((msg) => {
  switch (msg.tag) {
    case "CopyToClipboard":
      return navigator.clipboard
        .writeText(msg.data)
        .catch((error) =>
          send({ tag: "ClipboardError", message: err(error) }),
        );

    case "ReadFromLocalStorage":
      try {
        return send({
          tag: "LocalStorageRead",
          key: msg.data,
          value: localStorage.getItem(msg.data),
        });
      } catch (error) {
        return send({
          tag: "LocalStorageError",
          key: msg.data,
          message: err(error),
        });
      }

    case "WriteToLocalStorage":
      try {
        return localStorage.setItem(msg.data.key, msg.data.value);
      } catch (error) {
        return send({
          tag: "LocalStorageError",
          key: msg.data.key,
          message: err(error),
        });
      }

    case "CheckAvailability":
      return ai.availability().then(
        (availability) => send({ tag: "AvailabilityChecked", availability }),
        (error) => send({ tag: "AvailabilityError", message: err(error) }),
      );

    case "CreateSession":
      return ai
        .createSession((progress) => send({ tag: "DownloadProgress", progress }))
        .then(
          () => send({ tag: "SessionCreated" }),
          (error) => send({ tag: "SessionError", message: err(error) }),
        );

    case "CreateAbortController":
      return send({
        tag: "AbortControllerCreated",
        controller: opaque(new AbortController()),
      });

    case "Generate":
      return ai
        .generate(
          msg.data.instructions,
          msg.data.messages,
          controller(msg.data.controller).signal,
        )
        .then(
          (text) => send({ tag: "Generated", text }),
          (error) => send({ tag: "GenerationError", message: err(error) }),
        );

    case "Stream":
      return ai
        .stream(
          msg.data.instructions,
          msg.data.messages,
          controller(msg.data.controller).signal,
          (text) => send({ tag: "Chunk", text }),
        )
        .then(
          () => send({ tag: "StreamEnded" }),
          (error) => send({ tag: "StreamError", message: err(error) }),
        );

    case "Abort":
      return controller(msg.data).abort();

    default:
      return msg satisfies never;
  }
});
