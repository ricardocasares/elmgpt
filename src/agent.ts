import { z } from "zod";
import { ToolLoopAgent, isStepCount, tool, type ModelMessage } from "ai";
import {
    browserAI,
    doesBrowserSupportBrowserAI,
    type BrowserAIChatSettings,
} from "@browser-ai/core";

/** A turn as Elm holds it: attachments are `data:` URLs of images and audio clips. */
export type ChatMessage = {
    role: "user" | "assistant";
    content: string;
    attachments: { name: string; mediaType: string; url: string }[];
};

/**
 * The AI SDK's shape of a turn. Attachments become file parts, which browser-ai hands to the
 * Prompt API as image and audio inputs, asking for a session that expects them.
 */
const toModelMessage = ({ role, content, attachments }: ChatMessage): ModelMessage =>
    role === "assistant"
        ? { role, content }
        : {
              role,
              content: [
                  ...(content ? [{ type: "text" as const, text: content }] : []),
                  ...attachments.map(({ name, mediaType, url }) => ({
                      type: "file" as const,
                      filename: name,
                      mediaType,
                      // The base64 after the comma of a `data:` URL.
                      data: url.slice(url.indexOf(",") + 1),
                  })),
              ],
          };

/** How many tokens of the model's context window the conversation takes. */
export type Context = { used: number; window: number };

/** What the Prompt API says about the built-in model. `unsupported` is a browser without the API. */
export type Availability =
    | "unsupported"
    | "unavailable"
    | "downloadable"
    | "downloading"
    | "available";

const tools = {
    geolocation: tool({
        description: "Obtain the user geolocation",
        inputSchema: z.object({
            enableHighAccuracy: z.boolean(),
            timeout: z.number().meta({ description: "Time in milliseconds" }),
        }),
        execute: (input) =>
            new Promise<{ latitude: number; longitude: number; accuracy: number }>(
                (resolve, reject) =>
                    navigator.geolocation.getCurrentPosition(
                        ({ coords }) =>
                            resolve({
                                latitude: coords.latitude,
                                longitude: coords.longitude,
                                accuracy: coords.accuracy,
                            }),
                        (error) => reject(new Error(error.message)),
                        input,
                    ),
            ),
    }),
};

/**
 * The browser's built-in model behind the AI SDK. Every method is a single call with no state of
 * its own: Elm decides when to check, download, generate and abort, and holds the conversation.
 */
export function init(settings: BrowserAIChatSettings) {
    // browser-ai gives a model instance one Chrome session for life, and sets the system prompt
    // only when that session is created. This instance checks availability and downloads the model
    // with progress. Every reply gets an instance of its own, so the instructions apply and the
    // messages Elm sends are the whole context.
    const model = browserAI("text", settings);

    const agent = (instructions: string) => {
        const model = browserAI("text", settings);
        return {
            agent: new ToolLoopAgent({
                model,
                instructions,
                tools,
                stopWhen: isStepCount(20),
            }),
            // The reply's session holds the whole conversation, so what it has used once the reply
            // ends is what the conversation takes of the context window.
            measure: (onContext: (context: Context) => void) => {
                const used = model.getContextUsage();
                const window = model.getContextWindow();
                if (used !== undefined && window !== undefined) onContext({ used, window });
            },
        };
    };

    return {
        availability: async (): Promise<Availability> =>
            doesBrowserSupportBrowserAI() ? model.availability() : "unsupported",

        createSession: async (
            onProgress: (progress: number) => void,
        ): Promise<void> => {
            await model.createSessionWithProgress(onProgress);
        },

        generate: async (
            instructions: string,
            messages: ChatMessage[],
            signal: AbortSignal,
            onContext: (context: Context) => void,
        ): Promise<string> => {
            const reply = agent(instructions);
            try {
                const result = await reply.agent.generate({
                    messages: messages.map(toModelMessage),
                    abortSignal: signal,
                });
                return result.text;
            } finally {
                reply.measure(onContext);
            }
        },

        stream: async (
            instructions: string,
            messages: ChatMessage[],
            signal: AbortSignal,
            onChunk: (text: string) => void,
            onContext: (context: Context) => void,
        ): Promise<void> => {
            const reply = agent(instructions);
            try {
                const result = await reply.agent.stream({
                    messages: messages.map(toModelMessage),
                    abortSignal: signal,
                });
                for await (const part of result.stream) {
                    switch (part.type) {
                        case "text-delta":
                            onChunk(part.text);
                            break;
                        case "error":
                            throw part.error;
                    }
                }
            } finally {
                reply.measure(onContext);
            }
        },
    };
}
