import { z } from 'zod'
import { ToolLoopAgent, isStepCount, tool } from "ai";
import { browserAI, doesBrowserSupportBrowserAI } from "@browser-ai/core";
import { type ElmApp } from "./Main.elm";

async function init(app: ElmApp) {
    if (!doesBrowserSupportBrowserAI()) {
        return app.ports.interopToElm.send({
            tag: "AINotSupportedError",
            error: "Your browser does not support Prompt API"
        });
    }

    const model = browserAI("text", {
        onContextOverflow: (event) => app.ports.interopToElm.send({ tag: 'AIContextOverflow', error: event }),
    })

    const availability = await model.availability();

    switch (availability) {
        case 'unavailable':
            return app
                .ports
                .interopToElm
                .send({ tag: "AINotAvailableError", error: 'Propmt API is not available in your browser' })
        case 'available':
        case 'downloadable':
            try {
                await model
                    .createSessionWithProgress(progress => app
                        .ports
                        .interopToElm
                        .send({ tag: 'AIDownloadProgress', progress }));
            } catch (err) {
                return app.ports.interopToElm.send({ tag: 'AIDownloadError', error: String(err) })
            }
    }

    if (!availability) {
        return app.ports.interopToElm.send({
            tag: "AINotAvailableError",
            error: "Prompt API is not available See https://github.com/webmachinelearning/prompt-api"
        });
    }

    if (availability === 'downloadable') {
        await model
            .createSessionWithProgress((progress) => app
                .ports
                .interopToElm
                .send({ tag: 'AIProgress', progress }))
            .catch(err => app.ports.interopToElm.send({
                tag: 'AIDownloadError',
                error: String(err)
            }));
    }

    const agent = new ToolLoopAgent({
        model,
        instructions: "You are a weather assistant.",
        tools: {
            geolocation: tool({
                description: 'Obtain the user geolocation',
                inputSchema: z.object({
                    enableHighAccuracy: z.boolean(),
                    timeout: z.number().meta({ description: 'Time in milliseconds' })
                }),
                execute: async () => {
                    return new Promise((resolve, reject) =>
                        navigator.geolocation.getCurrentPosition(resolve, reject, {
                            enableHighAccuracy: true,
                            timeout: 5000
                        }))
                }
            })
        },
        stopWhen: isStepCount(20),
    });

    const generate = (prompt: string, signal: AbortSignal) => {
        // TODO
        // Elm must acquire an abort signal before calling this method
        // A signal can be passed as an opaque value to Elm via interop.
        // Then abortion can be handled at main.ts as a message.
    }

    const stream = (prompt: string, signal: AbortSignal) => {
        // TODO
        // Elm must acquire an abort signal before calling this method
        // A signal can be passed as an opaque value to Elm via interop.
        // Then abortion can be handled at main.ts as a message.
    }

    return { stream, generate }

}

export default { init }