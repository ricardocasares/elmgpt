# AGENTS.md

This is an Elm app styled with [elmcn](https://github.com/ricardocasares/elmcn): shadcn/ui
components for Elm. Like shadcn/ui, elmcn is not a package. The CLI copies a component's
source into `src/UI/<Name>.elm`, and from then on the code is ours to read and change.

## Find components

Search the registry with the shadcn CLI. No `components.json` is needed.

```sh
npx shadcn@latest search ricardocasares/elmcn
npx shadcn@latest search ricardocasares/elmcn -q dialog
```

Each result is an item name such as `button` or `alert-dialog`. To see what an item
installs and which Elm packages it needs, view it:

```sh
npx shadcn@latest view ricardocasares/elmcn/dialog
```

The installed components are the modules in `src/UI/`. Read one to learn its API before
using it.

## Install components

```sh
npx shadcn@latest add ricardocasares/elmcn/card ricardocasares/elmcn/input
```

- Each item is written to `src/UI/<Name>.elm`, together with the components it uses.
  `alert-dialog` becomes `UI.AlertDialog`.
- Read the notes the CLI prints after the install. Some items need an Elm package, for
  example `elm install elm/json`, or a script that `src/main.ts` must import.
- Run `npx shadcn@latest add ... --diff` to compare an installed component with the
  registry before overwriting local changes.
- Do not write a component by hand if the registry has it.

## Use components

Components are builders: `X.new`, then content setters, then variant and size setters,
then `toHtml`. Break the line after `X.new` and put each `|>` on its own line.

```elm
import UI.Button as Button

Button.new
    |> Button.text "Save"
    |> Button.outline
    |> Button.small
    |> Button.toHtml [ onClick Save ]
```

Required content is enforced by the type, so an incomplete builder does not compile.

## Project

- `src/Main.elm` is the app, started by `src/main.ts`.
- `src/main.css` is the one stylesheet: Tailwind and the theme. `index.html` links it,
  so do not import CSS from TypeScript.
- After changing `src/InteropDefinitions.elm`, regenerate the types with
  `npm run types`. `npm run dev` and `npm run build` do it for you.
- `npm run dev` starts Vite.
