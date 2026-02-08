# Gaigai VSCode Extension

Local-first VSCode tooling for Gaigai using `gaigai.js` as the language runtime and parser source of truth.

## Features
- Language registration for `gaigai` (`.gaigai`, `.gai`, `.gg`)
- TextMate syntax highlighting with builtin-function emphasis
- Snippets for core, OOP, and matrix workflows
- Live diagnostics (`gaigai` collection)
  - Syntax diagnostics from `parse(source)`
  - Core static diagnostics for unknown builtin calls and arity mismatch
- IntelliSense
  - Builtin completion with callable snippet inserts
  - Hover with signature/category metadata
  - Signature help with active argument tracking
- Command: `Gaigai: Run Current File`
  - Executes via `runGaigai(source, {}, { strictArity: true, loggerFn: noop })`
  - Output channel sections: `file`, `value`, `output`, `vars`, `timing(ms)`
  - Prepends a warning line when diagnostics contain errors

## Build Pipeline
- Builtin metadata is generated from `gaigai.js`:
  - Script: `scripts/extract-builtins.mjs`
  - Artifact: `src/generated/builtins.json`
- `npm run compile` runs metadata generation first, then TypeScript compile.

## Development
```bash
npm install
npm run compile
npm run test
```

Press `F5` in VSCode to launch an Extension Development Host.

## Packaging
```bash
npm run package
```

Packaging uses `npx @vscode/vsce package --no-yarn`.

## Screenshot Placeholders
- Diagnostics UI placeholder: `docs/screenshots/diagnostics.png`
- Completion UI placeholder: `docs/screenshots/completion.png`
- Signature help UI placeholder: `docs/screenshots/signature.png`
