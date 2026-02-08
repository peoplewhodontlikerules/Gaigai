# Gairai Language Specification

## 1. Overview
Gairai is a hardened Gaigai-compatible expression interpreter implemented in JavaScript.
Pipeline:

1. `tokenize(source)`
2. `parse(source)` -> AST (single expression)
3. `compile(ast, options)` -> closure `(ctx) => value`
4. `runGairai(source, initialVars, compileOptions)`

Core properties:
- Function-call-centric syntax.
- Program is exactly one expression.
- No `eval`, no `new Function`.
- Runtime variables are stored in `ctx.vars`.
- Security guards are enabled by default (loop/time/op/call budgets).
- Non-global external function calls are supported via explicit function table injection.

## 2. Public API
Exports:
- `tokenize(src)`
- `parse(source)`
- `compile(ast, options?)`
- `runGairai(source, initialVars?, compileOptions?)`
- `createGairaiRunner(source, compileOptions?)`
- `clearGairaiCache()`

Compatibility aliases:
- `runGaigai` -> `runGairai`
- `createGaigaiRunner` -> `createGairaiRunner`
- `clearGaigaiCache` -> `clearGairaiCache`

## 3. Security Controls
Gairai adds built-in DoS / runaway-execution guards.

### 3.1 Guard options (`compile` / `runGairai`)
- `maxOps` (default: `1_000_000`)
- `maxRuntimeMs` (default: `2_000`)
- `maxLoopIters` (default: `100_000`)
- `maxCallDepth` (default: `256`)
- `maxCalls` (default: `200_000`)

Existing options remain available:
- `strictVars` (default `false`)
- `strictArity` (default `false`)
- `loggerFn` (default `console.log`)
- `maxOutput` (default `Infinity`)

### 3.2 Guard behavior
- `whiler`/`forrer`/`timeser` are guarded by loop-iteration budget.
- Higher-order iteration builtins (`mapper`/`filterer`/`reducer`/`everyer`/`somer`) are guarded.
- Function invocation pathways (`funner`/`applyer`/`spreadApplyer`/`caller`/`newer`/`constructer`) are guarded with call depth and call count limits.
- Timeout is enforced during guarded execution points.

When limits are exceeded, execution throws `Error` with a phase-specific message.

## 4. Non-Global External Function Calls
Gairai never falls back to global object lookup for unknown calls.
Unknown function names are resolved only from explicitly provided `externals`.

### 4.1 Option
- `externals: { [name: string]: Function }`

### 4.2 Call styles
1. Direct unknown-name call (resolved from `externals`):
```gaigai
myFunc((1),(2))
```

2. Dynamic external call builtin:
```gaigai
externaler(("myFunc"),(1),(2))
```

If function is missing in `externals`, it throws `Unknown function '<name>'`.

### 4.3 JavaScript usage example
```js
const { runGairai } = require('./gairai.js');

const result = runGairai('externaler(("pow"),(2),(10))', {}, {
  externals: {
    pow: (a, b) => Math.pow(a, b),
  },
  strictArity: true,
});

console.log(result.value); // 1024
```

## 5. Caching
`runGairai` supports compile cache with `cache: true` (default).
Cache key includes source plus compile options, including guard options and `externals` object identity.

## 6. Language Surface
Gairai inherits Gaigai syntax, AST, and builtin families (numeric, control-flow, tuple/array, object/path, OOP, function-values, regex/encoding, matrix/linear algebra), with one additional builtin:
- `externaler(name, ...args)`

## 7. Error Behavior
- Syntax errors include absolute position + line/column + caret excerpt.
- Runtime failures throw `Error`.
- Guard violations throw `Error` with limit details.
- Unknown calls throw unless resolved from `externals`.

## 8. Non-Goals
- Backward compatibility with unsafe/no-guard behavior.
- Implicit global function execution.
