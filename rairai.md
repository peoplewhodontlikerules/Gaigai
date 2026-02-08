# Rairai Language Specification

## 1. Overview
Rairai is a hardened Raigai interpreter.

Pipeline:
1. `tokenize(source)`
2. `parse(source)` -> AST
3. `typeCheck(ast, options)`
4. `runRairai(source, initialVars, options)`

Goals:
- Keep Raigai readability and type safety
- Add infinite-loop / DoS protections
- Add explicit non-global external function calls
- No `eval`, no `new Function`

## 2. Public API
Exports:
- `tokenize(src)`
- `parse(source)`
- `parseTypeString(typeSource)`
- `typeCheck(ast, options?)`
- `compile(ast, options?)`
- `runRairai(source, initialVars?, options?)`
- `createRairaiRunner(source, options?)`
- `clearRairaiCache()`

## 3. Security Guards
Default guard options:
- `maxOps: 1_000_000`
- `maxRuntimeMs: 2_000`
- `maxLoopIters: 100_000`
- `maxCallDepth: 256`
- `maxCalls: 200_000`

Behavior:
- Every expression/statement consumes operation budget.
- `while` / `for in` consume loop-iteration budget.
- Function calls (user/builtin/external) consume call depth and call count budgets.
- Time limit is checked continuously.

When exceeded, Rairai throws `GuardError: ...`.

## 4. Non-Global External Function Calls
Rairai never resolves unknown calls from global objects.
Only `options.externals` is used.

### 4.1 Runtime option
```js
externals: {
  pow: (a, b) => Math.pow(a, b),
  slugify: (s) => String(s).toLowerCase().replace(/\s+/g, "-")
}
```

### 4.2 Call styles
1. Direct external call (identifier not locally defined):
```rairai
let x: number = pow(2, 10);
```

2. Explicit external dispatcher:
```rairai
let slug: string = external("slugify", "Rairai DSL");
```

## 5. External Type Hints (optional)
`externalTypes` lets static type-checking validate external function arguments and return type.

```js
externalTypes: {
  pow: {
    params: ["number", "number"],
    returnType: "number"
  },
  slugify: {
    params: ["string"],
    returnType: "string"
  }
}
```

## 6. Other Options
- `strictTypes` (default `true`)
- `loggerFn` (default `console.log`)
- `maxOutput` (default `Infinity`)
- `globalTypes` (initial variable types)
- `cache` (`runRairai` only, default `true`)

## 7. Example (JavaScript)
```js
const fs = require("fs");
const { runRairai } = require("./rairai.js");

const source = fs.readFileSync("./sample.rairai", "utf8");
const result = runRairai(source, {}, {
  externals: {
    pow: Math.pow,
    slugify: (s) => String(s).toLowerCase().replace(/\s+/g, "-")
  },
  externalTypes: {
    pow: { params: ["number", "number"], returnType: "number" },
    slugify: { params: ["string"], returnType: "string" }
  }
});

console.log(result.value);
```
