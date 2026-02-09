# Raggirale Language Specification

## 1. Overview
Raggirale is a non-compatible hardened evolution of Raigai, inspired by the Garrigale direction.

Pipeline:
1. `tokenize(source)`
2. `parse(source)` -> AST
3. `typeCheck(ast, options)`
4. `runRaggirale(source, initialVars, options)`

Goals:
- Keep Raigai readability and statement syntax
- Keep static/runtime type safety defaults
- Add stronger runaway protections
- Add logic inference + constraint reasoning builtins
- No `eval`, no `new Function`

## 2. Public API
Exports:
- `tokenize(src)`
- `parse(source)`
- `parseTypeString(typeSource)`
- `typeCheck(ast, options?)`
- `compile(ast, options?)`
- `runRaggirale(source, initialVars?, options?)`
- `createRaggiraleRunner(source, options?)`
- `clearRaggiraleCache()`

No compatibility aliases are exposed.

## 3. Guard Options
Default guard options:
- `maxOps: 1_000_000`
- `maxRuntimeMs: 2_000`
- `maxLoopIters: 100_000`
- `maxLoopStallIters: 2_048`
- `maxCallDepth: 256`
- `maxCalls: 200_000`
- `maxInferenceSteps: 4_096`
- `maxConstraintNodes: 50_000`

Behavior:
- Every expression/statement consumes op budget.
- `while` / `for in` consume loop budget.
- `while` also checks visible-scope state change and stops stalled loops.
- Function calls (user/builtin/external) consume call depth/count budgets.

Guard violations throw `GuardError: ...`.

## 4. External Calls
Unknown identifier calls never resolve through global objects.
Only `options.externals` is used.

- Direct: `pow(2, 10)` (when not locally declared)
- Explicit: `external("pow", 2, 10)`

Optional `externalTypes` keeps static/runtime checks for externals.

## 5. New Reasoning Builtins
### 5.1 `logicInfer(facts, rules, query, options)`
- `facts`: list of atoms
  - atom form: `[pred, arg1, arg2, ...]`
- `rules`: list of rules
  - fact-like: `[head]`
  - implication: `[body, head]`
  - body can be one literal or list of literals
- variable terms: strings starting with `?` (e.g. `"?x"`)
- negation literal: `["not", atom]` (negation-as-failure, ground-only)

`query` result:
- Ground query => `bool`
- Variable query => list of bindings
  - each binding is list of pairs: `[["?x", value], ...]`

`options`:
- number: interpreted as max steps (ground query) or max solutions (variable query)
- list: `[maxSteps, maxSolutions]`
- object (from JS host): `{ maxSteps, maxSolutions }`

### 5.2 `constraintLogic(kind, vars, args)`
Builds a constraint descriptor.

- `kind`: constraint kind string
- `vars`: variable name or list of variable names
- `args`: list (or scalar)

### 5.3 `constraintSolve(domains, constraints, options)`
Backtracking CSP solver.

Inputs:
- `domains`:
  - list form (DSL-friendly): `[["x", [1,2,3]], ["y", [1,2,3]]]`
  - object form (host-side JS): `{ x: [1,2,3], y: [1,2,3] }`
- `constraints`: list of descriptors (`constraintLogic(...)` output, list/object forms, or JS predicate fn)
- `options`:
  - number: max solutions
  - list: `[maxSolutions, maxNodes]`
  - object (host-side JS): `{ maxSolutions, maxNodes }`

Supported kinds:
- `allDifferent`
- `equal`
- `notEqual`
- `less`
- `lessEqual`
- `greater`
- `greaterEqual`
- `sumEqual`
- `sumLessEqual`
- `sumGreaterEqual`
- `inDomain`

Return:
- one solution (default) or multiple solutions
- solution format: list of pairs, e.g. `[["x",1],["y",2]]`

## 6. Other Options
- `strictTypes` (default `true`)
- `loggerFn` (default `console.log`)
- `maxOutput` (default `Infinity`)
- `globalTypes` (initial variable types)
- `cache` (`runRaggirale` only, default `true`)

## 7. Example
```js
const { runRaggirale } = require("./raggirale.js");

const src = `
let facts: any = [["parent","alice","bob"],["parent","bob","carol"]];
let rules: any = [
  [["parent","?x","?y"], ["ancestor","?x","?y"]],
  [[["parent","?x","?y"],["ancestor","?y","?z"]], ["ancestor","?x","?z"]]
];
logicInfer(facts, rules, ["ancestor","alice","?who"], [5000, 10]);
`;

console.log(runRaggirale(src).value);
```
