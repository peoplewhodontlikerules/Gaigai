# Garrigale Language Specification

## 1. Overview
Garrigale is a hardened, non-compatible evolution of Gaigai-style expression DSL runtime.
Pipeline:

1. `tokenize(source)`
2. `parse(source)` -> AST (single expression)
3. `compile(ast, options)` -> closure `(ctx) => value`
4. `runGarrigale(source, initialVars, compileOptions)`

Core properties:
- Function-call-centric Gaigai syntax and feel.
- Program is exactly one expression.
- No `eval`, no `new Function`.
- Runtime variables are stored in `ctx.vars`.
- Guarded execution by default (ops/time/loop/call + stall detection).
- Unknown calls resolve only via explicit `externals` table.
- No backward-compatibility aliases are provided.

## 2. Public API
Exports:
- `tokenize(src)`
- `parse(source)`
- `compile(ast, options?)`
- `runGarrigale(source, initialVars?, compileOptions?)`
- `createGarrigaleRunner(source, compileOptions?)`
- `clearGarrigaleCache()`

## 3. Security Controls
### 3.1 Guard options (`compile` / `runGarrigale`)
- `maxOps` (default: `1_000_000`)
- `maxRuntimeMs` (default: `2_000`)
- `maxLoopIters` (default: `100_000`)
- `maxLoopStallIters` (default: `2_048`)
- `maxCallDepth` (default: `256`)
- `maxCalls` (default: `200_000`)
- `maxInferenceSteps` (default: `4_096`)
- `maxConstraintNodes` (default: `50_000`)

Other options:
- `strictVars` (default `false`)
- `strictArity` (default `true`)
- `loggerFn` (default `console.log`)
- `maxOutput` (default `Infinity`)

### 3.2 Infinite-loop countermeasures
- `whiler`/`forrer`/`timeser` are limited by `maxLoopIters`.
- `whiler` additionally tracks `ctx.vars` signature; if unchanged for too many iterations, execution stops with a stall error (`maxLoopStallIters`).
- Timeout and op budgets are checked continuously.

## 4. New Builtins
### 4.1 Logical inference: `logicInferer`
Signature:
- `logicInferer(facts, rules)` -> inferred facts tuple
- `logicInferer(facts, rules, query)` -> boolean or bindings tuple
- `logicInferer(facts, rules, maxSteps:number)` -> inferred facts tuple
- `logicInferer(facts, rules, query, options)` -> boolean or bindings tuple

Behavior:
- Supports variable terms (`\"?x\"`, `\"?y\"`, ...).
- Uses unification over tuples/objects/primitives.
- Uses depth-first backtracking search.
- Supports negation-as-failure with `(\"not\", literal)` in rule/query literals.
- Negated literals must be ground at evaluation time.
- Ground query returns boolean.
- Query containing variables returns bindings list (e.g. `({\"?x\": ...}, ...)`).

Rule formats:
1. Tuple style (legacy): `(premisesTuple, conclusion)`
2. Object style (recommended):
`objecter((\"head\", headLiteral), (\"body\", bodyLiteralOrTuple))`
or
`objecter((\"if\", premises), (\"then\", conclusion))`

Options (`query` mode):
- number:
  - ground query: treated as `maxSteps`
  - variable query: treated as `maxSolutions`
- object:
  - `maxSteps` (or `maxDepth`)
  - `maxSolutions` (or `limit`)

### 4.2 Constraint logic descriptor: `constraintLogicer`
Signature:
- `constraintLogicer(kind, vars, ...args)` -> constraint descriptor object

Typical `kind` values:
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

### 4.3 Constraint solving: `constraintSolver`
Signature:
- `constraintSolver(domains, constraints)` -> first solution object or `null`
- `constraintSolver(domains, constraints, maxSolutions)` -> one object or solution list
- `constraintSolver(domains, constraints, maxSolutions, maxNodes)`

Inputs:
- `domains`: object `{ varName: tupleDomain }`
- `constraints`: tuple of
  - descriptors from `constraintLogicer`
  - tuple/object descriptor forms
  - predicate functions (`funner`) as custom constraints

Behavior:
- Uses backtracking search with node budget.
- Validates referenced variables.
- Returns first solution by default; multiple solutions when `maxSolutions > 1`.

## 5. External Calls
Unknown function names are never resolved from globals.
Only `externals` is used.

Example:
```js
const { runGarrigale } = require("./garrigale.js");

const result = runGarrigale('externaler(("pow"),(2),(10))', {}, {
  externals: { pow: (a, b) => Math.pow(a, b) }
});
```

## 6. Notes
- Garrigale keeps Gaigai-style naming and expression ergonomics.
- Garrigale intentionally does not expose `runGaigai`/`createGaigaiRunner`/`clearGaigaiCache` aliases.
