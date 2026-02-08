# Raigai Language Specification

## 1. Overview
Raigai is a **new DSL** inspired by Gaigai's runtime philosophy, but its syntax is intentionally **not backward compatible**.

Execution flow:
1. `tokenize(source)`
2. `parse(source)` -> AST (multi-statement program)
3. `typeCheck(ast, options)`
4. `runRaigai(source, initialVars, options)`

Core goals:
- Human-readable statement syntax (`let`, `fn`, `if`, `while`, `for in`)
- Infix operators instead of function-name chaining
- Static + runtime type safety
- No `eval`, no `new Function`
- **No infinite-loop mitigation**

## 2. Public API
Exports:
- `tokenize(src)`
- `parse(source)`
- `parseTypeString(typeSource)`
- `typeCheck(ast, options?)`
- `compile(ast, options?)`
- `runRaigai(source, initialVars?, options?)`
- `createRaigaiRunner(source, options?)`
- `clearRaigaiCache()`

### 2.1 runRaigai
```js
runRaigai(source, initialVars = {}, options = {})
```
Returns:
```js
{
  value,
  vars,
  output,
  ast
}
```

## 3. Lexical Syntax
- Whitespace: `\s`
- Line comment: `// ...`
- Block comment: `/* ... */`
- String: `'...'` or `"..."`
- Number: signed integer/float/exponent

## 4. Program Syntax (non-compatible with Gaigai)
Raigai is statement-oriented and requires semicolons for simple statements.

### 4.1 Statements
- Variable declaration:
```raigai
let x: number = 10;
const name: string = "raigai";
```

- Function declaration:
```raigai
fn add(a: number, b: number): number => a + b;

fn square(n: number): number {
  return n * n;
}
```

- Control flow:
```raigai
if (cond) { ... } else { ... }
while (cond) { ... }
for (item in listExpr) { ... }
```

- Return:
```raigai
return expr;
```

### 4.2 Expressions
- Arithmetic: `+ - * / %`
- Comparison: `< <= > >= == !=`
- Logical: `&& || !`
- Assignment: `= += -= *= /=`
- Range sugar: `1..5` (inclusive integer list)
- Pipeline sugar:
  - `x |> f`
  - `x |> f(y)` (equivalent to `f(x, y)`)
- Array literal and indexing:
  - `[1,2,3]`
  - `arr[0]`

## 5. Type System
Available types:
- Primitive: `number`, `string`, `bool`, `null`, `void`, `any`
- List: `list<T>` or `[T]`
- Function (internal)

Type safety layers:
1. Static check via `typeCheck`
2. Runtime checks for assignments, function args, and function returns (enabled by default)

`strictTypes: false` disables runtime type enforcement.

## 6. Builtins
- `print(any): void`
- `len(any): number` (string/list)
- `toString(any): string`
- `sum(list<number>): number`

## 7. Options
Supported options in `compile` / `runRaigai` / `createRaigaiRunner`:
- `strictTypes` (default `true`)
- `loggerFn` (default `console.log`)
- `maxOutput` (default `Infinity`)
- `globalTypes` (optional predeclared external variable types)
- `cache` (`runRaigai` only, default `true`)

## 8. Design Notes
- Raigai is intentionally not compatible with Gaigai syntax or built-in naming style.
- Program shape moved from single-expression to multi-statement for readability.
- Syntactic sugar (`..`, `|>`, `+=`) is first-class.
- Infinite-loop countermeasures are intentionally not included.
