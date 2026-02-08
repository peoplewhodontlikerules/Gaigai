# Gaigai Language Specification

## 1. Overview
Gaigai is a compact expression language implemented in JavaScript.
Execution flow:

1. `tokenize(source)`
2. `parse(source)` -> AST (single expression)
3. `compile(ast, options)` -> closure `(ctx) => value`
4. `runGaigai(source, initialVars, compileOptions)`

Design constraints preserved:

- Function-call-centric language shape.
- Program is exactly one expression.
- Side effects are provided by builtins.
- Runtime variables are stored in `ctx.vars`.
- No `eval`, no `new Function`.
- No infinite-loop guard.
- Backward compatibility is not required.

## 2. Public API
Exports:

- `tokenize(src)`
- `parse(source)`
- `compile(ast, options?)`
- `runGaigai(source, initialVars?, compileOptions?)`
- `createGaigaiRunner(source, compileOptions?)`
- `clearGaigaiCache()`

### 2.1 `runGaigai`
```js
runGaigai(source, initialVars = {}, compileOptions = {})
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

Runtime context:

```js
ctx = {
  vars: { ...initialVars },
  output: []
}
```

### 2.2 `createGaigaiRunner`
```js
const runner = createGaigaiRunner(source, compileOptions)
const result = runner(initialVars)
```
Compiles once and reuses AST/program for repeated execution.

### 2.3 `clearGaigaiCache`
Clears the internal source+options program cache used by `runGaigai`.

## 3. Lexical Syntax

### 3.1 Whitespace and comments
- Whitespace: `\s`
- Line comment: `// ...`
- Block comment: `/* ... */`

### 3.2 Tokens
- Punctuation: `(` `)` `,`
- Identifier: `[A-Za-z_][A-Za-z0-9_]*`
- Number: `-?digits(.digits)?([eE][+-]?digits)?`
- Strings: single or double quoted
  - escapes: `\\`, `\'`, `\"`, `\n`, `\t`, `\r`, `\uXXXX`
- Keywords: `true`, `false`, `null`, `undefined`

Token fields: `type`, `value`, `pos`, `line`, `col`.

## 4. Grammar and AST
Single-expression grammar:

```ebnf
program   := expr EOF
expr      := literal
          | identifier call_suffix?
          | "(" tuple_or_group ")"

call_suffix := "(" [expr ("," expr)* [","]] ")"
tuple_or_group :=
    /* empty */
  | expr ("," expr)* [","]
```

AST nodes:
- `Number`, `String`, `Boolean`, `Null`, `Undefined`
- `Var`
- `Call`
- `Tuple`

Tuple behavior:
- `()` -> empty tuple (`[]`)
- `(a,b)` -> tuple
- `(a,)` -> single-element tuple
- `(a)` -> grouped expression (not tuple node)

## 5. Options and Runtime Semantics
`compile(ast, options)` options:
- `strictVars` (default `false`)
- `strictArity` (default `false`)
- `loggerFn` (default `console.log`)
- `maxOutput` (default `Infinity`)

`runGaigai(..., compileOptions)` supports compile options plus:
- `cache` (default `true`)

Cache key includes source + strict flags + `maxOutput` + `loggerFn` identity.

## 6. OOP Runtime Model
Gaigai includes an object-oriented runtime model.

Class value:
- kind tag
- `name`
- `base` (nullable)
- `methods`
- `staticMethods`
- `staticFields`

Instance value:
- kind tag
- `cls`
- `fields`

Dispatch:
- `caller(instance, method, ...args)` -> inherited instance method dispatch, called as `fn(self, ...args)`
- `caller(class, method, ...args)` -> inherited static method dispatch, called as `fn(cls, ...args)`
- `superCaller(instance, method, ...args)` -> lookup starts at base class
- `newer(class, ...args)`/`constructer(class, ...args)` -> creates instance and optionally runs `init`

## 7. Builtins
Current implementation has 233 builtins.

### 7.1 Core / variables
- `mainner`, `returner`, `assigner`, `deleter`, `exister`

### 7.2 Numeric / logic / statistics / date-time
- Arithmetic and comparison:
  - `adder`, `subtractor`, `multiplier`, `divider`, `modder`, `powerer`, `inverter`
  - `lesser`, `lessEqualer`, `greater`, `greaterEqualer`, `equaler`, `notequaler`
  - `noter`, `ander`, `orer`, `nander`, `xorer`
- Numeric helpers:
  - `miner`, `maxer`, `abser`, `floorrer`, `ceiler`, `rounder`, `truncer`, `sqrter`, `signer`
  - `siner`, `coser`, `taner`, `loger`, `expeer`, `clampper`
  - `hypotter`, `deg2rader`, `rad2deger`, `lerper`, `sigmoider`, `tanher`
  - `randomer`, `randomInter`
- Statistics over numeric arrays:
  - `sumer`, `proder`, `meaner`, `medianer`, `varianceer`, `stddever`
- Date/time:
  - `nower`, `isoNower`, `dater`, `dateParseer`, `dateAdder`

### 7.3 Type / casting predicates
- `typeer`, `stringer`, `numberer`, `inter`, `booler`
- `isNaner`, `isFiniteer`, `isIntegerer`, `isArrayer`, `isObjecter`, `isFunctioner`

### 7.4 Control flow / errors
- `ifer`, `whiler`, `forrer`, `timeser`, `switcher`
- `incrementer`, `decrementer`
- `defaultor`, `coalescer`
- `tryer`, `thrower`

### 7.5 IO
- `logger`

### 7.6 Tuple/array utilities
- Basic:
  - `lengther`, `indexer`, `sliceer`, `joiner`, `concatener`
  - `pusher`, `popper`, `unshifter`, `shifter`, `setIndexer`
  - `cloner`, `deepCloner`, `rangeer`
- Functional:
  - `mapper`, `filterer`, `reducer`, `everyer`, `somer`
  - `finder`, `findIndexer`, `findLaster`, `findLastIndexer`
  - `includer`, `flatter`, `flatMaper`
- Ordering and set-like:
  - `sorter`, `reverser`, `uniquer`, `rotateer`, `shuffleer`, `sampleer`
- Structure transforms:
  - `chunker`, `zipper`, `unzipper`, `partitioner`, `groupByer`, `countByer`, `plucker`
- Access/mutation helpers:
  - `firster`, `laster`, `inserter`, `splicer`

### 7.7 Object utilities
- `objecter`, `geter`, `seter`, `keyser`, `valuer`
- `merger`, `deepMerger`
- `haser`, `delKeyer`
- `entrieser`, `fromEntrieser`
- Path ops:
  - `pathGeter`, `pathSeter`, `pathDeleter`
- Shape ops:
  - `picker`, `omitter`, `mapObjecter`, `filterObjecter`

### 7.8 OOP builtins
- Class construction:
  - `classer`, `subclasser`
- Instantiation and dispatch:
  - `newer`, `constructer`, `caller`, `superCaller`, `bindMethoder`
- Dynamic class edits:
  - `methoder`, `staticMethoder`
- Instance fields:
  - `getFielder`, `setFielder`, `hasFielder`, `delFielder`, `fieldKeyser`
- Static fields:
  - `staticGeter`, `staticSeter`, `staticKeyser`
- Introspection:
  - `classNameer`, `instanceOfer`, `methodNameser`, `isClasser`, `isInstanceer`, `classBaseer`, `classChainser`

### 7.9 Function values and combinators
- `funner` (param list can be tuple or single variable)
- `applyer`, `spreadApplyer`
- `partialer`, `curryer`
- `pipeer`, `composeer`, `predicateNoter`
- `memoizer`, `onceer`
- `tapper`, `constanter`

### 7.10 String / JSON / regex / encoding
- String basics:
  - `replacer`, `spliter`, `trimmer`, `lowerer`, `upperer`
  - `includeser`, `startsWither`, `endsWither`, `repeater`
  - `padStarter`, `padEnder`, `substringer`, `charAter`, `charCodeer`, `fromCodePointerer`
- JSON:
  - `jsonStringifyer`, `jsonParser`
- Regex (pattern and flags are strings):
  - `regexTester`, `regexMatcher`, `regexAller`, `regexReplacer`, `regexSpliter`, `regexEscapeer`
- URL and Base64:
  - `urlEncodeer`, `urlDecodeer`, `base64Encoder`, `base64Decoder`

Note: `base64Encoder/base64Decoder` require `Buffer` in the current runtime.

### 7.11 Matrix / vector / linear algebra
- Construction and shape:
  - `matrixer`, `shaper`, `ismatrixer`
- Arithmetic:
  - `matAdder`, `matSubtractor`, `matHadamarder`, `matScaler`
  - `matMultiplier`, `matVecMultiplier`, `transposer`
- Systems and decomposition helpers:
  - `eyer`, `detter`, `solver`, `ranker`
  - `matPowerer`, `matInverser`
- Access:
  - `matIndexer`, `matSeter`, `rower`, `columner`, `diagVectorer`
- Vector/matrix analytics:
  - `tracer`, `dotter`, `normer`, `matFlattener`, `outerProducter`, `kroneckerer`
- Higher-order matrix ops:
  - `matMapper`, `matReducer`

## 8. Error Behavior
- Syntax errors contain absolute position + line/column + caret excerpt.
- Runtime failures throw `Error`.
- Unknown function calls throw `Unknown function '<name>'`.
- Unknown AST node types throw runtime error.
- `strictVars` and `strictArity` adjust validation behavior.

## 9. Optimization Notes
Current optimizations include:
- Fast block-comment skipping (`indexOf`).
- Shared helper aliases and utility functions in compiler.
- Program-level cache in `runGaigai`.
- Reusable precompiled runners via `createGaigaiRunner`.

## 10. Non-Goals
- Backward compatibility with old Gaigai revisions.
- Loop/time-limit safety mechanisms.
