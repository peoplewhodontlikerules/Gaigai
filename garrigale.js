// garrigale.js
// Garrigale: tokenizer -> parser(AST) -> closure-compiler -> run
// No eval, no new Function.

"use strict";

/* ---------------------------
 * Source location helpers
 * --------------------------- */
function buildLineMap(src) {
  const lineStarts = [0];
  for (let i = 0; i < src.length; i++) if (src[i] === "\n") lineStarts.push(i + 1);
  return lineStarts;
}

function posToLineCol(lineStarts, pos) {
  let lo = 0, hi = lineStarts.length - 1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (lineStarts[mid] <= pos) lo = mid + 1;
    else hi = mid - 1;
  }
  const line = Math.max(0, hi);
  const col = pos - lineStarts[line];
  return { line: line + 1, col: col + 1 };
}

function formatAround(src, pos, radius = 40) {
  const a = Math.max(0, pos - radius);
  const b = Math.min(src.length, pos + radius);
  const frag = src.slice(a, b).replace(/\t/g, "  ");
  const caretPos = pos - a;
  return `${frag}\n${" ".repeat(Math.max(0, caretPos))}^`;
}

function makeSyntaxError(src, lineStarts, pos, msg) {
  const lc = posToLineCol(lineStarts, pos);
  const around = formatAround(src, pos);
  return new SyntaxError(`${msg} at ${pos} (line ${lc.line}, col ${lc.col})\n${around}`);
}

/* ---------------------------
 * Tokenizer
 * --------------------------- */
function tokenize(src) {
  const tokens = [];
  const lineStarts = buildLineMap(src);

  let i = 0;

  const isSpace = (c) => /\s/.test(c);
  const isIdStart = (c) => /[A-Za-z_]/.test(c); // 意図的にシンプル（特徴維持）
  const isId = (c) => /[A-Za-z0-9_]/.test(c);
  const isDigit = (c) => /[0-9]/.test(c);

  const err = (msg, pos = i) => {
    throw makeSyntaxError(src, lineStarts, pos, msg);
  };

  const push = (type, value, pos) => {
    const lc = posToLineCol(lineStarts, pos);
    tokens.push({ type, value, pos, line: lc.line, col: lc.col });
  };

  const readString = (quote) => {
    const startPos = i;
    i++; // skip quote
    let s = "";
    while (i < src.length) {
      const ch = src[i];
      if (ch === "\\") {
        const next = src[i + 1];
        if (next === quote || next === "\\" || next === "n" || next === "t" || next === "r") {
          if (next === "n") s += "\n";
          else if (next === "t") s += "\t";
          else if (next === "r") s += "\r";
          else s += next;
          i += 2;
          continue;
        }
        if (next === "u") {
          const hex = src.slice(i + 2, i + 6);
          if (/^[0-9A-Fa-f]{4}$/.test(hex)) {
            s += String.fromCharCode(parseInt(hex, 16));
            i += 6;
            continue;
          }
        }
        s += ch; // keep backslash as-is
        i++;
        continue;
      }
      if (ch === quote) {
        i++;
        return { type: "string", value: s, pos: startPos };
      }
      s += ch;
      i++;
    }
    err("Unterminated string literal", startPos);
  };

  const scanNumberEnd = (startPos) => {
    // supports: -?digits(.digits)?([eE][+-]?digits)?
    let j = startPos;
    if (src[j] === "-") j++;

    const digitsStart = j;
    while (j < src.length && isDigit(src[j])) j++;
    if (j === digitsStart) err("Invalid number literal", startPos);

    if (src[j] === ".") {
      j++;
      while (j < src.length && isDigit(src[j])) j++;
      // allow "12." but disallow "-." etc (already prevented)
    }

    if (src[j] === "e" || src[j] === "E") {
      j++;
      if (src[j] === "+" || src[j] === "-") j++;
      const expStart = j;
      while (j < src.length && isDigit(src[j])) j++;
      if (expStart === j) err("Invalid exponent in number literal", startPos);
    }
    return j;
  };

  while (i < src.length) {
    const c = src[i];

    if (isSpace(c)) {
      i++;
      continue;
    }

    // line comment: //
    if (c === "/" && src[i + 1] === "/") {
      i += 2;
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }

    // block comment: /* ... */
    if (c === "/" && src[i + 1] === "*") {
      const startPos = i;
      const end = src.indexOf("*/", i + 2);
      if (end === -1) err("Unterminated block comment", startPos);
      i = end + 2;
      continue;
    }

    // punctuation
    if (c === "(" || c === ")" || c === ",") {
      push(c, c, i);
      i++;
      continue;
    }

    // string literal
    if (c === '"' || c === "'") {
      const t = readString(c);
      push(t.type, t.value, t.pos);
      continue;
    }

    // number
    if ((c === "-" && isDigit(src[i + 1] || "")) || isDigit(c)) {
      const startPos = i;
      const end = scanNumberEnd(startPos);
      const numStr = src.slice(startPos, end);
      const n = Number(numStr);
      if (Number.isNaN(n)) err(`Invalid number literal '${numStr}'`, startPos);
      push("number", n, startPos);
      i = end;
      continue;
    }

    // identifier / keywords
    if (isIdStart(c)) {
      const startPos = i;
      let j = i + 1;
      while (j < src.length && isId(src[j])) j++;
      const id = src.slice(i, j);

      if (id === "true" || id === "false") {
        push("boolean", id === "true", startPos);
      } else if (id === "null") {
        push("null", null, startPos);
      } else if (id === "undefined") {
        push("undefined", undefined, startPos);
      } else {
        push("id", id, startPos);
      }

      i = j;
      continue;
    }

    err(`Unexpected character '${c}'`);
  }

  push("eof", "", i);
  return { tokens, lineStarts, src };
}

/* ---------------------------
 * Parser (AST)
 * - 1つの式だけを読む（特徴維持）
 * --------------------------- */
function parse(source) {
  const tk = tokenize(source);
  const tokens = tk.tokens;
  const lineStarts = tk.lineStarts;
  const src = tk.src;

  let pos = 0;
  const peek = () => tokens[pos];
  const next = () => tokens[pos++];

  const syntaxErr = (t, msg) => {
    const p = t?.pos ?? -1;
    throw makeSyntaxError(src, lineStarts, p, `${msg} (token: '${t?.type ?? "?"}')`);
  };

  const expect = (type) => {
    const t = next();
    if (t.type !== type) syntaxErr(t, `Expected '${type}', got '${t.type}'`);
    return t;
  };

  function parseExpr() {
    const t = peek();

    if (t.type === "number") {
      next();
      return { type: "Number", value: t.value, pos: t.pos };
    }
    if (t.type === "string") {
      next();
      return { type: "String", value: t.value, pos: t.pos };
    }
    if (t.type === "boolean") {
      next();
      return { type: "Boolean", value: t.value, pos: t.pos };
    }
    if (t.type === "null") {
      next();
      return { type: "Null", value: null, pos: t.pos };
    }
    if (t.type === "undefined") {
      next();
      return { type: "Undefined", value: undefined, pos: t.pos };
    }

    if (t.type === "id") {
      const idTok = next();
      if (peek().type === "(") {
        expect("(");
        const args = [];
        if (peek().type !== ")") {
          args.push(parseExpr());
          while (peek().type === ",") {
            next();
            if (peek().type === ")") break; // trailing comma
            args.push(parseExpr());
          }
        }
        expect(")");
        return { type: "Call", name: idTok.value, args, pos: idTok.pos };
      }
      return { type: "Var", name: idTok.value, pos: idTok.pos };
    }

    if (t.type === "(") {
      const open = expect("(");
      if (peek().type === ")") {
        expect(")");
        return { type: "Tuple", items: [], pos: open.pos };
      }
      const items = [parseExpr()];
      let sawComma = false;
      while (peek().type === ",") {
        sawComma = true;
        next();
        if (peek().type === ")") break; // trailing comma
        items.push(parseExpr());
      }
      expect(")");
      if (items.length === 1 && !sawComma) return items[0];
      return { type: "Tuple", items, pos: open.pos };
    }

    syntaxErr(t, `Unexpected token '${t.type}'`);
  }

  const ast = parseExpr();
  if (peek().type !== "eof") syntaxErr(peek(), `Unexpected trailing token '${peek().type}'`);
  return ast;
}

/* ---------------------------
 * Closure Compiler
 * AST -> (ctx) => value
 * --------------------------- */
function compile(ast, options = {}) {
  const {
    strictVars = false, // undefined var read throws
    loggerFn = console.log,
    maxOutput = Infinity,
    strictArity = true, // non-compatible by default
    maxOps = 1_000_000, // total operation budget
    maxRuntimeMs = 2_000, // hard runtime budget per execution
    maxLoopIters = 100_000, // total loop iterations budget
    maxLoopStallIters = 2_048, // same-state while-loop guard
    maxCallDepth = 256, // function recursion/call depth guard
    maxCalls = 200_000, // total function call budget
    maxInferenceSteps = 4_096, // forward-chaining inference budget
    maxConstraintNodes = 50_000, // CSP search-node budget
    externals = Object.create(null), // non-global external call table
  } = options;
  const hasOwn = Object.prototype.hasOwnProperty;
  const isFiniteNonNeg = (n) => typeof n === "number" && Number.isFinite(n) && n >= 0;
  const maxOpsLimit = isFiniteNonNeg(Number(maxOps)) ? Number(maxOps) : 1_000_000;
  const maxRuntimeLimit = isFiniteNonNeg(Number(maxRuntimeMs)) ? Number(maxRuntimeMs) : 2_000;
  const maxLoopLimit = isFiniteNonNeg(Number(maxLoopIters)) ? Number(maxLoopIters) : 100_000;
  const maxLoopStallLimit = maxLoopStallIters === Infinity
    ? Infinity
    : isFiniteNonNeg(Number(maxLoopStallIters))
      ? Number(maxLoopStallIters)
      : 2_048;
  const maxCallDepthLimit = isFiniteNonNeg(Number(maxCallDepth)) ? Math.max(1, Number(maxCallDepth) | 0) : 256;
  const maxCallsLimit = isFiniteNonNeg(Number(maxCalls)) ? Number(maxCalls) : 200_000;
  const maxInferenceStepLimit = maxInferenceSteps === Infinity
    ? Infinity
    : isFiniteNonNeg(Number(maxInferenceSteps))
      ? Number(maxInferenceSteps)
      : 4_096;
  const maxConstraintNodeLimit = maxConstraintNodes === Infinity
    ? Infinity
    : isFiniteNonNeg(Number(maxConstraintNodes))
      ? Number(maxConstraintNodes)
      : 50_000;
  const externalFns = externals && typeof externals === "object" ? externals : Object.create(null);

  const ensureGuard = (ctx) => {
    if (!ctx || typeof ctx !== "object") throw new Error("runtime context is required");
    if (!ctx.__GarrigaleGuard || typeof ctx.__GarrigaleGuard !== "object") {
      ctx.__GarrigaleGuard = {
        startedAtMs: Date.now(),
        ops: 0,
        loops: 0,
        calls: 0,
        callDepth: 0,
      };
    }
    return ctx.__GarrigaleGuard;
  };

  const checkRuntime = (ctx, phase) => {
    if (maxRuntimeLimit === Infinity) return;
    const g = ensureGuard(ctx);
    const elapsed = Date.now() - g.startedAtMs;
    if (elapsed > maxRuntimeLimit) {
      throw new Error(`Execution timeout in ${phase}: ${elapsed}ms > ${maxRuntimeLimit}ms`);
    }
  };

  const chargeOps = (ctx, phase = "operation", cost = 1) => {
    const g = ensureGuard(ctx);
    g.ops += cost;
    if (g.ops > maxOpsLimit) {
      throw new Error(`Operation budget exceeded in ${phase}: ${g.ops} > ${maxOpsLimit}`);
    }
    checkRuntime(ctx, phase);
  };

  const chargeLoop = (ctx, phase = "loop") => {
    const g = ensureGuard(ctx);
    g.loops += 1;
    if (g.loops > maxLoopLimit) {
      throw new Error(`Loop iteration budget exceeded in ${phase}: ${g.loops} > ${maxLoopLimit}`);
    }
    chargeOps(ctx, phase, 1);
  };

  const enterCall = (ctx, phase = "call") => {
    const g = ensureGuard(ctx);
    g.calls += 1;
    g.callDepth += 1;
    if (g.calls > maxCallsLimit) {
      g.callDepth = Math.max(0, g.callDepth - 1);
      throw new Error(`Call budget exceeded in ${phase}: ${g.calls} > ${maxCallsLimit}`);
    }
    if (g.callDepth > maxCallDepthLimit) {
      const exceeded = g.callDepth;
      g.callDepth = Math.max(0, g.callDepth - 1);
      throw new Error(`Call depth exceeded in ${phase}: ${exceeded} > ${maxCallDepthLimit}`);
    }
    checkRuntime(ctx, phase);
  };

  const leaveCall = (ctx) => {
    const g = ensureGuard(ctx);
    g.callDepth = Math.max(0, g.callDepth - 1);
  };

  const invokeGuarded = (ctx, fn, args, phase = "call") => {
    enterCall(ctx, phase);
    try {
      return fn(...args);
    } finally {
      leaveCall(ctx);
    }
  };

  const toInt = (x) => {
    if (typeof x === "number") return x | 0;
    const n = Number(x);
    return Number.isFinite(n) ? (n | 0) : 0;
  };

  const toNumber = (x) => {
    if (typeof x === "number") return x;
    const n = Number(x);
    return Number.isFinite(n) ? n : NaN;
  };

  const isObj = (v) => v !== null && typeof v === "object";
  const isFn = (v) => typeof v === "function";
  const isArr = Array.isArray;
  const asArray = (v, name = "value") => {
    if (!isArr(v)) throw new Error(`${name}: expected an array (tuple)`);
    return v;
  };
  const normalizeDepth = (x) => {
    if (x === undefined) return 1;
    const n = toInt(x);
    return n < 0 ? 0 : n;
  };
  const toFiniteNumber = (x, name) => {
    const n = toNumber(x);
    if (!Number.isFinite(n)) throw new Error(`${name}: expected a finite number`);
    return n;
  };
  const deepClone = (v) => {
    if (isArr(v)) return v.map((it) => deepClone(it));
    if (isObj(v)) {
      const out = Object.create(null);
      for (const k of Object.keys(v)) out[k] = deepClone(v[k]);
      return out;
    }
    return v;
  };
  const flattenWithDepth = (arr, depth = 1) => {
    const out = [];
    const push = (v, d) => {
      if (d > 0 && isArr(v)) {
        for (let i = 0; i < v.length; i++) push(v[i], d - 1);
      } else {
        out.push(v);
      }
    };
    push(arr, depth);
    return out;
  };
  const sortArray = (arr, mode) => {
    const cp = arr.slice();
    if (mode === "desc") {
      cp.sort((a, b) => {
        const na = toNumber(a);
        const nb = toNumber(b);
        if (Number.isNaN(na) || Number.isNaN(nb)) return String(b).localeCompare(String(a));
        return nb - na;
      });
      return cp;
    }
    cp.sort((a, b) => {
      const na = toNumber(a);
      const nb = toNumber(b);
      if (Number.isNaN(na) || Number.isNaN(nb)) return String(a).localeCompare(String(b));
      return na - nb;
    });
    return cp;
  };

  const safeGetVar = (ctx, name, pos) => {
    if (hasOwn.call(ctx.vars, name)) return ctx.vars[name];
    if (strictVars) throw new Error(`Undefined variable '${name}' (at ${pos ?? "?"})`);
    return undefined;
  };

  const arity = (name, got, min, max = min) => {
    if (!strictArity) return;
    if (got < min || got > max) {
      throw new Error(`Arity error: '${name}' expects ${min}${max !== min ? ".." + max : ""} args, got ${got}`);
    }
  };

  /* ---------------------------
   * Matrix helpers (new)
   * - 行列は「配列の配列」(number 以外は numberer 的に Number(...) へ寄せる)
   * - Garrigale の tuple (= JS Array) をそのまま使う
   * --------------------------- */
  const isMatrix = (v) => {
    if (!Array.isArray(v)) return false;
    if (v.length === 0) return true; // 0x0 として許す
    if (!Array.isArray(v[0])) return false;
    const cols = v[0].length;
    for (let r = 0; r < v.length; r++) {
      if (!Array.isArray(v[r])) return false;
      if (v[r].length !== cols) return false;
    }
    return true;
  };

  const matrixShape = (m) => {
    if (!Array.isArray(m)) return { rows: 0, cols: 0 };
    const rows = m.length;
    const cols = rows === 0 ? 0 : (Array.isArray(m[0]) ? m[0].length : 0);
    return { rows, cols };
  };

  const asMatrix = (v, name = "matrix") => {
    if (!isMatrix(v)) throw new Error(`${name}: expected matrix (array of equal-length rows)`);
    return v;
  };

  const cloneMatrix = (m) => m.map((row) => row.slice());

  const mapMatrix = (m, fn) => {
    const out = new Array(m.length);
    for (let r = 0; r < m.length; r++) {
      const row = m[r];
      const outRow = new Array(row.length);
      for (let c = 0; c < row.length; c++) outRow[c] = fn(row[c], r, c);
      out[r] = outRow;
    }
    return out;
  };

  const zipMatrix = (a, b, fn, opname) => {
    a = asMatrix(a, opname);
    b = asMatrix(b, opname);
    const sa = matrixShape(a);
    const sb = matrixShape(b);
    if (sa.rows !== sb.rows || sa.cols !== sb.cols) {
      throw new Error(`${opname}: shape mismatch (${sa.rows}x${sa.cols}) vs (${sb.rows}x${sb.cols})`);
    }
    const out = new Array(sa.rows);
    for (let r = 0; r < sa.rows; r++) {
      const outRow = new Array(sa.cols);
      for (let c = 0; c < sa.cols; c++) outRow[c] = fn(a[r][c], b[r][c], r, c);
      out[r] = outRow;
    }
    return out;
  };

  const transpose = (m) => {
    m = asMatrix(m, "transposer");
    const { rows, cols } = matrixShape(m);
    const out = new Array(cols);
    for (let c = 0; c < cols; c++) {
      const row = new Array(rows);
      for (let r = 0; r < rows; r++) row[r] = m[r][c];
      out[c] = row;
    }
    return out;
  };

  const matMul = (a, b) => {
    a = asMatrix(a, "matMultiplier");
    b = asMatrix(b, "matMultiplier");
    const sa = matrixShape(a);
    const sb = matrixShape(b);
    if (sa.cols !== sb.rows) {
      throw new Error(`matMultiplier: shape mismatch (${sa.rows}x${sa.cols}) * (${sb.rows}x${sb.cols})`);
    }
    const out = new Array(sa.rows);
    for (let r = 0; r < sa.rows; r++) {
      const outRow = new Array(sb.cols);
      for (let c = 0; c < sb.cols; c++) {
        let acc = 0;
        for (let k = 0; k < sa.cols; k++) acc += toNumber(a[r][k]) * toNumber(b[k][c]);
        outRow[c] = acc;
      }
      out[r] = outRow;
    }
    return out;
  };

  const matVecMul = (a, v) => {
    a = asMatrix(a, "matVecMultiplier");
    if (!Array.isArray(v) || (v.length > 0 && Array.isArray(v[0]))) {
      throw new Error("matVecMultiplier: second arg must be a vector (1D tuple/array)");
    }
    const sa = matrixShape(a);
    if (v.length !== sa.cols) {
      throw new Error(`matVecMultiplier: shape mismatch (${sa.rows}x${sa.cols}) * (${v.length})`);
    }
    const out = new Array(sa.rows);
    for (let r = 0; r < sa.rows; r++) {
      let acc = 0;
      for (let k = 0; k < sa.cols; k++) acc += toNumber(a[r][k]) * toNumber(v[k]);
      out[r] = acc;
    }
    return out;
  };

  const identity = (n) => {
    n = toInt(n);
    if (n < 0) throw new Error("eyer: n must be >= 0");
    const out = new Array(n);
    for (let r = 0; r < n; r++) {
      const row = new Array(n);
      for (let c = 0; c < n; c++) row[c] = r === c ? 1 : 0;
      out[r] = row;
    }
    return out;
  };

  const det = (m) => {
    m = asMatrix(m, "detter");
    const { rows, cols } = matrixShape(m);
    if (rows !== cols) throw new Error(`detter: must be square, got ${rows}x${cols}`);
    const n = rows;
    if (n === 0) return 1;

    // Gaussian elimination with partial pivoting
    const a = cloneMatrix(m).map((row) => row.map((x) => toNumber(x)));
    let sign = 1;
    let detVal = 1;

    for (let i = 0; i < n; i++) {
      // pivot
      let pivotRow = i;
      let maxAbs = Math.abs(a[i][i]);
      for (let r = i + 1; r < n; r++) {
        const v = Math.abs(a[r][i]);
        if (v > maxAbs) {
          maxAbs = v;
          pivotRow = r;
        }
      }

      if (maxAbs === 0 || !Number.isFinite(maxAbs)) return 0;

      if (pivotRow !== i) {
        const tmp = a[i];
        a[i] = a[pivotRow];
        a[pivotRow] = tmp;
        sign *= -1;
      }

      const pivot = a[i][i];
      detVal *= pivot;

      // eliminate below
      for (let r = i + 1; r < n; r++) {
        const factor = a[r][i] / pivot;
        if (factor === 0) continue;
        for (let c = i; c < n; c++) a[r][c] -= factor * a[i][c];
      }
    }
    return sign * detVal;
  };

  const solve = (A, b) => {
    A = asMatrix(A, "solver");
    const { rows, cols } = matrixShape(A);
    if (rows !== cols) throw new Error(`solver: A must be square, got ${rows}x${cols}`);

    // b can be vector (n) or matrix (n x k)
    const n = rows;

    let B;
    let isVec = false;
    if (Array.isArray(b) && (b.length === 0 || !Array.isArray(b[0]))) {
      // vector
      if (b.length !== n) throw new Error(`solver: b length mismatch, expected ${n}, got ${b.length}`);
      B = b.map((x) => [toNumber(x)]);
      isVec = true;
    } else {
      B = asMatrix(b, "solver");
      const sb = matrixShape(B);
      if (sb.rows !== n) throw new Error(`solver: b rows mismatch, expected ${n}, got ${sb.rows}`);
      B = B.map((row) => row.map((x) => toNumber(x)));
    }

    // Augmented matrix [A | B]
    const k = B[0]?.length ?? 0;
    const M = new Array(n);
    for (let r = 0; r < n; r++) {
      const row = new Array(n + k);
      for (let c = 0; c < n; c++) row[c] = toNumber(A[r][c]);
      for (let c = 0; c < k; c++) row[n + c] = B[r][c];
      M[r] = row;
    }

    // Gauss-Jordan
    for (let col = 0; col < n; col++) {
      // pivot
      let pivotRow = col;
      let maxAbs = Math.abs(M[col][col]);
      for (let r = col + 1; r < n; r++) {
        const v = Math.abs(M[r][col]);
        if (v > maxAbs) {
          maxAbs = v;
          pivotRow = r;
        }
      }
      if (maxAbs === 0 || !Number.isFinite(maxAbs)) throw new Error("solver: singular matrix");

      if (pivotRow !== col) {
        const tmp = M[col];
        M[col] = M[pivotRow];
        M[pivotRow] = tmp;
      }

      const pivot = M[col][col];
      for (let c = col; c < n + k; c++) M[col][c] /= pivot;

      for (let r = 0; r < n; r++) {
        if (r === col) continue;
        const factor = M[r][col];
        if (factor === 0) continue;
        for (let c = col; c < n + k; c++) M[r][c] -= factor * M[col][c];
      }
    }

    const X = new Array(n);
    for (let r = 0; r < n; r++) {
      const row = new Array(k);
      for (let c = 0; c < k; c++) row[c] = M[r][n + c];
      X[r] = row;
    }

    if (isVec) return X.map((row) => row[0]);
    return X;
  };

  const isVector = (v) => isArr(v) && (v.length === 0 || !isArr(v[0]));
  const asVector = (v, name = "vector") => {
    if (!isVector(v)) throw new Error(`${name}: expected vector (1D tuple/array)`);
    return v;
  };
  const dot = (a, b, name = "dotter") => {
    a = asVector(a, name);
    b = asVector(b, name);
    if (a.length !== b.length) throw new Error(`${name}: length mismatch (${a.length}) vs (${b.length})`);
    let acc = 0;
    for (let i = 0; i < a.length; i++) acc += toNumber(a[i]) * toNumber(b[i]);
    return acc;
  };
  const matTrace = (m) => {
    m = asMatrix(m, "tracer");
    const s = matrixShape(m);
    if (s.rows !== s.cols) throw new Error(`tracer: must be square, got ${s.rows}x${s.cols}`);
    let t = 0;
    for (let i = 0; i < s.rows; i++) t += toNumber(m[i][i]);
    return t;
  };
  const matPow = (m, exp) => {
    m = asMatrix(m, "matPowerer");
    const s = matrixShape(m);
    if (s.rows !== s.cols) throw new Error(`matPowerer: must be square, got ${s.rows}x${s.cols}`);
    let e = toInt(exp);
    if (e < 0) throw new Error("matPowerer: exponent must be >= 0");
    let base = mapMatrix(m, (x) => toNumber(x));
    let out = identity(s.rows);
    while (e > 0) {
      if (e & 1) out = matMul(out, base);
      e >>= 1;
      if (e > 0) base = matMul(base, base);
    }
    return out;
  };
  const matInverse = (m) => {
    m = asMatrix(m, "matInverser");
    const s = matrixShape(m);
    if (s.rows !== s.cols) throw new Error(`matInverser: must be square, got ${s.rows}x${s.cols}`);
    return solve(m, identity(s.rows));
  };
  const matrixRank = (m) => {
    m = asMatrix(m, "ranker");
    const s = matrixShape(m);
    const a = cloneMatrix(m).map((row) => row.map((x) => toNumber(x)));
    let rank = 0;
    let row = 0;
    const eps = 1e-12;
    for (let col = 0; col < s.cols && row < s.rows; col++) {
      let pivot = row;
      for (let r = row + 1; r < s.rows; r++) if (Math.abs(a[r][col]) > Math.abs(a[pivot][col])) pivot = r;
      if (Math.abs(a[pivot][col]) <= eps) continue;
      const tmp = a[row];
      a[row] = a[pivot];
      a[pivot] = tmp;
      const pv = a[row][col];
      for (let c = col; c < s.cols; c++) a[row][c] /= pv;
      for (let r = 0; r < s.rows; r++) {
        if (r === row) continue;
        const f = a[r][col];
        if (Math.abs(f) <= eps) continue;
        for (let c = col; c < s.cols; c++) a[r][c] -= f * a[row][c];
      }
      row++;
      rank++;
    }
    return rank;
  };
  const matOuter = (a, b) => {
    a = asVector(a, "outerProducter");
    b = asVector(b, "outerProducter");
    const out = new Array(a.length);
    for (let r = 0; r < a.length; r++) {
      const row = new Array(b.length);
      for (let c = 0; c < b.length; c++) row[c] = toNumber(a[r]) * toNumber(b[c]);
      out[r] = row;
    }
    return out;
  };
  const matrixRow = (m, r) => {
    m = asMatrix(m, "rower");
    const rr = toInt(r);
    return m[rr] ? m[rr].slice() : [];
  };
  const matrixCol = (m, c) => {
    m = asMatrix(m, "columner");
    const cc = toInt(c);
    const out = new Array(m.length);
    for (let r = 0; r < m.length; r++) out[r] = m[r]?.[cc];
    return out;
  };
  const matrixDiag = (m) => {
    m = asMatrix(m, "diagVectorer");
    const s = matrixShape(m);
    const n = Math.min(s.rows, s.cols);
    const out = new Array(n);
    for (let i = 0; i < n; i++) out[i] = m[i][i];
    return out;
  };
  const matrixKronecker = (a, b) => {
    a = asMatrix(a, "kroneckerer");
    b = asMatrix(b, "kroneckerer");
    const sa = matrixShape(a);
    const sb = matrixShape(b);
    const out = new Array(sa.rows * sb.rows);
    for (let ar = 0; ar < sa.rows; ar++) {
      for (let br = 0; br < sb.rows; br++) {
        const row = new Array(sa.cols * sb.cols);
        for (let ac = 0; ac < sa.cols; ac++) {
          for (let bc = 0; bc < sb.cols; bc++) row[ac * sb.cols + bc] = toNumber(a[ar][ac]) * toNumber(b[br][bc]);
        }
        out[ar * sb.rows + br] = row;
      }
    }
    return out;
  };
  const unzipObjectEntries = (entries, name = "fromEntrieser") => {
    const arr = asArray(entries, name);
    const out = Object.create(null);
    for (let i = 0; i < arr.length; i++) {
      const pair = arr[i];
      if (!isArr(pair) || pair.length < 2) throw new Error(`${name}: entry ${i} must be (key,value) tuple`);
      out[String(pair[0])] = pair[1];
    }
    return out;
  };

  /* ---------------------------
   * OOP helpers
   * --------------------------- */
  const Garrigale_CLASS_KIND = "Garrigale.class";
  const Garrigale_INSTANCE_KIND = "Garrigale.instance";

  const isPlainObject = (v) => isObj(v) && !isArr(v);
  const isGarrigaleClass = (v) => isPlainObject(v) && v.__GarrigaleKind === Garrigale_CLASS_KIND;
  const isGarrigaleInstance = (v) => isPlainObject(v) && v.__GarrigaleKind === Garrigale_INSTANCE_KIND;
  const asGarrigaleClass = (v, name = "class") => {
    if (!isGarrigaleClass(v)) throw new Error(`${name}: expected class object`);
    return v;
  };
  const asGarrigaleInstance = (v, name = "instance") => {
    if (!isGarrigaleInstance(v)) throw new Error(`${name}: expected instance object`);
    return v;
  };

  const normalizeCallableMap = (raw, name) => {
    if (raw === undefined || raw === null) return Object.create(null);
    if (!isPlainObject(raw)) throw new Error(`${name}: expected object map`);
    const out = Object.create(null);
    const keys = Object.keys(raw);
    for (let i = 0; i < keys.length; i++) {
      const k = keys[i];
      const fn = raw[k];
      if (!isFn(fn)) throw new Error(`${name}: '${k}' must be a function`);
      out[k] = fn;
    }
    return out;
  };

  const createClass = (nameValue, methodsValue, baseValue, staticMethodsValue) => {
    const name = String(nameValue ?? "AnonymousClass");
    const base = baseValue === undefined || baseValue === null ? null : asGarrigaleClass(baseValue, "classer(base)");
    const methods = normalizeCallableMap(methodsValue, "classer(methods)");
    const staticMethods = normalizeCallableMap(staticMethodsValue, "classer(statics)");
    return {
      __GarrigaleKind: Garrigale_CLASS_KIND,
      name,
      base,
      methods,
      staticMethods,
      staticFields: Object.create(null),
    };
  };

  const findClassMethod = (cls, methodName) => {
    let cur = cls;
    while (cur) {
      if (hasOwn.call(cur.methods, methodName)) return { owner: cur, fn: cur.methods[methodName] };
      cur = cur.base;
    }
    return null;
  };

  const findClassStaticMethod = (cls, methodName) => {
    let cur = cls;
    while (cur) {
      if (hasOwn.call(cur.staticMethods, methodName)) return { owner: cur, fn: cur.staticMethods[methodName] };
      cur = cur.base;
    }
    return null;
  };

  const findClassStaticField = (cls, fieldName) => {
    let cur = cls;
    while (cur) {
      if (hasOwn.call(cur.staticFields, fieldName)) return { owner: cur, value: cur.staticFields[fieldName] };
      cur = cur.base;
    }
    return null;
  };

  const makeInstance = (cls) => ({
    __GarrigaleKind: Garrigale_INSTANCE_KIND,
    cls,
    fields: Object.create(null),
  });

  const invokeClassMethod = (inst, methodName, args, superOnly = false) => {
    const self = asGarrigaleInstance(inst, "caller(target)");
    const mName = String(methodName);
    const startClass = superOnly ? self.cls.base : self.cls;
    if (!startClass) throw new Error(`superCaller: no base class for '${self.cls.name}'`);
    const found = findClassMethod(startClass, mName);
    if (!found) throw new Error(`caller: method '${mName}' not found on class '${self.cls.name}'`);
    return found.fn(self, ...args);
  };

  const invokeStaticMethod = (cls, methodName, args) => {
    const ccls = asGarrigaleClass(cls, "caller(target)");
    const mName = String(methodName);
    const found = findClassStaticMethod(ccls, mName);
    if (!found) throw new Error(`caller: static method '${mName}' not found on class '${ccls.name}'`);
    return found.fn(ccls, ...args);
  };

  const isInstanceOfClass = (inst, cls) => {
    if (!isGarrigaleInstance(inst) || !isGarrigaleClass(cls)) return false;
    let cur = inst.cls;
    while (cur) {
      if (cur === cls) return true;
      cur = cur.base;
    }
    return false;
  };

  const collectMethodNames = (cls, includeInherited = true) => {
    const names = [];
    const seen = Object.create(null);
    let cur = cls;
    while (cur) {
      const keys = Object.keys(cur.methods);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        if (!hasOwn.call(seen, k)) {
          seen[k] = true;
          names.push(k);
        }
      }
      if (!includeInherited) break;
      cur = cur.base;
    }
    return names;
  };

  const asNumberArray = (v, name = "array") => asArray(v, name).map((x) => toNumber(x));
  const numericSortAsc = (arr) => arr.slice().sort((a, b) => a - b);
  const meanOf = (arr) => (arr.length === 0 ? NaN : arr.reduce((a, b) => a + b, 0) / arr.length);
  const medianOf = (arr) => {
    if (arr.length === 0) return NaN;
    const s = numericSortAsc(arr);
    const mid = s.length >> 1;
    return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2;
  };
  const varianceOf = (arr, sample = false) => {
    if (arr.length === 0) return NaN;
    if (sample && arr.length < 2) return NaN;
    const m = meanOf(arr);
    let acc = 0;
    for (let i = 0; i < arr.length; i++) {
      const d = arr[i] - m;
      acc += d * d;
    }
    return acc / (sample ? arr.length - 1 : arr.length);
  };
  const normalizeIndex = (len, idx) => {
    const i = toInt(idx);
    if (i < 0) return Math.max(0, len + i);
    return Math.min(len, i);
  };
  const rotateArray = (arr, steps) => {
    const a = asArray(arr, "rotateer");
    const n = a.length;
    if (n === 0) return [];
    let k = toInt(steps) % n;
    if (k < 0) k += n;
    if (k === 0) return a.slice();
    return a.slice(n - k).concat(a.slice(0, n - k));
  };
  const shuffleArray = (arr) => {
    const a = asArray(arr, "shuffleer").slice();
    for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      const t = a[i];
      a[i] = a[j];
      a[j] = t;
    }
    return a;
  };
  const zipArrays = (arrays) => {
    const arrs = arrays.map((x, i) => asArray(x, `zipper(arg ${i})`));
    const minLen = arrs.reduce((m, a) => Math.min(m, a.length), Infinity);
    if (!Number.isFinite(minLen)) return [];
    const out = new Array(minLen);
    for (let i = 0; i < minLen; i++) out[i] = arrs.map((a) => a[i]);
    return out;
  };
  const groupByArray = (arr, keyer) => {
    const a = asArray(arr, "groupByer");
    const out = Object.create(null);
    for (let i = 0; i < a.length; i++) {
      const item = a[i];
      const key = String(keyer(item, i, a));
      if (!hasOwn.call(out, key)) out[key] = [];
      out[key].push(item);
    }
    return out;
  };
  const countByArray = (arr, keyer) => {
    const a = asArray(arr, "countByer");
    const out = Object.create(null);
    for (let i = 0; i < a.length; i++) {
      const key = String(keyer(a[i], i, a));
      out[key] = (out[key] ?? 0) + 1;
    }
    return out;
  };
  const parsePath = (path) => {
    if (isArr(path)) return path.map((x) => String(x));
    const s = String(path);
    if (s === "") return [];
    return s.split(".").filter((x) => x.length > 0);
  };
  const isPathIndex = (key) => /^-?\d+$/.test(key);
  const pathGet = (obj, path, fallback = undefined) => {
    const keys = parsePath(path);
    let cur = obj;
    for (let i = 0; i < keys.length; i++) {
      const k = keys[i];
      if (cur === null || cur === undefined) return fallback;
      const kk = isPathIndex(k) ? toInt(k) : k;
      cur = cur[kk];
    }
    return cur === undefined ? fallback : cur;
  };
  const pathSet = (obj, path, value) => {
    if (!isObj(obj)) throw new Error("pathSeter: first arg must be object/array");
    const keys = parsePath(path);
    if (keys.length === 0) throw new Error("pathSeter: path must not be empty");
    let cur = obj;
    for (let i = 0; i < keys.length - 1; i++) {
      const k = keys[i];
      const kk = isPathIndex(k) ? toInt(k) : k;
      const nk = keys[i + 1];
      if (!isObj(cur[kk])) cur[kk] = isPathIndex(nk) ? [] : Object.create(null);
      cur = cur[kk];
    }
    const last = keys[keys.length - 1];
    const lastKey = isPathIndex(last) ? toInt(last) : last;
    cur[lastKey] = value;
    return value;
  };
  const pathDelete = (obj, path) => {
    if (!isObj(obj)) return false;
    const keys = parsePath(path);
    if (keys.length === 0) return false;
    let cur = obj;
    for (let i = 0; i < keys.length - 1; i++) {
      const k = isPathIndex(keys[i]) ? toInt(keys[i]) : keys[i];
      cur = cur?.[k];
      if (!isObj(cur) && !isArr(cur)) return false;
    }
    const last = isPathIndex(keys[keys.length - 1]) ? toInt(keys[keys.length - 1]) : keys[keys.length - 1];
    if (cur && (isObj(cur) || isArr(cur)) && last in cur) {
      delete cur[last];
      return true;
    }
    return false;
  };
  const escapeRegex = (s) => String(s).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const toBufferUtf8 = (s) => {
    if (typeof Buffer !== "undefined") return Buffer.from(String(s), "utf8");
    throw new Error("base64 helpers require Buffer in this runtime");
  };
  const fromCodePoints = (...xs) => String.fromCodePoint(...xs.map((x) => Math.max(0, toInt(x))));
  const deepMergeValues = (a, b) => {
    if (isArr(a) && isArr(b)) {
      const out = a.slice();
      for (let i = 0; i < b.length; i++) out[i] = i < out.length ? deepMergeValues(out[i], b[i]) : deepClone(b[i]);
      return out;
    }
    if (isObj(a) && isObj(b) && !isFn(a) && !isFn(b)) {
      const out = Object.create(null);
      for (const k of Object.keys(a)) out[k] = deepClone(a[k]);
      for (const k of Object.keys(b)) {
        if (hasOwn.call(out, k)) out[k] = deepMergeValues(out[k], b[k]);
        else out[k] = deepClone(b[k]);
      }
      return out;
    }
    return deepClone(b);
  };
  const stableSerialize = (v, seen = new WeakSet()) => {
    if (v === null) return "null";
    const t = typeof v;
    if (t === "string") return JSON.stringify(v);
    if (t === "number") {
      if (Number.isNaN(v)) return "NaN";
      if (v === Infinity) return "Infinity";
      if (v === -Infinity) return "-Infinity";
      if (Object.is(v, -0)) return "-0";
      return String(v);
    }
    if (t === "boolean") return v ? "true" : "false";
    if (t === "undefined") return "undefined";
    if (t === "function") return `[Function:${v.name || "anonymous"}]`;
    if (t === "symbol") return `[Symbol:${String(v.description ?? "")}]`;
    if (!isObj(v)) return String(v);

    if (seen.has(v)) return "[Circular]";
    seen.add(v);
    try {
      if (isArr(v)) {
        const items = new Array(v.length);
        for (let i = 0; i < v.length; i++) items[i] = stableSerialize(v[i], seen);
        return `[${items.join(",")}]`;
      }
      const keys = Object.keys(v).sort();
      const items = [];
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        items.push(`${JSON.stringify(k)}:${stableSerialize(v[k], seen)}`);
      }
      return `{${items.join(",")}}`;
    } finally {
      seen.delete(v);
    }
  };
  const valueKey = (v) => stableSerialize(v);
  const varsSignature = (vars) => {
    const keys = Object.keys(vars).sort();
    const parts = new Array(keys.length);
    for (let i = 0; i < keys.length; i++) {
      const k = keys[i];
      parts[i] = `${JSON.stringify(k)}:${valueKey(vars[k])}`;
    }
    return parts.join("|");
  };

  const normalizeRule = (rule, index) => {
    if (isArr(rule)) {
      if (rule.length < 2) throw new Error(`logicInferer: rule[${index}] needs at least 2 items`);
      const premisesRaw = rule[0];
      const premises = isArr(premisesRaw) ? premisesRaw : [premisesRaw];
      return { premises, conclusion: rule[1] };
    }
    if (isObj(rule)) {
      const hasIf = hasOwn.call(rule, "if") || hasOwn.call(rule, "when") || hasOwn.call(rule, "premises");
      const hasThen = hasOwn.call(rule, "then") || hasOwn.call(rule, "conclusion");
      if (!hasIf || !hasThen) throw new Error(`logicInferer: rule[${index}] object must have if/then`);
      const premisesRaw = hasOwn.call(rule, "if") ? rule.if : hasOwn.call(rule, "when") ? rule.when : rule.premises;
      const premises = isArr(premisesRaw) ? premisesRaw : [premisesRaw];
      const conclusion = hasOwn.call(rule, "then") ? rule.then : rule.conclusion;
      return { premises, conclusion };
    }
    throw new Error(`logicInferer: unsupported rule format at index ${index}`);
  };
  const runInference = (ctx, factsRaw, rulesRaw, maxStepsRaw) => {
    const baseFacts = asArray(factsRaw, "logicInferer(facts)");
    const rules = asArray(rulesRaw, "logicInferer(rules)");
    const maxSteps = maxStepsRaw === undefined ? maxInferenceStepLimit : Math.max(0, toInt(maxStepsRaw));
    const known = new Set();
    const facts = [];
    const pushFact = (fact) => {
      const key = valueKey(fact);
      if (known.has(key)) return false;
      known.add(key);
      facts.push(fact);
      return true;
    };
    for (let i = 0; i < baseFacts.length; i++) pushFact(baseFacts[i]);
    if (maxSteps <= 0 || rules.length === 0) return facts;

    let saturated = false;
    for (let step = 0; step < maxSteps; step++) {
      chargeOps(ctx, "logicInferer:step", 1);
      let changed = false;
      for (let i = 0; i < rules.length; i++) {
        chargeOps(ctx, "logicInferer:rule", 1);
        const r = normalizeRule(rules[i], i);
        let enabled = true;
        for (let p = 0; p < r.premises.length; p++) {
          if (!known.has(valueKey(r.premises[p]))) {
            enabled = false;
            break;
          }
        }
        if (enabled && pushFact(r.conclusion)) changed = true;
      }
      if (!changed) {
        saturated = true;
        break;
      }
    }
    if (!saturated) {
      throw new Error(`logicInferer: inference step limit exceeded: ${maxSteps}`);
    }
    return facts;
  };

  const isLogicVarTerm = (v) => {
    if (typeof v === "string") return v.length >= 2 && v[0] === "?";
    if (!isObj(v) || isArr(v)) return false;
    if (hasOwn.call(v, "var")) return typeof v.var === "string" || typeof v.var === "number";
    if (hasOwn.call(v, "Var")) return typeof v.Var === "string" || typeof v.Var === "number";
    return false;
  };
  const logicVarName = (v) => {
    if (typeof v === "string") return v;
    const raw = hasOwn.call(v, "var") ? v.var : v.Var;
    const s = String(raw);
    return s.startsWith("?") ? s : `?${s}`;
  };
  const normalizeLogicAtom = (raw) => {
    if (isLogicVarTerm(raw)) return logicVarName(raw);
    if (isObj(raw) && !isArr(raw)) {
      if (hasOwn.call(raw, "atom")) return normalizeLogicAtom(raw.atom);
      if (hasOwn.call(raw, "goal")) return normalizeLogicAtom(raw.goal);
      if (hasOwn.call(raw, "pred") || hasOwn.call(raw, "name")) {
        const pred = hasOwn.call(raw, "pred") ? raw.pred : raw.name;
        let args = hasOwn.call(raw, "args") ? raw.args : [];
        if (!isArr(args)) args = [args];
        const out = new Array(args.length + 1);
        out[0] = pred;
        for (let i = 0; i < args.length; i++) out[i + 1] = args[i];
        return out;
      }
    }
    return raw;
  };
  const normalizeLogicLiteral = (raw) => {
    if (isObj(raw) && !isArr(raw)) {
      if (hasOwn.call(raw, "not")) return { negated: true, atom: normalizeLogicAtom(raw.not) };
      if (hasOwn.call(raw, "neg")) return { negated: true, atom: normalizeLogicAtom(raw.neg) };
      if (hasOwn.call(raw, "atom")) return { negated: false, atom: normalizeLogicAtom(raw.atom) };
      if (hasOwn.call(raw, "goal")) return { negated: false, atom: normalizeLogicAtom(raw.goal) };
    }
    if (isArr(raw) && raw.length === 2 && raw[0] === "not") {
      return { negated: true, atom: normalizeLogicAtom(raw[1]) };
    }
    return { negated: false, atom: normalizeLogicAtom(raw) };
  };
  const normalizeLogicBody = (raw, forceList = false) => {
    if (raw === undefined || raw === null) return [];
    if (isArr(raw) && !(raw.length === 2 && raw[0] === "not")) {
      if (forceList) return raw.map((lit) => normalizeLogicLiteral(lit));
      if (raw.length === 0) return [];
      if (isArr(raw[0]) || (isObj(raw[0]) && !isLogicVarTerm(raw[0]))) {
        return raw.map((lit) => normalizeLogicLiteral(lit));
      }
    }
    return [normalizeLogicLiteral(raw)];
  };
  const normalizeLogicRule = (rule, index) => {
    if (isArr(rule)) {
      if (rule.length === 0) throw new Error(`logicInferer: rule[${index}] must not be empty`);
      if (rule.length === 1) return { head: normalizeLogicAtom(rule[0]), body: [] };
      return {
        head: normalizeLogicAtom(rule[1]),
        body: normalizeLogicBody(rule[0], true),
      };
    }
    if (isObj(rule)) {
      const hasHead = hasOwn.call(rule, "head");
      const hasBody = hasOwn.call(rule, "body");
      const hasIf = hasOwn.call(rule, "if") || hasOwn.call(rule, "when") || hasOwn.call(rule, "premises");
      const hasThen = hasOwn.call(rule, "then") || hasOwn.call(rule, "conclusion");

      if (hasHead) {
        const bodyRaw = hasBody ? rule.body : [];
        return {
          head: normalizeLogicAtom(rule.head),
          body: normalizeLogicBody(bodyRaw, false),
        };
      }

      if (!hasIf || !hasThen) throw new Error(`logicInferer: rule[${index}] object must have head/body or if/then`);
      const ifRaw = hasOwn.call(rule, "if") ? rule.if : hasOwn.call(rule, "when") ? rule.when : rule.premises;
      const thenRaw = hasOwn.call(rule, "then") ? rule.then : rule.conclusion;
      return {
        head: normalizeLogicAtom(thenRaw),
        body: normalizeLogicBody(ifRaw, true),
      };
    }
    throw new Error(`logicInferer: unsupported rule format at index ${index}`);
  };
  const normalizeLogicQuery = (raw) => {
    if (isObj(raw) && !isArr(raw)) {
      if (hasOwn.call(raw, "all")) return normalizeLogicBody(raw.all, true);
      if (hasOwn.call(raw, "and")) return normalizeLogicBody(raw.and, true);
      if (hasOwn.call(raw, "goal")) return [normalizeLogicLiteral(raw.goal)];
      if (hasOwn.call(raw, "atom")) return [normalizeLogicLiteral(raw.atom)];
    }
    if (isArr(raw) && raw.length > 0 && (isArr(raw[0]) || (isObj(raw[0]) && !isLogicVarTerm(raw[0])))) {
      return normalizeLogicBody(raw, true);
    }
    return [normalizeLogicLiteral(raw)];
  };
  const cloneSubst = (subst) => {
    const out = Object.create(null);
    const keys = Object.keys(subst);
    for (let i = 0; i < keys.length; i++) out[keys[i]] = subst[keys[i]];
    return out;
  };
  const derefLogic = (term, subst) => {
    let cur = term;
    while (isLogicVarTerm(cur)) {
      const k = logicVarName(cur);
      if (!hasOwn.call(subst, k)) break;
      const next = subst[k];
      if (Object.is(next, cur)) break;
      cur = next;
    }
    return cur;
  };
  const occursLogic = (varName, term, subst, seen = new WeakSet()) => {
    const t = derefLogic(term, subst);
    if (isLogicVarTerm(t)) return logicVarName(t) === varName;
    if (isArr(t)) {
      for (let i = 0; i < t.length; i++) if (occursLogic(varName, t[i], subst, seen)) return true;
      return false;
    }
    if (isObj(t)) {
      if (seen.has(t)) return false;
      seen.add(t);
      const keys = Object.keys(t);
      for (let i = 0; i < keys.length; i++) if (occursLogic(varName, t[keys[i]], subst, seen)) return true;
    }
    return false;
  };
  const bindLogicVar = (v, x, subst) => {
    const name = logicVarName(v);
    const val = derefLogic(x, subst);
    if (isLogicVarTerm(val) && logicVarName(val) === name) return subst;
    if (occursLogic(name, val, subst)) return null;
    const next = cloneSubst(subst);
    next[name] = val;
    return next;
  };
  const unifyLogic = (a0, b0, subst0) => {
    let subst = subst0;
    const stack = [[a0, b0]];
    while (stack.length > 0) {
      const [aRaw, bRaw] = stack.pop();
      const a = derefLogic(aRaw, subst);
      const b = derefLogic(bRaw, subst);
      if (isLogicVarTerm(a)) {
        subst = bindLogicVar(a, b, subst);
        if (!subst) return null;
        continue;
      }
      if (isLogicVarTerm(b)) {
        subst = bindLogicVar(b, a, subst);
        if (!subst) return null;
        continue;
      }
      if (isArr(a) && isArr(b)) {
        if (a.length !== b.length) return null;
        for (let i = a.length - 1; i >= 0; i--) stack.push([a[i], b[i]]);
        continue;
      }
      if (isObj(a) && isObj(b) && !isArr(a) && !isArr(b)) {
        const ak = Object.keys(a).sort();
        const bk = Object.keys(b).sort();
        if (ak.length !== bk.length) return null;
        for (let i = 0; i < ak.length; i++) if (ak[i] !== bk[i]) return null;
        for (let i = ak.length - 1; i >= 0; i--) stack.push([a[ak[i]], b[bk[i]]]);
        continue;
      }
      if (!Object.is(a, b)) return null;
    }
    return subst;
  };
  const substituteLogic = (term, subst, seen = new WeakSet()) => {
    const t = derefLogic(term, subst);
    if (isLogicVarTerm(t)) return logicVarName(t);
    if (isArr(t)) {
      const out = new Array(t.length);
      for (let i = 0; i < t.length; i++) out[i] = substituteLogic(t[i], subst, seen);
      return out;
    }
    if (isObj(t)) {
      if (seen.has(t)) return "[Circular]";
      seen.add(t);
      const out = Object.create(null);
      const keys = Object.keys(t);
      for (let i = 0; i < keys.length; i++) out[keys[i]] = substituteLogic(t[keys[i]], subst, seen);
      return out;
    }
    return t;
  };
  const hasLogicVars = (term, subst = Object.create(null), seen = new WeakSet()) => {
    const t = derefLogic(term, subst);
    if (isLogicVarTerm(t)) return true;
    if (isArr(t)) {
      for (let i = 0; i < t.length; i++) if (hasLogicVars(t[i], subst, seen)) return true;
      return false;
    }
    if (isObj(t)) {
      if (seen.has(t)) return false;
      seen.add(t);
      const keys = Object.keys(t);
      for (let i = 0; i < keys.length; i++) if (hasLogicVars(t[keys[i]], subst, seen)) return true;
    }
    return false;
  };
  const collectLogicVars = (term, out, seen = new WeakSet()) => {
    if (isLogicVarTerm(term)) {
      out.add(logicVarName(term));
      return;
    }
    if (isArr(term)) {
      for (let i = 0; i < term.length; i++) collectLogicVars(term[i], out, seen);
      return;
    }
    if (isObj(term)) {
      if (seen.has(term)) return;
      seen.add(term);
      const keys = Object.keys(term);
      for (let i = 0; i < keys.length; i++) collectLogicVars(term[keys[i]], out, seen);
    }
  };
  const freshenLogicTerm = (term, suffix, table) => {
    if (isLogicVarTerm(term)) {
      const k = logicVarName(term);
      if (!hasOwn.call(table, k)) table[k] = `${k}#${suffix}`;
      return table[k];
    }
    if (isArr(term)) return term.map((x) => freshenLogicTerm(x, suffix, table));
    if (isObj(term)) {
      const out = Object.create(null);
      const keys = Object.keys(term);
      for (let i = 0; i < keys.length; i++) out[keys[i]] = freshenLogicTerm(term[keys[i]], suffix, table);
      return out;
    }
    return term;
  };
  const freshenLogicClause = (clause, suffix) => {
    const table = Object.create(null);
    return {
      head: freshenLogicTerm(clause.head, suffix, table),
      body: clause.body.map((lit) => ({
        negated: lit.negated,
        atom: freshenLogicTerm(lit.atom, suffix, table),
      })),
    };
  };
  const parseLogicOptions = (raw, queryHasVars) => {
    let maxSteps = maxInferenceStepLimit;
    let maxSolutions = queryHasVars ? 64 : 1;
    if (raw === undefined) return { maxSteps, maxSolutions };
    if (typeof raw === "number" && Number.isFinite(raw)) {
      if (queryHasVars) maxSolutions = Math.max(1, toInt(raw));
      else maxSteps = Math.max(1, toInt(raw));
      return { maxSteps, maxSolutions };
    }
    if (!isObj(raw) || isArr(raw)) throw new Error("logicInferer: options must be number or object");
    if (hasOwn.call(raw, "maxSteps")) {
      maxSteps = raw.maxSteps === Infinity ? Infinity : Math.max(1, toInt(raw.maxSteps));
    } else if (hasOwn.call(raw, "maxDepth")) {
      maxSteps = raw.maxDepth === Infinity ? Infinity : Math.max(1, toInt(raw.maxDepth));
    }
    if (hasOwn.call(raw, "maxSolutions")) {
      maxSolutions = Math.max(1, toInt(raw.maxSolutions));
    } else if (hasOwn.call(raw, "limit")) {
      maxSolutions = Math.max(1, toInt(raw.limit));
    }
    return { maxSteps, maxSolutions };
  };
  const evalLogicBuiltin = (atom, subst) => {
    if (!isArr(atom) || atom.length === 0) return null;
    const pred = derefLogic(atom[0], subst);
    if (typeof pred !== "string") return null;
    if (pred === "=" || pred === "equal" || pred === "eq") {
      if (atom.length !== 3) throw new Error("logicInferer: '=' expects 2 arguments");
      const unified = unifyLogic(atom[1], atom[2], subst);
      return unified ? [unified] : [];
    }
    if (pred === "!=" || pred === "notEqual") {
      if (atom.length !== 3) throw new Error("logicInferer: '!=' expects 2 arguments");
      const unified = unifyLogic(atom[1], atom[2], subst);
      return unified ? [] : [subst];
    }
    if (pred === "true") return [subst];
    if (pred === "false" || pred === "fail") return [];
    return null;
  };
  const runLogicInference = (ctx, factsRaw, rulesRaw, queryRaw, optionsRaw) => {
    const factsArr = asArray(factsRaw, "logicInferer(facts)");
    const rulesArr = isArr(rulesRaw) ? rulesRaw : [rulesRaw];
    const queryLits = normalizeLogicQuery(queryRaw);
    const queryVarSet = new Set();
    for (let i = 0; i < queryLits.length; i++) collectLogicVars(queryLits[i].atom, queryVarSet);
    const queryVars = Array.from(queryVarSet).sort();
    const opts = parseLogicOptions(optionsRaw, queryVars.length > 0);
    const maxSteps = opts.maxSteps;
    const maxSolutions = opts.maxSolutions;

    const clauses = [];
    for (let i = 0; i < factsArr.length; i++) {
      clauses.push({
        head: normalizeLogicAtom(factsArr[i]),
        body: [],
      });
    }
    for (let i = 0; i < rulesArr.length; i++) {
      clauses.push(normalizeLogicRule(rulesArr[i], i));
    }

    let stepCount = 0;
    let freshSeq = 0;
    const step = (phase) => {
      chargeOps(ctx, phase, 1);
      stepCount += 1;
      if (maxSteps !== Infinity && stepCount > maxSteps) {
        throw new Error(`logicInferer: inference step limit exceeded: ${maxSteps}`);
      }
    };

    const solutions = [];
    const seen = new Set();
    const project = (subst) => {
      if (queryVars.length === 0) return true;
      const out = Object.create(null);
      for (let i = 0; i < queryVars.length; i++) {
        const k = queryVars[i];
        const v = substituteLogic(k, subst);
        if (isLogicVarTerm(v) && logicVarName(v) === k) continue;
        out[k] = v;
      }
      return out;
    };

    const search = (goals, subst, stopOnFirst) => {
      step("logicInferer:search");
      if (goals.length === 0) {
        if (stopOnFirst) return true;
        const projected = project(subst);
        const key = valueKey(projected);
        if (!seen.has(key)) {
          seen.add(key);
          solutions.push(projected);
        }
        return solutions.length >= maxSolutions;
      }

      const goal = goals[0];
      const rest = goals.slice(1);

      if (goal.negated) {
        const grounded = substituteLogic(goal.atom, subst);
        if (hasLogicVars(grounded)) {
          throw new Error("logicInferer: negated literal must be ground");
        }
        const hasProof = search([{ negated: false, atom: grounded }], subst, true);
        if (!hasProof) return search(rest, subst, stopOnFirst);
        return false;
      }

      const builtinBranches = evalLogicBuiltin(goal.atom, subst);
      if (builtinBranches) {
        for (let i = 0; i < builtinBranches.length; i++) {
          if (search(rest, builtinBranches[i], stopOnFirst)) return true;
        }
        return false;
      }

      for (let i = 0; i < clauses.length; i++) {
        step("logicInferer:clause");
        freshSeq += 1;
        const clause = freshenLogicClause(clauses[i], freshSeq);
        const unified = unifyLogic(goal.atom, clause.head, subst);
        if (!unified) continue;
        if (search(clause.body.concat(rest), unified, stopOnFirst)) return true;
      }
      return false;
    };

    const shouldStopOnFirst = queryVars.length === 0;
    const found = search(queryLits, Object.create(null), shouldStopOnFirst);

    if (queryVars.length === 0) return !!found;
    return solutions;
  };

  const normalizeConstraintVars = (varsRaw, label) => {
    const vars = isArr(varsRaw) ? varsRaw : [varsRaw];
    if (vars.length === 0) throw new Error(`${label}: vars must not be empty`);
    return vars.map((v) => String(v));
  };
  const normalizeConstraint = (constraint, index) => {
    if (isFn(constraint)) return { kind: "fn", vars: null, fn: constraint };
    if (isArr(constraint)) {
      if (constraint.length < 2) throw new Error(`constraintSolver: constraint[${index}] needs at least 2 items`);
      const kind = String(constraint[0]);
      const vars = normalizeConstraintVars(constraint[1], `constraint[${index}]`);
      const args = constraint.slice(2);
      return { kind, vars, args };
    }
    if (isObj(constraint)) {
      const fn = isFn(constraint.fn) ? constraint.fn : isFn(constraint.predicate) ? constraint.predicate : null;
      if (fn) {
        const vars = constraint.vars === undefined ? null : normalizeConstraintVars(constraint.vars, `constraint[${index}]`);
        return { kind: "fn", vars, fn };
      }
      const kindRaw = hasOwn.call(constraint, "kind")
        ? constraint.kind
        : hasOwn.call(constraint, "type")
          ? constraint.type
          : undefined;
      if (kindRaw === undefined) throw new Error(`constraintSolver: constraint[${index}] requires kind/type`);
      const vars = normalizeConstraintVars(constraint.vars, `constraint[${index}]`);
      let args = [];
      if (hasOwn.call(constraint, "args")) args = asArray(constraint.args, `constraint[${index}].args`).slice();
      else if (hasOwn.call(constraint, "values")) args = asArray(constraint.values, `constraint[${index}].values`).slice();
      else if (hasOwn.call(constraint, "value")) args = [constraint.value];
      return { kind: String(kindRaw), vars, args };
    }
    throw new Error(`constraintSolver: unsupported constraint format at index ${index}`);
  };
  const domainMinMax = (domain, label) => {
    let min = Infinity;
    let max = -Infinity;
    for (let i = 0; i < domain.length; i++) {
      const n = toFiniteNumber(domain[i], label);
      if (n < min) min = n;
      if (n > max) max = n;
    }
    return { min, max };
  };
  const evaluateConstraint = (ctx, constraint, assignment, domainTable, domainVars, assignedCount) => {
    if (constraint.kind === "fn") {
      const ready =
        constraint.vars === null
          ? assignedCount === domainVars.length
          : constraint.vars.every((name) => hasOwn.call(assignment, name));
      if (!ready) return true;
      chargeOps(ctx, "constraintSolver:predicate", 1);
      return !!constraint.fn(assignment);
    }

    const kind = constraint.kind;
    const vars = constraint.vars;
    const args = constraint.args || [];
    const assignedVars = [];
    const assignedValues = [];
    for (let i = 0; i < vars.length; i++) {
      const name = vars[i];
      if (hasOwn.call(assignment, name)) {
        assignedVars.push(name);
        assignedValues.push(assignment[name]);
      }
    }
    const allAssigned = assignedVars.length === vars.length;

    if (kind === "allDifferent") {
      const seen = new Set();
      for (let i = 0; i < assignedValues.length; i++) {
        const k = valueKey(assignedValues[i]);
        if (seen.has(k)) return false;
        seen.add(k);
      }
      return true;
    }

    if (kind === "equal") {
      if (assignedValues.length < 2) return true;
      const first = assignedValues[0];
      for (let i = 1; i < assignedValues.length; i++) if (!Object.is(first, assignedValues[i])) return false;
      return true;
    }

    if (kind === "notEqual") {
      if (assignedValues.length < 2) return true;
      const seen = new Set();
      for (let i = 0; i < assignedValues.length; i++) {
        const k = valueKey(assignedValues[i]);
        if (seen.has(k)) return false;
        seen.add(k);
      }
      return true;
    }

    if (kind === "less" || kind === "lessEqual" || kind === "greater" || kind === "greaterEqual") {
      if (!allAssigned) return true;
      if (assignedValues.length < 2) return true;
      for (let i = 0; i < assignedValues.length - 1; i++) {
        const a = toFiniteNumber(assignedValues[i], `constraintSolver:${kind}`);
        const b = toFiniteNumber(assignedValues[i + 1], `constraintSolver:${kind}`);
        if (kind === "less" && !(a < b)) return false;
        if (kind === "lessEqual" && !(a <= b)) return false;
        if (kind === "greater" && !(a > b)) return false;
        if (kind === "greaterEqual" && !(a >= b)) return false;
      }
      return true;
    }

    if (kind === "inDomain") {
      if (vars.length !== 1) throw new Error("constraintSolver: inDomain expects exactly 1 var");
      if (!hasOwn.call(assignment, vars[0])) return true;
      const allowed = args.length === 1 && isArr(args[0]) ? args[0] : args;
      const seen = new Set(allowed.map((v) => valueKey(v)));
      return seen.has(valueKey(assignment[vars[0]]));
    }

    if (kind === "sumEqual" || kind === "sumLessEqual" || kind === "sumGreaterEqual") {
      if (args.length < 1) throw new Error(`constraintSolver: ${kind} requires target value`);
      const target = toFiniteNumber(args[0], `constraintSolver:${kind}`);
      let sumAssigned = 0;
      for (let i = 0; i < assignedValues.length; i++) {
        sumAssigned += toFiniteNumber(assignedValues[i], `constraintSolver:${kind}`);
      }
      if (allAssigned) {
        if (kind === "sumEqual") return Object.is(sumAssigned, target);
        if (kind === "sumLessEqual") return sumAssigned <= target;
        return sumAssigned >= target;
      }

      let minRest = 0;
      let maxRest = 0;
      for (let i = 0; i < vars.length; i++) {
        const name = vars[i];
        if (hasOwn.call(assignment, name)) continue;
        const mm = domainMinMax(domainTable[name], `constraintSolver:${kind}:${name}`);
        minRest += mm.min;
        maxRest += mm.max;
      }
      if (kind === "sumEqual") return sumAssigned + minRest <= target && sumAssigned + maxRest >= target;
      if (kind === "sumLessEqual") return sumAssigned + minRest <= target;
      return sumAssigned + maxRest >= target;
    }

    throw new Error(`constraintSolver: unknown constraint kind '${kind}'`);
  };
  const solveConstraints = (ctx, domainsRaw, constraintsRaw, maxSolutionsRaw, maxNodesRaw) => {
    if (!isObj(domainsRaw) || isArr(domainsRaw)) {
      throw new Error("constraintSolver: first arg must be object { varName: domainTuple }");
    }
    const domainVars = Object.keys(domainsRaw);
    if (domainVars.length === 0) return maxSolutionsRaw && toInt(maxSolutionsRaw) > 1 ? [] : null;

    const domainTable = Object.create(null);
    for (let i = 0; i < domainVars.length; i++) {
      const name = domainVars[i];
      const dom = asArray(domainsRaw[name], `constraintSolver: domain '${name}'`).slice();
      if (dom.length === 0) throw new Error(`constraintSolver: domain '${name}' must not be empty`);
      domainTable[name] = dom;
    }

    const constraints = asArray(constraintsRaw, "constraintSolver(constraints)").map((cst, i) => normalizeConstraint(cst, i));
    for (let i = 0; i < constraints.length; i++) {
      const cst = constraints[i];
      if (cst.kind === "fn" && cst.vars === null) continue;
      const vars = cst.vars || [];
      for (let j = 0; j < vars.length; j++) {
        if (!hasOwn.call(domainTable, vars[j])) {
          throw new Error(`constraintSolver: unknown variable '${vars[j]}' in constraint[${i}]`);
        }
      }
    }

    const maxSolutions = Math.max(1, toInt(maxSolutionsRaw === undefined ? 1 : maxSolutionsRaw));
    const maxNodes = maxNodesRaw === undefined
      ? maxConstraintNodeLimit
      : Math.max(1, toInt(maxNodesRaw));

    const assignment = Object.create(null);
    const solutions = [];
    let assignedCount = 0;
    let nodeCount = 0;

    const constraintsPass = () => {
      for (let i = 0; i < constraints.length; i++) {
        chargeOps(ctx, "constraintSolver:check", 1);
        if (!evaluateConstraint(ctx, constraints[i], assignment, domainTable, domainVars, assignedCount)) return false;
      }
      return true;
    };

    const chooseNextVar = () => {
      let best = null;
      let bestSize = Infinity;
      for (let i = 0; i < domainVars.length; i++) {
        const name = domainVars[i];
        if (hasOwn.call(assignment, name)) continue;
        const size = domainTable[name].length;
        if (size < bestSize) {
          bestSize = size;
          best = name;
        }
      }
      return best;
    };

    const search = () => {
      chargeOps(ctx, "constraintSolver:node", 1);
      nodeCount += 1;
      if (nodeCount > maxNodes) {
        throw new Error(`constraintSolver: node budget exceeded: ${nodeCount} > ${maxNodes}`);
      }
      if (!constraintsPass()) return false;

      if (assignedCount === domainVars.length) {
        const out = Object.create(null);
        for (let i = 0; i < domainVars.length; i++) out[domainVars[i]] = assignment[domainVars[i]];
        solutions.push(out);
        return solutions.length >= maxSolutions;
      }

      const varName = chooseNextVar();
      const domain = domainTable[varName];
      for (let i = 0; i < domain.length; i++) {
        chargeOps(ctx, "constraintSolver:branch", 1);
        assignment[varName] = domain[i];
        assignedCount += 1;
        const done = search();
        assignedCount -= 1;
        delete assignment[varName];
        if (done) return true;
      }
      return false;
    };

    search();
    if (maxSolutions === 1) return solutions.length ? solutions[0] : null;
    return solutions;
  };
  const resolveExternalFn = (name) => {
    if (!hasOwn.call(externalFns, name)) return null;
    const fn = externalFns[name];
    if (!isFn(fn)) throw new Error(`External '${name}' is not callable`);
    return fn;
  };
  const callExternal = (ctx, name, args) => {
    const fn = resolveExternalFn(name);
    if (!fn) throw new Error(`Unknown function '${name}'`);
    chargeOps(ctx, `external:${name}`, 1);
    return invokeGuarded(ctx, fn, args, `external:${name}`);
  };

  function c(node) {
    switch (node.type) {
      case "Number":
      case "String":
      case "Boolean":
      case "Null":
      case "Undefined":
        return () => node.value;

      case "Var":
        return (ctx) => safeGetVar(ctx, node.name, node.pos);

      case "Tuple": {
        const parts = node.items.map(c);
        return (ctx) => parts.map((fn) => fn(ctx));
      }

      case "Call": {
        const name = node.name;
        const argFns = node.args.map(c);
        const evalArgs = (ctx) => {
          chargeOps(ctx, `call:${name}`, 1);
          return argFns.map((fn) => fn(ctx));
        };

        /* ---------- core builtins ---------- */

        if (name === "mainner") {
          return (ctx) => {
            chargeOps(ctx, "mainner", 1);
            let v = undefined;
            for (const fn of argFns) {
              chargeOps(ctx, "mainner:step", 1);
              v = fn(ctx);
            }
            return v;
          };
        }

        if (name === "returner") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => argFns[0](ctx);
        }

        if (name === "assigner") {
          arity(name, node.args.length, 2, 2);
          const raw = node.args[0];
          if (!raw || raw.type !== "Var") throw new Error("assigner(first arg) must be a variable name");
          const varName = raw.name;
          const valueFn = argFns[1];
          return (ctx) => {
            const v = valueFn(ctx);
            ctx.vars[varName] = v;
            return v;
          };
        }

        if (name === "deleter") {
          arity(name, node.args.length, 1, 1);
          const raw = node.args[0];
          if (!raw || raw.type !== "Var") throw new Error("deleter(arg) must be a variable name");
          const varName = raw.name;
          return (ctx) => {
            if (hasOwn.call(ctx.vars, varName)) {
              delete ctx.vars[varName];
              return true;
            }
            return false;
          };
        }

        if (name === "exister") {
          arity(name, node.args.length, 1, 1);
          const raw = node.args[0];
          if (!raw || raw.type !== "Var") throw new Error("exister(arg) must be a variable name");
          const varName = raw.name;
          return (ctx) => hasOwn.call(ctx.vars, varName);
        }

        /* ---------- numeric / logic ---------- */

        if (name === "adder") return (ctx) => evalArgs(ctx).reduce((a, b) => toNumber(a) + toNumber(b), 0);

        if (name === "subtractor") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) - toNumber(b);
          };
        }

        if (name === "multiplier") return (ctx) => evalArgs(ctx).reduce((a, b) => toNumber(a) * toNumber(b), 1);

        if (name === "divider") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) / toNumber(b);
          };
        }

        if (name === "modder") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) % toNumber(b);
          };
        }

        if (name === "powerer") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) ** toNumber(b);
          };
        }

        if (name === "inverter") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => -toNumber(argFns[0](ctx));
        }

        if (name === "lesser") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) < toNumber(b);
          };
        }

        if (name === "lessEqualer") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) <= toNumber(b);
          };
        }

        if (name === "greater") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) > toNumber(b);
          };
        }

        if (name === "greaterEqualer") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return toNumber(a) >= toNumber(b);
          };
        }

        if (name === "equaler") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return Object.is(a, b);
          };
        }

        if (name === "notequaler") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx);
            return !Object.is(a, b);
          };
        }

        if (name === "noter") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => !argFns[0](ctx);
        }

        if (name === "ander") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const bFn = argFns[1];
          return (ctx) => (aFn(ctx) ? !!bFn(ctx) : false);
        }

        if (name === "orer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const bFn = argFns[1];
          return (ctx) => (aFn(ctx) ? true : !!bFn(ctx));
        }

        if (name === "nander") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const bFn = argFns[1];
          return (ctx) => !(aFn(ctx) && bFn(ctx));
        }

        if (name === "xorer") {
          arity(name, node.args.length, 2, 2);
          return (ctx) => {
            const [a, b] = evalArgs(ctx).map((x) => !!x);
            return a !== b;
          };
        }

        if (name === "miner") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            const xs = evalArgs(ctx);
            let v = toFiniteNumber(xs[0], "miner");
            for (let i = 1; i < xs.length; i++) {
              const n = toFiniteNumber(xs[i], "miner");
              if (n < v) v = n;
            }
            return v;
          };
        }

        if (name === "maxer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            const xs = evalArgs(ctx);
            let v = toFiniteNumber(xs[0], "maxer");
            for (let i = 1; i < xs.length; i++) {
              const n = toFiniteNumber(xs[i], "maxer");
              if (n > v) v = n;
            }
            return v;
          };
        }

        if (name === "abser") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.abs(toNumber(argFns[0](ctx)));
        }

        if (name === "floorrer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.floor(toNumber(argFns[0](ctx)));
        }

        if (name === "ceiler") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.ceil(toNumber(argFns[0](ctx)));
        }

        if (name === "rounder") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.round(toNumber(argFns[0](ctx)));
        }

        if (name === "truncer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.trunc(toNumber(argFns[0](ctx)));
        }

        if (name === "sqrter") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.sqrt(toNumber(argFns[0](ctx)));
        }

        if (name === "signer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.sign(toNumber(argFns[0](ctx)));
        }

        if (name === "siner") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.sin(toNumber(argFns[0](ctx)));
        }

        if (name === "coser") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.cos(toNumber(argFns[0](ctx)));
        }

        if (name === "taner") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.tan(toNumber(argFns[0](ctx)));
        }

        if (name === "loger") {
          arity(name, node.args.length, 1, 2);
          const xFn = argFns[0];
          const baseFn = argFns[1] || null;
          return (ctx) => {
            const x = toNumber(xFn(ctx));
            if (!baseFn) return Math.log(x);
            const base = toNumber(baseFn(ctx));
            return Math.log(x) / Math.log(base);
          };
        }

        if (name === "expeer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.exp(toNumber(argFns[0](ctx)));
        }

        if (name === "clampper") {
          arity(name, node.args.length, 3, 3);
          const xFn = argFns[0], minFn = argFns[1], maxFn = argFns[2];
          return (ctx) => {
            const x = toNumber(xFn(ctx));
            let lo = toNumber(minFn(ctx));
            let hi = toNumber(maxFn(ctx));
            if (lo > hi) {
              const t = lo;
              lo = hi;
              hi = t;
            }
            return x < lo ? lo : x > hi ? hi : x;
          };
        }

        if (name === "randomer") {
          arity(name, node.args.length, 0, 2);
          return (ctx) => {
            if (node.args.length === 0) return Math.random();
            if (node.args.length === 1) return Math.random() * toNumber(argFns[0](ctx));
            let a = toNumber(argFns[0](ctx));
            let b = toNumber(argFns[1](ctx));
            if (a > b) {
              const t = a;
              a = b;
              b = t;
            }
            return a + Math.random() * (b - a);
          };
        }

        if (name === "randomInter") {
          // randomInter(max) or randomInter(min,max) ; upper bound exclusive
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0];
          const bFn = argFns[1] || null;
          return (ctx) => {
            if (!bFn) {
              const max = toInt(aFn(ctx));
              if (max <= 0) return 0;
              return Math.floor(Math.random() * max);
            }
            let min = toInt(aFn(ctx));
            let max = toInt(bFn(ctx));
            if (min > max) {
              const t = min;
              min = max;
              max = t;
            }
            if (min === max) return min;
            return min + Math.floor(Math.random() * (max - min));
          };
        }

        if (name === "hypotter") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => Math.hypot(...evalArgs(ctx).map((x) => toNumber(x)));
        }

        if (name === "deg2rader") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => (toNumber(argFns[0](ctx)) * Math.PI) / 180;
        }

        if (name === "rad2deger") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => (toNumber(argFns[0](ctx)) * 180) / Math.PI;
        }

        if (name === "lerper") {
          arity(name, node.args.length, 3, 3);
          const aFn = argFns[0], bFn = argFns[1], tFn = argFns[2];
          return (ctx) => {
            const a = toNumber(aFn(ctx));
            const b = toNumber(bFn(ctx));
            const t = toNumber(tFn(ctx));
            return a + (b - a) * t;
          };
        }

        if (name === "sigmoider") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => {
            const x = toNumber(argFns[0](ctx));
            return 1 / (1 + Math.exp(-x));
          };
        }

        if (name === "tanher") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Math.tanh(toNumber(argFns[0](ctx)));
        }

        if (name === "meaner") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => meanOf(asNumberArray(aFn(ctx), "meaner"));
        }

        if (name === "medianer") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => medianOf(asNumberArray(aFn(ctx), "medianer"));
        }

        if (name === "varianceer") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0];
          const sFn = argFns[1] || (() => false);
          return (ctx) => varianceOf(asNumberArray(aFn(ctx), "varianceer"), !!sFn(ctx));
        }

        if (name === "stddever") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0];
          const sFn = argFns[1] || (() => false);
          return (ctx) => Math.sqrt(varianceOf(asNumberArray(aFn(ctx), "stddever"), !!sFn(ctx)));
        }

        if (name === "sumer") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => asNumberArray(aFn(ctx), "sumer").reduce((a, b) => a + b, 0);
        }

        if (name === "proder") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => asNumberArray(aFn(ctx), "proder").reduce((a, b) => a * b, 1);
        }

        if (name === "nower") {
          arity(name, node.args.length, 0, 0);
          return () => Date.now();
        }

        if (name === "isoNower") {
          arity(name, node.args.length, 0, 0);
          return () => new Date().toISOString();
        }

        if (name === "dater") {
          // dater(ms?) -> ISO string
          arity(name, node.args.length, 0, 1);
          const tFn = argFns[0] || null;
          return (ctx) => {
            const ms = tFn ? toNumber(tFn(ctx)) : Date.now();
            return new Date(ms).toISOString();
          };
        }

        if (name === "dateParseer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => Date.parse(String(sFn(ctx)));
        }

        if (name === "dateAdder") {
          // dateAdder(msOrDateString, amount, unit) -> timestamp(ms)
          arity(name, node.args.length, 3, 3);
          const tFn = argFns[0], aFn = argFns[1], uFn = argFns[2];
          return (ctx) => {
            const baseRaw = tFn(ctx);
            const baseMs = typeof baseRaw === "number" ? baseRaw : Date.parse(String(baseRaw));
            const amount = toNumber(aFn(ctx));
            const unit = String(uFn(ctx)).toLowerCase();
            const d = new Date(baseMs);
            if (!Number.isFinite(d.getTime())) return NaN;
            if (unit === "ms" || unit === "millisecond" || unit === "milliseconds") d.setTime(d.getTime() + amount);
            else if (unit === "s" || unit === "sec" || unit === "second" || unit === "seconds") d.setTime(d.getTime() + amount * 1000);
            else if (unit === "m" || unit === "min" || unit === "minute" || unit === "minutes") d.setTime(d.getTime() + amount * 60000);
            else if (unit === "h" || unit === "hour" || unit === "hours") d.setTime(d.getTime() + amount * 3600000);
            else if (unit === "d" || unit === "day" || unit === "days") d.setTime(d.getTime() + amount * 86400000);
            else if (unit === "month" || unit === "months") d.setMonth(d.getMonth() + amount);
            else if (unit === "y" || unit === "year" || unit === "years") d.setFullYear(d.getFullYear() + amount);
            else throw new Error(`dateAdder: unknown unit '${unit}'`);
            return d.getTime();
          };
        }

        /* ---------- type / cast ---------- */

        if (name === "typeer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => {
            const v = argFns[0](ctx);
            if (v === null) return "null";
            if (Array.isArray(v)) return "array";
            return typeof v;
          };
        }

        if (name === "stringer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => String(argFns[0](ctx));
        }

        if (name === "numberer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => toNumber(argFns[0](ctx));
        }

        if (name === "inter") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => toInt(argFns[0](ctx));
        }

        if (name === "booler") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => !!argFns[0](ctx);
        }

        if (name === "isNaner") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Number.isNaN(toNumber(argFns[0](ctx)));
        }

        if (name === "isFiniteer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Number.isFinite(toNumber(argFns[0](ctx)));
        }

        if (name === "isIntegerer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => Number.isInteger(toNumber(argFns[0](ctx)));
        }

        if (name === "isArrayer") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => isArr(argFns[0](ctx));
        }

        if (name === "isObjecter") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => isObj(argFns[0](ctx));
        }

        if (name === "isFunctioner") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => isFn(argFns[0](ctx));
        }

        /* ---------- control flow ---------- */

        if (name === "ifer") {
          arity(name, node.args.length, 2, 3);
          const condFn = argFns[0];
          const thenFn = argFns[1];
          const elseFn = argFns[2] || (() => undefined);
          return (ctx) => (condFn(ctx) ? thenFn(ctx) : elseFn(ctx));
        }

        if (name === "whiler") {
          arity(name, node.args.length, 2, 2);
          const condFn = argFns[0];
          const bodyFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "whiler:init", 1);
            let stallCount = 0;
            let prevSig = varsSignature(ctx.vars);
            while (condFn(ctx)) {
              chargeLoop(ctx, "whiler");
              bodyFn(ctx);
              if (maxLoopStallLimit !== Infinity) {
                chargeOps(ctx, "whiler:stall-check", 1);
                const nextSig = varsSignature(ctx.vars);
                if (nextSig === prevSig) {
                  stallCount += 1;
                  if (stallCount > maxLoopStallLimit) {
                    throw new Error(
                      `Loop stall detected in whiler: ${stallCount} > ${maxLoopStallLimit} (vars unchanged)`
                    );
                  }
                } else {
                  stallCount = 0;
                }
                prevSig = nextSig;
              }
            }
            return undefined;
          };
        }

        if (name === "forrer") {
          const raw = node.args[0];
          if (!raw || raw.type !== "Var") throw new Error("forrer(first arg) must be a variable name");
          const varName = raw.name;

          const argc = node.args.length;
          if (!(argc === 4 || argc === 5)) {
            throw new Error("forrer expects 4 or 5 arguments: forrer(i,start,end,body) or forrer(i,start,end,step,body)");
          }

          const startFn = argFns[1];
          const endFn = argFns[2];
          const stepFn = argc === 5 ? argFns[3] : null;
          const bodyFn = argc === 5 ? argFns[4] : argFns[3];

          return (ctx) => {
            chargeOps(ctx, "forrer:init", 1);
            const prevHad = hasOwn.call(ctx.vars, varName);
            const prevVal = ctx.vars[varName];

            const start = toInt(startFn(ctx));
            const end = toInt(endFn(ctx));

            let step;
            if (stepFn) {
              step = toInt(stepFn(ctx));
              if (step === 0) throw new Error("forrer: step must not be 0");
            } else {
              step = start <= end ? 1 : -1;
            }

            const forward = step > 0;
            for (let k = start; forward ? k < end : k > end; k += step) {
              chargeLoop(ctx, "forrer");
              ctx.vars[varName] = k;
              bodyFn(ctx);
            }

            if (prevHad) ctx.vars[varName] = prevVal;
            else delete ctx.vars[varName];
            return undefined;
          };
        }

        if (name === "timeser") {
          // timeser(count, bodyExprOrFn)
          arity(name, node.args.length, 2, 2);
          const nFn = argFns[0];
          const bodyExprFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "timeser:init", 1);
            const n = Math.max(0, toInt(nFn(ctx)));
            let out = undefined;
            for (let i = 0; i < n; i++) {
              chargeLoop(ctx, "timeser");
              const bodyVal = bodyExprFn(ctx);
              if (isFn(bodyVal)) out = invokeGuarded(ctx, bodyVal, [i], "timeser:callback");
              else out = bodyVal;
            }
            return out;
          };
        }

        if (name === "switcher") {
          arity(name, node.args.length, 2, Infinity);
          const testFn = argFns[0];
          const rawCases = node.args.slice(1);

          const compiledCases = [];
          let defaultFn = null;

          for (let i = 0; i < rawCases.length; i++) {
            const rc = rawCases[i];
            if (rc.type === "Tuple" && rc.items.length === 2 && rc.items[0].type === "Tuple") {
              const values = rc.items[0].items.map((v) => {
                if (v.type === "Number") return v.value;
                if (v.type === "String") return v.value;
                if (v.type === "Boolean") return v.value;
                if (v.type === "Null") return null;
                if (v.type === "Undefined") return undefined;
                return { __expr: c(v) };
              });
              const bodyFn = c(rc.items[1]);
              compiledCases.push({ values, bodyFn });
            } else {
              defaultFn = c(rc);
            }
          }

          return (ctx) => {
            const testVal = testFn(ctx);
            for (const cs of compiledCases) {
              for (const vv of cs.values) {
                const v = vv && vv.__expr ? vv.__expr(ctx) : vv;
                if (Object.is(v, testVal)) return cs.bodyFn(ctx);
              }
            }
            return defaultFn ? defaultFn(ctx) : undefined;
          };
        }

        if (name === "logicInferer") {
          arity(name, node.args.length, 2, 4);
          const factsFn = argFns[0];
          const rulesFn = argFns[1];
          const a3Fn = argFns[2];
          const a4Fn = argFns[3];
          const argc = node.args.length;
          return (ctx) => {
            const factsRaw = factsFn(ctx);
            const rulesRaw = rulesFn(ctx);

            if (argc === 2) {
              return runInference(ctx, factsRaw, rulesRaw, undefined);
            }

            if (argc === 3) {
              const v3 = a3Fn(ctx);
              if (typeof v3 === "number" && Number.isFinite(v3)) {
                return runInference(ctx, factsRaw, rulesRaw, v3);
              }
              return runLogicInference(ctx, factsRaw, rulesRaw, v3, undefined);
            }

            const query = a3Fn(ctx);
            const options = a4Fn(ctx);
            return runLogicInference(ctx, factsRaw, rulesRaw, query, options);
          };
        }

        if (name === "constraintLogicer") {
          arity(name, node.args.length, 2, Infinity);
          const kindFn = argFns[0];
          const varsFn = argFns[1];
          const paramFns = argFns.slice(2);
          return (ctx) => {
            const kind = String(kindFn(ctx));
            const vars = normalizeConstraintVars(varsFn(ctx), "constraintLogicer");
            const out = Object.create(null);
            out.kind = kind;
            out.vars = vars;
            if (paramFns.length > 0) out.args = paramFns.map((fn) => fn(ctx));
            return out;
          };
        }

        if (name === "constraintSolver") {
          arity(name, node.args.length, 2, 4);
          const domainsFn = argFns[0];
          const constraintsFn = argFns[1];
          const maxSolutionsFn = argFns[2];
          const maxNodesFn = argFns[3];
          return (ctx) =>
            solveConstraints(
              ctx,
              domainsFn(ctx),
              constraintsFn(ctx),
              maxSolutionsFn ? maxSolutionsFn(ctx) : undefined,
              maxNodesFn ? maxNodesFn(ctx) : undefined
            );
        }

        /* ---------- variables / mutation ---------- */

        if (name === "incrementer") {
          arity(name, node.args.length, 1, 1);
          const raw = node.args[0];
          if (!raw || raw.type !== "Var") throw new Error("incrementer(arg) must be a variable name");
          const varName = raw.name;
          return (ctx) => {
            const v = toNumber(ctx.vars[varName] ?? 0) + 1;
            ctx.vars[varName] = v;
            return v;
          };
        }

        if (name === "decrementer") {
          arity(name, node.args.length, 1, 1);
          const raw = node.args[0];
          if (!raw || raw.type !== "Var") throw new Error("decrementer(arg) must be a variable name");
          const varName = raw.name;
          return (ctx) => {
            const v = toNumber(ctx.vars[varName] ?? 0) - 1;
            ctx.vars[varName] = v;
            return v;
          };
        }

        if (name === "defaultor") {
          arity(name, node.args.length, 2, 2);
          const xFn = argFns[0];
          const fbFn = argFns[1];
          return (ctx) => {
            const v = xFn(ctx);
            return v === null || v === undefined ? fbFn(ctx) : v;
          };
        }

        if (name === "coalescer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            for (let i = 0; i < argFns.length; i++) {
              const v = argFns[i](ctx);
              if (v !== null && v !== undefined) return v;
            }
            return undefined;
          };
        }

        if (name === "tryer") {
          arity(name, node.args.length, 2, 2);
          const xFn = argFns[0];
          const fbFn = argFns[1];
          return (ctx) => {
            try {
              return xFn(ctx);
            } catch (_) {
              return fbFn(ctx);
            }
          };
        }

        if (name === "thrower") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => {
            const v = argFns[0](ctx);
            throw new Error(String(v));
          };
        }

        /* ---------- IO ---------- */

        if (name === "logger") {
          arity(name, node.args.length, 1, 1);
          return (ctx) => {
            chargeOps(ctx, "logger", 1);
            const v = argFns[0](ctx);
            if (ctx.output.length < maxOutput) ctx.output.push(v);
            if (isFn(loggerFn)) loggerFn(v);
            return v;
          };
        }

        /* ---------- tuple/array helpers ---------- */

        if (name === "lengther") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const v = xFn(ctx);
            if (Array.isArray(v) || typeof v === "string") return v.length;
            return 0;
          };
        }

        if (name === "indexer") {
          arity(name, node.args.length, 2, 2);
          const xFn = argFns[0];
          const iFn = argFns[1];
          return (ctx) => {
            const v = xFn(ctx);
            const idx = toInt(iFn(ctx));
            if (Array.isArray(v) || typeof v === "string") return v[idx];
            return undefined;
          };
        }

        if (name === "sliceer") {
          arity(name, node.args.length, 2, 3);
          const xFn = argFns[0];
          const sFn = argFns[1];
          const eFn = argFns[2] || (() => undefined);
          return (ctx) => {
            const v = xFn(ctx);
            const s = toInt(sFn(ctx));
            const e = eFn(ctx);
            const ee = e === undefined ? undefined : toInt(e);
            if (Array.isArray(v) || typeof v === "string") return v.slice(s, ee);
            return Array.isArray(v) ? [] : "";
          };
        }

        if (name === "joiner") {
          arity(name, node.args.length, 2, 2);
          const arrFn = argFns[0];
          const sepFn = argFns[1];
          return (ctx) => {
            const a = arrFn(ctx);
            const sep = String(sepFn(ctx));
            if (!Array.isArray(a)) return "";
            return a.map((x) => String(x)).join(sep);
          };
        }

        if (name === "concatener") {
          return (ctx) => evalArgs(ctx).map((x) => String(x)).join("");
        }

        if (name === "pusher") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const vFn = argFns[1];
          return (ctx) => {
            const a = aFn(ctx);
            if (!Array.isArray(a)) throw new Error("pusher: first arg must be an array (tuple)");
            a.push(vFn(ctx));
            return a.length;
          };
        }

        if (name === "popper") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => {
            const a = aFn(ctx);
            if (!Array.isArray(a)) throw new Error("popper: first arg must be an array (tuple)");
            return a.pop();
          };
        }

        if (name === "unshifter") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => {
            const a = aFn(ctx);
            if (!Array.isArray(a)) throw new Error("unshifter: first arg must be an array (tuple)");
            return a.shift();
          };
        }

        if (name === "shifter") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const vFn = argFns[1];
          return (ctx) => {
            const a = aFn(ctx);
            if (!Array.isArray(a)) throw new Error("shifter: first arg must be an array (tuple)");
            a.unshift(vFn(ctx));
            return a.length;
          };
        }

        if (name === "setIndexer") {
          arity(name, node.args.length, 3, 3);
          const aFn = argFns[0];
          const iFn = argFns[1];
          const vFn = argFns[2];
          return (ctx) => {
            const a = aFn(ctx);
            if (!Array.isArray(a)) throw new Error("setIndexer: first arg must be an array (tuple)");
            const idx = toInt(iFn(ctx));
            const v = vFn(ctx);
            a[idx] = v;
            return v;
          };
        }

        if (name === "cloner") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const v = xFn(ctx);
            if (Array.isArray(v)) return v.slice();
            if (isObj(v)) return Object.assign(Object.create(null), v);
            return v;
          };
        }

        if (name === "rangeer") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0];
          const eFn = argFns[1];
          const stFn = argFns[2] || null;
          return (ctx) => {
            chargeOps(ctx, "rangeer:init", 1);
            const start = toInt(sFn(ctx));
            const end = toInt(eFn(ctx));
            let step = stFn ? toInt(stFn(ctx)) : start <= end ? 1 : -1;
            if (step === 0) throw new Error("rangeer: step must not be 0");
            const forward = step > 0;
            const out = [];
            for (let k = start; forward ? k < end : k > end; k += step) {
              chargeLoop(ctx, "rangeer");
              out.push(k);
            }
            return out;
          };
        }

        if (name === "mapper") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "mapper:init", 1);
            const a = aFn(ctx);
            const fn = fFn(ctx);
            if (!Array.isArray(a)) throw new Error("mapper: first arg must be an array");
            if (typeof fn !== "function") throw new Error("mapper: second arg must be a function");
            const out = new Array(a.length);
            for (let i = 0; i < a.length; i++) {
              chargeLoop(ctx, "mapper");
              out[i] = invokeGuarded(ctx, fn, [a[i], i, a], "mapper:callback");
            }
            return out;
          };
        }

        if (name === "filterer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "filterer:init", 1);
            const a = aFn(ctx);
            const fn = fFn(ctx);
            if (!Array.isArray(a)) throw new Error("filterer: first arg must be an array");
            if (typeof fn !== "function") throw new Error("filterer: second arg must be a function");
            const out = [];
            for (let i = 0; i < a.length; i++) {
              chargeLoop(ctx, "filterer");
              if (invokeGuarded(ctx, fn, [a[i], i, a], "filterer:callback")) out.push(a[i]);
            }
            return out;
          };
        }

        if (name === "reducer") {
          arity(name, node.args.length, 3, 3);
          const aFn = argFns[0];
          const fFn = argFns[1];
          const initFn = argFns[2];
          return (ctx) => {
            chargeOps(ctx, "reducer:init", 1);
            const a = aFn(ctx);
            const fn = fFn(ctx);
            if (!Array.isArray(a)) throw new Error("reducer: first arg must be an array");
            if (typeof fn !== "function") throw new Error("reducer: second arg must be a function");
            let acc = initFn(ctx);
            for (let i = 0; i < a.length; i++) {
              chargeLoop(ctx, "reducer");
              acc = invokeGuarded(ctx, fn, [acc, a[i], i, a], "reducer:callback");
            }
            return acc;
          };
        }

        if (name === "everyer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "everyer:init", 1);
            const a = asArray(aFn(ctx), "everyer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("everyer: second arg must be a function");
            for (let i = 0; i < a.length; i++) {
              chargeLoop(ctx, "everyer");
              if (!invokeGuarded(ctx, fn, [a[i], i, a], "everyer:callback")) return false;
            }
            return true;
          };
        }

        if (name === "somer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "somer:init", 1);
            const a = asArray(aFn(ctx), "somer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("somer: second arg must be a function");
            for (let i = 0; i < a.length; i++) {
              chargeLoop(ctx, "somer");
              if (invokeGuarded(ctx, fn, [a[i], i, a], "somer:callback")) return true;
            }
            return false;
          };
        }

        if (name === "finder") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "finder");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("finder: second arg must be a function");
            for (let i = 0; i < a.length; i++) if (fn(a[i], i, a)) return a[i];
            return undefined;
          };
        }

        if (name === "findIndexer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "findIndexer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("findIndexer: second arg must be a function");
            for (let i = 0; i < a.length; i++) if (fn(a[i], i, a)) return i;
            return -1;
          };
        }

        if (name === "includer") {
          arity(name, node.args.length, 2, 2);
          const xFn = argFns[0];
          const vFn = argFns[1];
          return (ctx) => {
            const x = xFn(ctx);
            const v = vFn(ctx);
            if (typeof x === "string") return x.includes(String(v));
            if (isArr(x)) return x.includes(v);
            return false;
          };
        }

        if (name === "flatter") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0];
          const dFn = argFns[1] || (() => 1);
          return (ctx) => flattenWithDepth(asArray(aFn(ctx), "flatter"), normalizeDepth(dFn(ctx)));
        }

        if (name === "flatMaper") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "flatMaper");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("flatMaper: second arg must be a function");
            const out = [];
            for (let i = 0; i < a.length; i++) {
              const v = fn(a[i], i, a);
              if (isArr(v)) out.push(...v);
              else out.push(v);
            }
            return out;
          };
        }

        if (name === "sorter") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0];
          const modeFn = argFns[1] || (() => "asc");
          return (ctx) => {
            const a = asArray(aFn(ctx), "sorter");
            const modeRaw = modeFn(ctx);
            if (isFn(modeRaw)) return a.slice().sort((x, y) => toNumber(modeRaw(x, y)));
            const mode = String(modeRaw).toLowerCase() === "desc" ? "desc" : "asc";
            return sortArray(a, mode);
          };
        }

        if (name === "reverser") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => asArray(aFn(ctx), "reverser").slice().reverse();
        }

        if (name === "uniquer") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => Array.from(new Set(asArray(aFn(ctx), "uniquer")));
        }

        if (name === "deepCloner") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => deepClone(xFn(ctx));
        }

        if (name === "chunker") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], sFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "chunker");
            const size = Math.max(1, toInt(sFn(ctx)));
            const out = [];
            for (let i = 0; i < a.length; i += size) out.push(a.slice(i, i + size));
            return out;
          };
        }

        if (name === "zipper") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => zipArrays(evalArgs(ctx));
        }

        if (name === "unzipper") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => {
            const zipped = asArray(aFn(ctx), "unzipper");
            if (zipped.length === 0) return [];
            const first = asArray(zipped[0], "unzipper");
            const cols = first.length;
            const out = new Array(cols);
            for (let c = 0; c < cols; c++) out[c] = [];
            for (let r = 0; r < zipped.length; r++) {
              const row = asArray(zipped[r], "unzipper");
              for (let c = 0; c < cols; c++) out[c].push(row[c]);
            }
            return out;
          };
        }

        if (name === "firster") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0], fbFn = argFns[1] || (() => undefined);
          return (ctx) => {
            const a = asArray(aFn(ctx), "firster");
            return a.length > 0 ? a[0] : fbFn(ctx);
          };
        }

        if (name === "laster") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0], fbFn = argFns[1] || (() => undefined);
          return (ctx) => {
            const a = asArray(aFn(ctx), "laster");
            return a.length > 0 ? a[a.length - 1] : fbFn(ctx);
          };
        }

        if (name === "findLaster") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "findLaster");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("findLaster: second arg must be a function");
            for (let i = a.length - 1; i >= 0; i--) if (fn(a[i], i, a)) return a[i];
            return undefined;
          };
        }

        if (name === "findLastIndexer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "findLastIndexer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("findLastIndexer: second arg must be a function");
            for (let i = a.length - 1; i >= 0; i--) if (fn(a[i], i, a)) return i;
            return -1;
          };
        }

        if (name === "partitioner") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "partitioner");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("partitioner: second arg must be a function");
            const yes = [];
            const no = [];
            for (let i = 0; i < a.length; i++) (fn(a[i], i, a) ? yes : no).push(a[i]);
            return [yes, no];
          };
        }

        if (name === "groupByer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], kFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "groupByer");
            const keyer = kFn(ctx);
            if (isFn(keyer)) return groupByArray(a, keyer);
            const keyPath = keyer;
            return groupByArray(a, (item) => pathGet(item, keyPath, ""));
          };
        }

        if (name === "countByer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], kFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "countByer");
            const keyer = kFn(ctx);
            if (isFn(keyer)) return countByArray(a, keyer);
            const keyPath = keyer;
            return countByArray(a, (item) => pathGet(item, keyPath, ""));
          };
        }

        if (name === "plucker") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], pFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "plucker");
            const path = pFn(ctx);
            return a.map((item) => pathGet(item, path, undefined));
          };
        }

        if (name === "rotateer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], sFn = argFns[1];
          return (ctx) => rotateArray(aFn(ctx), sFn(ctx));
        }

        if (name === "shuffleer") {
          arity(name, node.args.length, 1, 1);
          const aFn = argFns[0];
          return (ctx) => shuffleArray(aFn(ctx));
        }

        if (name === "sampleer") {
          arity(name, node.args.length, 1, 2);
          const aFn = argFns[0], nFn = argFns[1] || null;
          return (ctx) => {
            const a = asArray(aFn(ctx), "sampleer");
            if (!nFn) return a.length === 0 ? undefined : a[Math.floor(Math.random() * a.length)];
            const n = Math.max(0, toInt(nFn(ctx)));
            if (n === 0) return [];
            if (n >= a.length) return shuffleArray(a);
            return shuffleArray(a).slice(0, n);
          };
        }

        if (name === "inserter") {
          // inserter(arr, index, ...values) mutates target array
          arity(name, node.args.length, 3, Infinity);
          const aFn = argFns[0], iFn = argFns[1];
          const vFns = argFns.slice(2);
          return (ctx) => {
            const a = asArray(aFn(ctx), "inserter");
            const idx = normalizeIndex(a.length, iFn(ctx));
            const values = vFns.map((fn) => fn(ctx));
            a.splice(idx, 0, ...values);
            return a.length;
          };
        }

        if (name === "splicer") {
          // splicer(arr, start, deleteCount?, ...items) mutates target and returns removed items
          arity(name, node.args.length, 2, Infinity);
          const aFn = argFns[0], sFn = argFns[1];
          const dFn = argFns[2] || null;
          const iFns = argFns.slice(dFn ? 3 : 2);
          return (ctx) => {
            const a = asArray(aFn(ctx), "splicer");
            const start = normalizeIndex(a.length, sFn(ctx));
            const del = dFn ? Math.max(0, toInt(dFn(ctx))) : a.length - start;
            const items = iFns.map((fn) => fn(ctx));
            return a.splice(start, del, ...items);
          };
        }

        /* ---------- object helpers ---------- */

        if (name === "objecter") {
          const pairFns = node.args.map((arg, idx) => {
            if (arg.type !== "Tuple" || arg.items.length !== 2) {
              throw new Error(`objecter: arg ${idx} must be a tuple (key,value)`);
            }
            const kFn = c(arg.items[0]);
            const vFn = c(arg.items[1]);
            return { kFn, vFn };
          });
          return (ctx) => {
            const o = Object.create(null);
            for (const { kFn, vFn } of pairFns) o[String(kFn(ctx))] = vFn(ctx);
            return o;
          };
        }

        if (name === "geter") {
          arity(name, node.args.length, 2, 3);
          const oFn = argFns[0];
          const kFn = argFns[1];
          const fbFn = argFns[2] || (() => undefined);
          return (ctx) => {
            const o = oFn(ctx);
            const k = String(kFn(ctx));
            if (!isObj(o)) return fbFn(ctx);
            return hasOwn.call(o, k) ? o[k] : fbFn(ctx);
          };
        }

        if (name === "seter") {
          arity(name, node.args.length, 3, 3);
          const oFn = argFns[0];
          const kFn = argFns[1];
          const vFn = argFns[2];
          return (ctx) => {
            const o = oFn(ctx);
            if (!isObj(o)) throw new Error("seter: first arg must be an object");
            const k = String(kFn(ctx));
            const v = vFn(ctx);
            o[k] = v;
            return v;
          };
        }

        if (name === "keyser") {
          arity(name, node.args.length, 1, 1);
          const oFn = argFns[0];
          return (ctx) => {
            const o = oFn(ctx);
            if (!isObj(o)) return [];
            return Object.keys(o);
          };
        }

        if (name === "valuer") {
          arity(name, node.args.length, 1, 1);
          const oFn = argFns[0];
          return (ctx) => {
            const o = oFn(ctx);
            if (!isObj(o)) return [];
            return Object.keys(o).map((k) => o[k]);
          };
        }

        if (name === "merger") {
          arity(name, node.args.length, 2, Infinity);
          return (ctx) => {
            const objs = evalArgs(ctx);
            const out = Object.create(null);
            for (const o of objs) {
              if (!isObj(o)) continue;
              for (const k of Object.keys(o)) out[k] = o[k];
            }
            return out;
          };
        }

        if (name === "haser") {
          arity(name, node.args.length, 2, 2);
          const oFn = argFns[0];
          const kFn = argFns[1];
          return (ctx) => {
            const o = oFn(ctx);
            if (!isObj(o)) return false;
            return hasOwn.call(o, String(kFn(ctx)));
          };
        }

        if (name === "delKeyer") {
          arity(name, node.args.length, 2, 2);
          const oFn = argFns[0];
          const kFn = argFns[1];
          return (ctx) => {
            const o = oFn(ctx);
            if (!isObj(o)) return false;
            const k = String(kFn(ctx));
            if (hasOwn.call(o, k)) {
              delete o[k];
              return true;
            }
            return false;
          };
        }

        if (name === "entrieser") {
          arity(name, node.args.length, 1, 1);
          const oFn = argFns[0];
          return (ctx) => {
            const o = oFn(ctx);
            if (!isObj(o)) return [];
            const keys = Object.keys(o);
            const out = new Array(keys.length);
            for (let i = 0; i < keys.length; i++) {
              const k = keys[i];
              out[i] = [k, o[k]];
            }
            return out;
          };
        }

        if (name === "fromEntrieser") {
          arity(name, node.args.length, 1, 1);
          const eFn = argFns[0];
          return (ctx) => unzipObjectEntries(eFn(ctx));
        }

        if (name === "deepMerger") {
          arity(name, node.args.length, 2, Infinity);
          return (ctx) => {
            const objs = evalArgs(ctx);
            let out = Object.create(null);
            for (let i = 0; i < objs.length; i++) {
              const o = objs[i];
              if (!isObj(o)) continue;
              out = deepMergeValues(out, o);
            }
            return out;
          };
        }

        if (name === "pathGeter") {
          arity(name, node.args.length, 2, 3);
          const oFn = argFns[0], pFn = argFns[1], fbFn = argFns[2] || (() => undefined);
          return (ctx) => pathGet(oFn(ctx), pFn(ctx), fbFn(ctx));
        }

        if (name === "pathSeter") {
          arity(name, node.args.length, 3, 3);
          const oFn = argFns[0], pFn = argFns[1], vFn = argFns[2];
          return (ctx) => pathSet(oFn(ctx), pFn(ctx), vFn(ctx));
        }

        if (name === "pathDeleter") {
          arity(name, node.args.length, 2, 2);
          const oFn = argFns[0], pFn = argFns[1];
          return (ctx) => pathDelete(oFn(ctx), pFn(ctx));
        }

        if (name === "picker") {
          arity(name, node.args.length, 2, Infinity);
          const oFn = argFns[0];
          const kFns = argFns.slice(1);
          return (ctx) => {
            const o = oFn(ctx);
            const out = Object.create(null);
            if (!isObj(o)) return out;
            for (let i = 0; i < kFns.length; i++) {
              const k = String(kFns[i](ctx));
              if (hasOwn.call(o, k)) out[k] = o[k];
            }
            return out;
          };
        }

        if (name === "omitter") {
          arity(name, node.args.length, 2, Infinity);
          const oFn = argFns[0];
          const kFns = argFns.slice(1);
          return (ctx) => {
            const o = oFn(ctx);
            const out = Object.create(null);
            if (!isObj(o)) return out;
            const drop = Object.create(null);
            for (let i = 0; i < kFns.length; i++) drop[String(kFns[i](ctx))] = true;
            for (const k of Object.keys(o)) if (!drop[k]) out[k] = o[k];
            return out;
          };
        }

        if (name === "mapObjecter") {
          arity(name, node.args.length, 2, 2);
          const oFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const o = oFn(ctx);
            const fn = fFn(ctx);
            if (!isObj(o)) return Object.create(null);
            if (!isFn(fn)) throw new Error("mapObjecter: second arg must be a function");
            const out = Object.create(null);
            for (const k of Object.keys(o)) out[k] = fn(o[k], k, o);
            return out;
          };
        }

        if (name === "filterObjecter") {
          arity(name, node.args.length, 2, 2);
          const oFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const o = oFn(ctx);
            const fn = fFn(ctx);
            if (!isObj(o)) return Object.create(null);
            if (!isFn(fn)) throw new Error("filterObjecter: second arg must be a function");
            const out = Object.create(null);
            for (const k of Object.keys(o)) if (fn(o[k], k, o)) out[k] = o[k];
            return out;
          };
        }

        /* ---------- object-oriented helpers ---------- */

        if (name === "classer") {
          // classer(name, methodsObj, baseClass?, staticMethodsObj?)
          arity(name, node.args.length, 2, 4);
          const nameFn = argFns[0];
          const methodsFn = argFns[1];
          const baseFn = argFns[2] || (() => null);
          const staticFn = argFns[3] || (() => null);
          return (ctx) => createClass(nameFn(ctx), methodsFn(ctx), baseFn(ctx), staticFn(ctx));
        }

        if (name === "subclasser") {
          // subclasser(baseClass, name, methodsObj, staticMethodsObj?)
          arity(name, node.args.length, 3, 4);
          const baseFn = argFns[0];
          const nameFn = argFns[1];
          const methodsFn = argFns[2];
          const staticFn = argFns[3] || (() => null);
          return (ctx) => createClass(nameFn(ctx), methodsFn(ctx), baseFn(ctx), staticFn(ctx));
        }

        if (name === "newer") {
          // newer(classObj, ...args) -> instance, calls init(self, ...args) if present
          arity(name, node.args.length, 1, Infinity);
          const clsFn = argFns[0];
          const restFns = argFns.slice(1);
          return (ctx) => {
            chargeOps(ctx, "newer:init", 1);
            const cls = asGarrigaleClass(clsFn(ctx), "newer(first arg)");
            const inst = makeInstance(cls);
            const initMethod = findClassMethod(cls, "init");
            if (!initMethod) return inst;
            const args = restFns.map((fn) => fn(ctx));
            const ret = invokeGuarded(ctx, initMethod.fn, [inst, ...args], "newer:init");
            return ret === undefined ? inst : ret;
          };
        }

        if (name === "caller") {
          // caller(instanceOrClass, methodName, ...args)
          arity(name, node.args.length, 2, Infinity);
          const targetFn = argFns[0];
          const nameFn = argFns[1];
          const restFns = argFns.slice(2);
          return (ctx) => {
            chargeOps(ctx, "caller:init", 1);
            const target = targetFn(ctx);
            const mName = String(nameFn(ctx));
            const args = restFns.map((fn) => fn(ctx));
            if (isGarrigaleInstance(target)) {
              return invokeGuarded(ctx, (...x) => invokeClassMethod(target, mName, x, false), args, `caller:${mName}`);
            }
            if (isGarrigaleClass(target)) {
              return invokeGuarded(ctx, (...x) => invokeStaticMethod(target, mName, x), args, `caller:${mName}`);
            }
            if (isPlainObject(target) && isFn(target[mName])) {
              return invokeGuarded(ctx, target[mName], args, `caller:${mName}`);
            }
            throw new Error("caller: target must be Garrigale instance/class or object with callable property");
          };
        }

        if (name === "superCaller") {
          // superCaller(instance, methodName, ...args)
          arity(name, node.args.length, 2, Infinity);
          const instFn = argFns[0];
          const nameFn = argFns[1];
          const restFns = argFns.slice(2);
          return (ctx) => {
            chargeOps(ctx, "superCaller:init", 1);
            const inst = asGarrigaleInstance(instFn(ctx), "superCaller(first arg)");
            const mName = String(nameFn(ctx));
            const args = restFns.map((fn) => fn(ctx));
            return invokeGuarded(ctx, (...x) => invokeClassMethod(inst, mName, x, true), args, `superCaller:${mName}`);
          };
        }

        if (name === "methoder") {
          // methoder(classObj, methodName, fn) -> fn
          arity(name, node.args.length, 3, 3);
          const clsFn = argFns[0], nameFn = argFns[1], fnFn = argFns[2];
          return (ctx) => {
            const cls = asGarrigaleClass(clsFn(ctx), "methoder(first arg)");
            const mName = String(nameFn(ctx));
            const fn = fnFn(ctx);
            if (!isFn(fn)) throw new Error("methoder: third arg must be a function");
            cls.methods[mName] = fn;
            return fn;
          };
        }

        if (name === "staticMethoder") {
          // staticMethoder(classObj, methodName, fn) -> fn
          arity(name, node.args.length, 3, 3);
          const clsFn = argFns[0], nameFn = argFns[1], fnFn = argFns[2];
          return (ctx) => {
            const cls = asGarrigaleClass(clsFn(ctx), "staticMethoder(first arg)");
            const mName = String(nameFn(ctx));
            const fn = fnFn(ctx);
            if (!isFn(fn)) throw new Error("staticMethoder: third arg must be a function");
            cls.staticMethods[mName] = fn;
            return fn;
          };
        }

        if (name === "getFielder") {
          arity(name, node.args.length, 2, 3);
          const instFn = argFns[0], keyFn = argFns[1], fbFn = argFns[2] || (() => undefined);
          return (ctx) => {
            const inst = asGarrigaleInstance(instFn(ctx), "getFielder(first arg)");
            const key = String(keyFn(ctx));
            return hasOwn.call(inst.fields, key) ? inst.fields[key] : fbFn(ctx);
          };
        }

        if (name === "setFielder") {
          arity(name, node.args.length, 3, 3);
          const instFn = argFns[0], keyFn = argFns[1], valFn = argFns[2];
          return (ctx) => {
            const inst = asGarrigaleInstance(instFn(ctx), "setFielder(first arg)");
            const key = String(keyFn(ctx));
            const value = valFn(ctx);
            inst.fields[key] = value;
            return value;
          };
        }

        if (name === "hasFielder") {
          arity(name, node.args.length, 2, 2);
          const instFn = argFns[0], keyFn = argFns[1];
          return (ctx) => {
            const inst = asGarrigaleInstance(instFn(ctx), "hasFielder(first arg)");
            return hasOwn.call(inst.fields, String(keyFn(ctx)));
          };
        }

        if (name === "delFielder") {
          arity(name, node.args.length, 2, 2);
          const instFn = argFns[0], keyFn = argFns[1];
          return (ctx) => {
            const inst = asGarrigaleInstance(instFn(ctx), "delFielder(first arg)");
            const key = String(keyFn(ctx));
            if (hasOwn.call(inst.fields, key)) {
              delete inst.fields[key];
              return true;
            }
            return false;
          };
        }

        if (name === "staticGeter") {
          arity(name, node.args.length, 2, 3);
          const clsFn = argFns[0], keyFn = argFns[1], fbFn = argFns[2] || (() => undefined);
          return (ctx) => {
            const cls = asGarrigaleClass(clsFn(ctx), "staticGeter(first arg)");
            const key = String(keyFn(ctx));
            const found = findClassStaticField(cls, key);
            return found ? found.value : fbFn(ctx);
          };
        }

        if (name === "staticSeter") {
          arity(name, node.args.length, 3, 3);
          const clsFn = argFns[0], keyFn = argFns[1], valFn = argFns[2];
          return (ctx) => {
            const cls = asGarrigaleClass(clsFn(ctx), "staticSeter(first arg)");
            const key = String(keyFn(ctx));
            const value = valFn(ctx);
            cls.staticFields[key] = value;
            return value;
          };
        }

        if (name === "classNameer") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const x = xFn(ctx);
            if (isGarrigaleClass(x)) return x.name;
            if (isGarrigaleInstance(x)) return x.cls.name;
            return "";
          };
        }

        if (name === "instanceOfer") {
          arity(name, node.args.length, 2, 2);
          const instFn = argFns[0], clsFn = argFns[1];
          return (ctx) => isInstanceOfClass(instFn(ctx), clsFn(ctx));
        }

        if (name === "methodNameser") {
          // methodNameser(classOrInstance, includeInherited?)
          arity(name, node.args.length, 1, 2);
          const targetFn = argFns[0];
          const inhFn = argFns[1] || (() => true);
          return (ctx) => {
            const target = targetFn(ctx);
            const includeInherited = !!inhFn(ctx);
            const cls = isGarrigaleClass(target) ? target : isGarrigaleInstance(target) ? target.cls : null;
            if (!cls) return [];
            return collectMethodNames(cls, includeInherited);
          };
        }

        if (name === "isClasser") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => isGarrigaleClass(xFn(ctx));
        }

        if (name === "isInstanceer") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => isGarrigaleInstance(xFn(ctx));
        }

        if (name === "classBaseer") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const x = xFn(ctx);
            if (isGarrigaleClass(x)) return x.base;
            if (isGarrigaleInstance(x)) return x.cls.base;
            return null;
          };
        }

        if (name === "classChainser") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const x = xFn(ctx);
            let cur = isGarrigaleClass(x) ? x : isGarrigaleInstance(x) ? x.cls : null;
            const out = [];
            while (cur) {
              out.push(cur.name);
              cur = cur.base;
            }
            return out;
          };
        }

        if (name === "fieldKeyser") {
          arity(name, node.args.length, 1, 1);
          const iFn = argFns[0];
          return (ctx) => {
            const inst = asGarrigaleInstance(iFn(ctx), "fieldKeyser(first arg)");
            return Object.keys(inst.fields);
          };
        }

        if (name === "staticKeyser") {
          arity(name, node.args.length, 1, 1);
          const cFn = argFns[0];
          return (ctx) => {
            const cls = asGarrigaleClass(cFn(ctx), "staticKeyser(first arg)");
            const out = [];
            const seen = Object.create(null);
            let cur = cls;
            while (cur) {
              for (const k of Object.keys(cur.staticFields)) {
                if (!seen[k]) {
                  seen[k] = true;
                  out.push(k);
                }
              }
              cur = cur.base;
            }
            return out;
          };
        }

        if (name === "bindMethoder") {
          arity(name, node.args.length, 2, 2);
          const iFn = argFns[0], mFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "bindMethoder:init", 1);
            const inst = asGarrigaleInstance(iFn(ctx), "bindMethoder(first arg)");
            const mName = String(mFn(ctx));
            return (...args) => invokeGuarded(ctx, (...x) => invokeClassMethod(inst, mName, x, false), args, `bindMethoder:${mName}`);
          };
        }

        if (name === "constructer") {
          arity(name, node.args.length, 1, Infinity);
          const clsFn = argFns[0];
          const restFns = argFns.slice(1);
          return (ctx) => {
            chargeOps(ctx, "constructer:init", 1);
            const cls = asGarrigaleClass(clsFn(ctx), "constructer(first arg)");
            const inst = makeInstance(cls);
            const initMethod = findClassMethod(cls, "init");
            if (!initMethod) return inst;
            const args = restFns.map((fn) => fn(ctx));
            const ret = invokeGuarded(ctx, initMethod.fn, [inst, ...args], "constructer:init");
            return ret === undefined ? inst : ret;
          };
        }

        /* ---------- function values ---------- */

        if (name === "funner") {
          arity(name, node.args.length, 2, 2);
          const paramsAst = node.args[0];
          let paramNames;
          if (paramsAst && paramsAst.type === "Tuple") {
            for (const it of paramsAst.items) if (it.type !== "Var") throw new Error("funner: params must be variable names");
            paramNames = paramsAst.items.map((v) => v.name);
          } else if (paramsAst && paramsAst.type === "Var") {
            // parser collapses single-item tuple, so allow funner(x, body)
            paramNames = [paramsAst.name];
          } else {
            throw new Error("funner: first arg must be a parameter name or tuple of parameter names");
          }
          const bodyFn = argFns[1];

          return (ctx) => {
            return (...args) => {
              enterCall(ctx, "funner");
              // save existing bindings precisely (had + value)
              const saved = Object.create(null);
              for (const pn of paramNames) {
                saved[pn] = {
                  had: hasOwn.call(ctx.vars, pn),
                  val: ctx.vars[pn],
                };
              }
              // bind
              for (let i = 0; i < paramNames.length; i++) ctx.vars[paramNames[i]] = args[i];
              try {
                return bodyFn(ctx);
              } finally {
                for (const pn of paramNames) {
                  const s = saved[pn];
                  if (s.had) ctx.vars[pn] = s.val;
                  else delete ctx.vars[pn];
                }
                leaveCall(ctx);
              }
            };
          };
        }

        if (name === "applyer") {
          arity(name, node.args.length, 1, Infinity);
          const fFn = argFns[0];
          const restFns = argFns.slice(1);
          return (ctx) => {
            chargeOps(ctx, "applyer:init", 1);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("applyer: first arg must be a function");
            const args = restFns.map((g) => g(ctx));
            return invokeGuarded(ctx, fn, args, "applyer");
          };
        }

        if (name === "partialer") {
          arity(name, node.args.length, 1, Infinity);
          const fFn = argFns[0];
          const boundFns = argFns.slice(1);
          return (ctx) => {
            chargeOps(ctx, "partialer:init", 1);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("partialer: first arg must be a function");
            const bound = boundFns.map((g) => g(ctx));
            return (...rest) => invokeGuarded(ctx, fn, [...bound, ...rest], "partialer:call");
          };
        }

        if (name === "pipeer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            chargeOps(ctx, "pipeer:init", 1);
            const fns = argFns.map((g) => g(ctx));
            for (let i = 0; i < fns.length; i++) if (!isFn(fns[i])) throw new Error("pipeer: all args must be functions");
            return (...args) => {
              let v = invokeGuarded(ctx, fns[0], args, "pipeer:step0");
              for (let i = 1; i < fns.length; i++) v = invokeGuarded(ctx, fns[i], [v], `pipeer:step${i}`);
              return v;
            };
          };
        }

        if (name === "composeer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            chargeOps(ctx, "composeer:init", 1);
            const fns = argFns.map((g) => g(ctx));
            for (let i = 0; i < fns.length; i++) if (!isFn(fns[i])) throw new Error("composeer: all args must be functions");
            return (...args) => {
              let v = invokeGuarded(ctx, fns[fns.length - 1], args, `composeer:step${fns.length - 1}`);
              for (let i = fns.length - 2; i >= 0; i--) v = invokeGuarded(ctx, fns[i], [v], `composeer:step${i}`);
              return v;
            };
          };
        }

        if (name === "memoizer") {
          arity(name, node.args.length, 1, 2);
          const fFn = argFns[0];
          const keyFnExpr = argFns[1] || null;
          return (ctx) => {
            chargeOps(ctx, "memoizer:init", 1);
            const fn = fFn(ctx);
            const keyFn = keyFnExpr ? keyFnExpr(ctx) : null;
            if (!isFn(fn)) throw new Error("memoizer: first arg must be a function");
            if (keyFn !== null && !isFn(keyFn)) throw new Error("memoizer: second arg must be a function");
            const cache = new Map();
            return (...args) => {
              let key;
              if (keyFn) key = invokeGuarded(ctx, keyFn, args, "memoizer:keyFn");
              else {
                try {
                  key = JSON.stringify(args);
                } catch (_) {
                  key = args.map((a) => `${typeof a}:${String(a)}`).join("|");
                }
              }
              if (cache.has(key)) return cache.get(key);
              const v = invokeGuarded(ctx, fn, args, "memoizer:fn");
              cache.set(key, v);
              return v;
            };
          };
        }

        if (name === "onceer") {
          arity(name, node.args.length, 1, 1);
          const fFn = argFns[0];
          return (ctx) => {
            chargeOps(ctx, "onceer:init", 1);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("onceer: first arg must be a function");
            let called = false;
            let value = undefined;
            return (...args) => {
              if (!called) {
                called = true;
                value = invokeGuarded(ctx, fn, args, "onceer:fn");
              }
              return value;
            };
          };
        }

        if (name === "curryer") {
          arity(name, node.args.length, 1, 2);
          const fFn = argFns[0];
          const nFn = argFns[1] || null;
          return (ctx) => {
            chargeOps(ctx, "curryer:init", 1);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("curryer: first arg must be a function");
            const expected = nFn ? Math.max(0, toInt(nFn(ctx))) : Math.max(0, fn.length);
            const curry = (collected) => (...args) => {
              const next = collected.concat(args);
              if (next.length >= expected) return invokeGuarded(ctx, fn, next, "curryer:fn");
              return curry(next);
            };
            return curry([]);
          };
        }

        if (name === "spreadApplyer") {
          arity(name, node.args.length, 2, 2);
          const fFn = argFns[0];
          const aFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "spreadApplyer:init", 1);
            const fn = fFn(ctx);
            const args = asArray(aFn(ctx), "spreadApplyer(second arg)");
            if (!isFn(fn)) throw new Error("spreadApplyer: first arg must be a function");
            return invokeGuarded(ctx, fn, args, "spreadApplyer");
          };
        }

        if (name === "tapper") {
          arity(name, node.args.length, 2, 2);
          const xFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            chargeOps(ctx, "tapper:init", 1);
            const x = xFn(ctx);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("tapper: second arg must be a function");
            invokeGuarded(ctx, fn, [x], "tapper");
            return x;
          };
        }

        if (name === "constanter") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const x = xFn(ctx);
            return () => x;
          };
        }

        if (name === "predicateNoter") {
          arity(name, node.args.length, 1, 1);
          const fFn = argFns[0];
          return (ctx) => {
            chargeOps(ctx, "predicateNoter:init", 1);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("predicateNoter: first arg must be a function");
            return (...args) => !invokeGuarded(ctx, fn, args, "predicateNoter:fn");
          };
        }

        /* ---------- string helpers ---------- */

        if (name === "replacer") {
          arity(name, node.args.length, 3, 3);
          const sFn = argFns[0], aFn = argFns[1], bFn = argFns[2];
          return (ctx) => {
            const s = String(sFn(ctx));
            const from = String(aFn(ctx));
            const to = String(bFn(ctx));
            if (from === "") return s;
            return s.split(from).join(to);
          };
        }

        if (name === "spliter") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], sepFn = argFns[1];
          return (ctx) => String(sFn(ctx)).split(String(sepFn(ctx)));
        }

        if (name === "trimmer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => String(sFn(ctx)).trim();
        }

        if (name === "lowerer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => String(sFn(ctx)).toLowerCase();
        }

        if (name === "upperer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => String(sFn(ctx)).toUpperCase();
        }

        if (name === "includeser") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], qFn = argFns[1];
          return (ctx) => String(sFn(ctx)).includes(String(qFn(ctx)));
        }

        if (name === "startsWither") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], qFn = argFns[1];
          return (ctx) => String(sFn(ctx)).startsWith(String(qFn(ctx)));
        }

        if (name === "endsWither") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], qFn = argFns[1];
          return (ctx) => String(sFn(ctx)).endsWith(String(qFn(ctx)));
        }

        if (name === "repeater") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], nFn = argFns[1];
          return (ctx) => String(sFn(ctx)).repeat(Math.max(0, toInt(nFn(ctx))));
        }

        if (name === "padStarter") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], nFn = argFns[1], padFn = argFns[2] || (() => " ");
          return (ctx) => String(sFn(ctx)).padStart(Math.max(0, toInt(nFn(ctx))), String(padFn(ctx)));
        }

        if (name === "padEnder") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], nFn = argFns[1], padFn = argFns[2] || (() => " ");
          return (ctx) => String(sFn(ctx)).padEnd(Math.max(0, toInt(nFn(ctx))), String(padFn(ctx)));
        }

        if (name === "jsonStringifyer") {
          arity(name, node.args.length, 1, 2);
          const xFn = argFns[0], spFn = argFns[1] || (() => 0);
          return (ctx) => JSON.stringify(xFn(ctx), null, Math.max(0, toInt(spFn(ctx))));
        }

        if (name === "jsonParser") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => JSON.parse(String(sFn(ctx)));
        }

        if (name === "charAter") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], iFn = argFns[1];
          return (ctx) => {
            const s = String(sFn(ctx));
            const idx = normalizeIndex(s.length, iFn(ctx));
            return s.charAt(idx);
          };
        }

        if (name === "charCodeer") {
          arity(name, node.args.length, 2, 2);
          const sFn = argFns[0], iFn = argFns[1];
          return (ctx) => {
            const s = String(sFn(ctx));
            const idx = normalizeIndex(s.length, iFn(ctx));
            return s.codePointAt(idx);
          };
        }

        if (name === "fromCodePointerer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => fromCodePoints(...evalArgs(ctx));
        }

        if (name === "substringer") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], aFn = argFns[1], bFn = argFns[2] || (() => undefined);
          return (ctx) => {
            const s = String(sFn(ctx));
            const a = toInt(aFn(ctx));
            const bRaw = bFn(ctx);
            const b = bRaw === undefined ? undefined : toInt(bRaw);
            return s.substring(a, b);
          };
        }

        if (name === "regexTester") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], pFn = argFns[1], fFn = argFns[2] || (() => "");
          return (ctx) => {
            const s = String(sFn(ctx));
            const p = String(pFn(ctx));
            const flags = String(fFn(ctx));
            return new RegExp(p, flags).test(s);
          };
        }

        if (name === "regexMatcher") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], pFn = argFns[1], fFn = argFns[2] || (() => "");
          return (ctx) => {
            const s = String(sFn(ctx));
            const p = String(pFn(ctx));
            const flags = String(fFn(ctx));
            return s.match(new RegExp(p, flags));
          };
        }

        if (name === "regexAller") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], pFn = argFns[1], fFn = argFns[2] || (() => "");
          return (ctx) => {
            const s = String(sFn(ctx));
            const p = String(pFn(ctx));
            let flags = String(fFn(ctx));
            if (!flags.includes("g")) flags += "g";
            const rx = new RegExp(p, flags);
            const out = [];
            let m;
            while ((m = rx.exec(s)) !== null) {
              out.push(m[0]);
              if (m[0] === "") rx.lastIndex++;
            }
            return out;
          };
        }

        if (name === "regexReplacer") {
          arity(name, node.args.length, 3, 4);
          const sFn = argFns[0], pFn = argFns[1], rFn = argFns[2], fFn = argFns[3] || (() => "g");
          return (ctx) => {
            const s = String(sFn(ctx));
            const p = String(pFn(ctx));
            const r = String(rFn(ctx));
            const flags = String(fFn(ctx));
            return s.replace(new RegExp(p, flags), r);
          };
        }

        if (name === "regexSpliter") {
          arity(name, node.args.length, 2, 3);
          const sFn = argFns[0], pFn = argFns[1], fFn = argFns[2] || (() => "");
          return (ctx) => String(sFn(ctx)).split(new RegExp(String(pFn(ctx)), String(fFn(ctx))));
        }

        if (name === "regexEscapeer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => escapeRegex(sFn(ctx));
        }

        if (name === "externaler") {
          // externaler(name, ...args) -> call externals[name](...args)
          arity(name, node.args.length, 1, Infinity);
          const nFn = argFns[0];
          const restFns = argFns.slice(1);
          return (ctx) => {
            const externalName = String(nFn(ctx));
            const args = restFns.map((fn) => fn(ctx));
            return callExternal(ctx, externalName, args);
          };
        }

        if (name === "urlEncodeer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => encodeURIComponent(String(sFn(ctx)));
        }

        if (name === "urlDecodeer") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => decodeURIComponent(String(sFn(ctx)));
        }

        if (name === "base64Encoder") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => toBufferUtf8(sFn(ctx)).toString("base64");
        }

        if (name === "base64Decoder") {
          arity(name, node.args.length, 1, 1);
          const sFn = argFns[0];
          return (ctx) => {
            if (typeof Buffer === "undefined") throw new Error("base64Decoder requires Buffer in this runtime");
            return Buffer.from(String(sFn(ctx)), "base64").toString("utf8");
          };
        }

        /* ---------- Matrix builtins (new) ---------- */

        if (name === "matrixer") {
          // matrixer( (r1c1,r1c2,...), (r2c1,r2c2,...), ... ) -> matrix
          // row は tuple(=Array)。中身は number に寄せる（Number(...)）
          arity(name, node.args.length, 0, Infinity);
          return (ctx) => {
            const rows = evalArgs(ctx);
            if (rows.length === 0) return [];
            for (let i = 0; i < rows.length; i++) {
              if (!Array.isArray(rows[i]) || (rows[i].length > 0 && Array.isArray(rows[i][0]))) {
                throw new Error("matrixer: each arg must be a row vector (1D tuple/array)");
              }
            }
            const cols = rows[0].length;
            for (let r = 0; r < rows.length; r++) {
              if (rows[r].length !== cols) throw new Error("matrixer: all rows must have the same length");
            }
            return rows.map((row) => row.map((x) => toNumber(x)));
          };
        }

        if (name === "shaper") {
          // shaper(matrix) -> (rows, cols)
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => {
            const m = mFn(ctx);
            const mm = asMatrix(m, "shaper");
            const s = matrixShape(mm);
            return [s.rows, s.cols];
          };
        }

        if (name === "ismatrixer") {
          // ismatrixer(x) -> boolean
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => isMatrix(xFn(ctx));
        }

        if (name === "matAdder") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => zipMatrix(aFn(ctx), bFn(ctx), (x, y) => toNumber(x) + toNumber(y), "matAdder");
        }

        if (name === "matSubtractor") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => zipMatrix(aFn(ctx), bFn(ctx), (x, y) => toNumber(x) - toNumber(y), "matSubtractor");
        }

        if (name === "matHadamarder") {
          // elementwise multiply
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => zipMatrix(aFn(ctx), bFn(ctx), (x, y) => toNumber(x) * toNumber(y), "matHadamarder");
        }

        if (name === "matScaler") {
          // matScaler(matrix, scalar) -> matrix
          arity(name, node.args.length, 2, 2);
          const mFn = argFns[0], sFn = argFns[1];
          return (ctx) => {
            const m = asMatrix(mFn(ctx), "matScaler");
            const s = toNumber(sFn(ctx));
            return mapMatrix(m, (x) => toNumber(x) * s);
          };
        }

        if (name === "matMultiplier") {
          // matMultiplier(A, B) -> A*B
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => matMul(aFn(ctx), bFn(ctx));
        }

        if (name === "matVecMultiplier") {
          // matVecMultiplier(A, v) -> vector
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], vFn = argFns[1];
          return (ctx) => matVecMul(aFn(ctx), vFn(ctx));
        }

        if (name === "transposer") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => transpose(mFn(ctx));
        }

        if (name === "eyer") {
          // eyeer(n) -> identity n x n
          arity(name, node.args.length, 1, 1);
          const nFn = argFns[0];
          return (ctx) => identity(nFn(ctx));
        }

        if (name === "detter") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => det(mFn(ctx));
        }

        if (name === "solver") {
          // solver(A, b) -> x (vector or matrix)
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => solve(aFn(ctx), bFn(ctx));
        }

        if (name === "matIndexer") {
          // matIndexer(M, r, c)
          arity(name, node.args.length, 3, 3);
          const mFn = argFns[0], rFn = argFns[1], cFn = argFns[2];
          return (ctx) => {
            const m = asMatrix(mFn(ctx), "matIndexer");
            const r = toInt(rFn(ctx));
            const c = toInt(cFn(ctx));
            return m[r]?.[c];
          };
        }

        if (name === "matSeter") {
          // matSeter(M, r, c, v) (in-place)
          arity(name, node.args.length, 4, 4);
          const mFn = argFns[0], rFn = argFns[1], cFn = argFns[2], vFn = argFns[3];
          return (ctx) => {
            const m = asMatrix(mFn(ctx), "matSeter");
            const r = toInt(rFn(ctx));
            const c = toInt(cFn(ctx));
            const v = toNumber(vFn(ctx));
            if (!m[r]) throw new Error("matSeter: row out of range");
            m[r][c] = v;
            return v;
          };
        }

        if (name === "tracer") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => matTrace(mFn(ctx));
        }

        if (name === "dotter") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => dot(aFn(ctx), bFn(ctx), "dotter");
        }

        if (name === "normer") {
          arity(name, node.args.length, 1, 1);
          const vFn = argFns[0];
          return (ctx) => Math.sqrt(dot(vFn(ctx), vFn(ctx), "normer"));
        }

        if (name === "matFlattener") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => {
            const m = asMatrix(mFn(ctx), "matFlattener");
            const out = [];
            for (let r = 0; r < m.length; r++) for (let c = 0; c < m[r].length; c++) out.push(m[r][c]);
            return out;
          };
        }

        if (name === "diagMatrixer") {
          arity(name, node.args.length, 1, 1);
          const vFn = argFns[0];
          return (ctx) => {
            const v = asVector(vFn(ctx), "diagMatrixer");
            const out = new Array(v.length);
            for (let r = 0; r < v.length; r++) {
              const row = new Array(v.length);
              for (let c = 0; c < v.length; c++) row[c] = r === c ? toNumber(v[r]) : 0;
              out[r] = row;
            }
            return out;
          };
        }

        if (name === "matPowerer") {
          arity(name, node.args.length, 2, 2);
          const mFn = argFns[0], eFn = argFns[1];
          return (ctx) => matPow(mFn(ctx), eFn(ctx));
        }

        if (name === "matInverser") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => matInverse(mFn(ctx));
        }

        if (name === "ranker") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => matrixRank(mFn(ctx));
        }

        if (name === "outerProducter") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => matOuter(aFn(ctx), bFn(ctx));
        }

        if (name === "rower") {
          arity(name, node.args.length, 2, 2);
          const mFn = argFns[0], rFn = argFns[1];
          return (ctx) => matrixRow(mFn(ctx), rFn(ctx));
        }

        if (name === "columner") {
          arity(name, node.args.length, 2, 2);
          const mFn = argFns[0], cFn = argFns[1];
          return (ctx) => matrixCol(mFn(ctx), cFn(ctx));
        }

        if (name === "diagVectorer") {
          arity(name, node.args.length, 1, 1);
          const mFn = argFns[0];
          return (ctx) => matrixDiag(mFn(ctx));
        }

        if (name === "kroneckerer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0], bFn = argFns[1];
          return (ctx) => matrixKronecker(aFn(ctx), bFn(ctx));
        }

        if (name === "matMapper") {
          arity(name, node.args.length, 2, 2);
          const mFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const m = asMatrix(mFn(ctx), "matMapper");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("matMapper: second arg must be a function");
            return mapMatrix(m, (x, r, c) => fn(x, r, c, m));
          };
        }

        if (name === "matReducer") {
          arity(name, node.args.length, 3, 3);
          const mFn = argFns[0], fFn = argFns[1], initFn = argFns[2];
          return (ctx) => {
            const m = asMatrix(mFn(ctx), "matReducer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("matReducer: second arg must be a function");
            let acc = initFn(ctx);
            for (let r = 0; r < m.length; r++) for (let c = 0; c < m[r].length; c++) acc = fn(acc, m[r][c], r, c, m);
            return acc;
          };
        }

        /* ---------- fallback: unknown function ---------- */
        return (ctx) => {
          const args = evalArgs(ctx);
          return callExternal(ctx, name, args);
        };
      }

      default:
        throw new Error(`Unknown AST node type: ${node?.type ?? "?"}`);
    }
  }

  return c(ast);
}

/* ---------------------------
 * Runner
 * --------------------------- */
const __GarrigaleProgramCache = new Map();
const __GarrigaleFnIds = new WeakMap();
let __GarrigaleFnSeq = 0;
const __GarrigaleObjIds = new WeakMap();
let __GarrigaleObjSeq = 0;

function getFnCacheId(fn) {
  if (!fn || typeof fn !== "function") return "nofn";
  if (__GarrigaleFnIds.has(fn)) return __GarrigaleFnIds.get(fn);
  __GarrigaleFnSeq += 1;
  const id = `fn${__GarrigaleFnSeq}`;
  __GarrigaleFnIds.set(fn, id);
  return id;
}

function getObjCacheId(obj) {
  if (!obj || typeof obj !== "object") return "noobj";
  if (__GarrigaleObjIds.has(obj)) return __GarrigaleObjIds.get(obj);
  __GarrigaleObjSeq += 1;
  const id = `obj${__GarrigaleObjSeq}`;
  __GarrigaleObjIds.set(obj, id);
  return id;
}

function normalizeRunOptions(compileOptions = {}) {
  if (!compileOptions || typeof compileOptions !== "object") {
    return { cache: true, compileOnly: {} };
  }
  const { cache = true, ...compileOnly } = compileOptions;
  return { cache: !!cache, compileOnly };
}

function makeProgramCacheKey(source, compileOnly) {
  const strictVars = !!compileOnly.strictVars;
  const strictArity = !!compileOnly.strictArity;
  const maxOutput = compileOnly.maxOutput === undefined ? Infinity : compileOnly.maxOutput;
  const maxOps = compileOnly.maxOps === undefined ? 1_000_000 : Number(compileOnly.maxOps);
  const maxRuntimeMs = compileOnly.maxRuntimeMs === undefined ? 2_000 : Number(compileOnly.maxRuntimeMs);
  const maxLoopIters = compileOnly.maxLoopIters === undefined ? 100_000 : Number(compileOnly.maxLoopIters);
  const maxLoopStallIters = compileOnly.maxLoopStallIters === undefined ? 2_048 : Number(compileOnly.maxLoopStallIters);
  const maxCallDepth = compileOnly.maxCallDepth === undefined ? 256 : Number(compileOnly.maxCallDepth);
  const maxCalls = compileOnly.maxCalls === undefined ? 200_000 : Number(compileOnly.maxCalls);
  const maxInferenceSteps = compileOnly.maxInferenceSteps === undefined ? 4_096 : Number(compileOnly.maxInferenceSteps);
  const maxConstraintNodes = compileOnly.maxConstraintNodes === undefined ? 50_000 : Number(compileOnly.maxConstraintNodes);
  const loggerId = getFnCacheId(compileOnly.loggerFn);
  const externalsId = getObjCacheId(compileOnly.externals);
  return `${strictVars}|${strictArity}|${String(maxOutput)}|${String(maxOps)}|${String(maxRuntimeMs)}|${String(maxLoopIters)}|${String(maxLoopStallIters)}|${String(maxCallDepth)}|${String(maxCalls)}|${String(maxInferenceSteps)}|${String(maxConstraintNodes)}|${loggerId}|${externalsId}|${source}`;
}

function createRuntimeContext(initialVars = {}) {
  return {
    vars: { ...initialVars },
    output: [],
    __GarrigaleGuard: {
      startedAtMs: Date.now(),
      ops: 0,
      loops: 0,
      calls: 0,
      callDepth: 0,
    },
  };
}

function createGarrigaleRunner(source, compileOptions = {}) {
  const ast = parse(source);
  const program = compile(ast, compileOptions);
  return (initialVars = {}) => {
    const ctx = createRuntimeContext(initialVars);
    const value = program(ctx);
    return { value, vars: ctx.vars, output: ctx.output, ast };
  };
}

function clearGarrigaleCache() {
  __GarrigaleProgramCache.clear();
}

function runGarrigale(source, initialVars = {}, compileOptions = {}) {
  const { cache, compileOnly } = normalizeRunOptions(compileOptions);
  let ast;
  let program;

  if (cache) {
    const cacheKey = makeProgramCacheKey(source, compileOnly);
    const hit = __GarrigaleProgramCache.get(cacheKey);
    if (hit) {
      ast = hit.ast;
      program = hit.program;
    } else {
      ast = parse(source);
      program = compile(ast, compileOnly);
      __GarrigaleProgramCache.set(cacheKey, { ast, program });
    }
  } else {
    ast = parse(source);
    program = compile(ast, compileOnly);
  }

  const ctx = createRuntimeContext(initialVars);
  const value = program(ctx);
  return { value, vars: ctx.vars, output: ctx.output, ast };
}

/* ---------------------------
 * Example
 * --------------------------- */
const sample = `
mainner(
  // --- basic arithmetic ---
  assigner(a,returner(10)),
  assigner(b,returner(20)),
  assigner(c,returner(30)),
  assigner(d,returner(40)),
  logger(adder(a,b,c,d)),
  logger(adder(d,inverter(adder(a,b,c)))),
  logger(subtractor(d,adder(a,b,c))),
  assigner(e,subtractor(d,adder(a,b,c))),

  // switcher demo
  switcher(e,
    ((40,30,20,10,0),(logger(("40 or 30 or 20 or 10 or 0")))),
    ((-50,-40,-30,-20,-10),(logger(("-50 or -40 or -30 or -20 or -10")))),
    (logger(("idk")))
  ),

  // ifer / equaler demo
  logger(ifer(equaler(e,(-20)),("e is -20"),("e is not -20"))),

  // while demo (guarded)
  assigner(a,(0)),
  whiler(
    (lesser(a,(10))),
    (incrementer(a))
  ),
  logger(a),

  // forrer demo
  forrer(i,(0),(5),(logger(i))),

  // tuple helpers demo
  assigner(t,(1,2,3,4)),
  logger(lengther(t)),
  logger(indexer(t,(2))),
  logger(joiner(t,(","))),
  logger(concatener(("hello "),("Garrigale"))),

  // object demo
  assigner(o, objecter( ("x",1), ("y",2) )),
  logger(geter(o,("x"),("none"))),
  logger(seter(o,("z"),(99))),
  logger(geter(o,("z"),("none"))),

  // function-value demo
  assigner(add2, funner((x,y), adder(x,y))),
  logger(applyer(add2, (7), (8))),

  // --- Matrix demo (NEW) ---
  assigner(A, matrixer( (1,2,3), (4,5,6) )),     // 2x3
  assigner(B, matrixer( (7,8), (9,10), (11,12) )), // 3x2
  logger(shaper(A)),           // (2,3)
  logger(shaper(B)),           // (3,2)
  assigner(C, matMultiplier(A,B)), // 2x2
  logger(C),
  logger(transposer(C)),
  logger(matAdder(C, eyer((2)))),  // C + I
  logger(detter(matAdder(eyer((2)), C))), // det(I + C)

  // solve Ax=b (2x2)
  assigner(M, matrixer( (2,1), (5,3) )),
  assigner(bv, (1,2)),
  logger(solver(M, bv)), // vector solution

  // matVecMultiplier
  logger(matVecMultiplier(M, (1,1))),

  // --- Extended builtins demo ---
  logger(maxer((3),(9),(4),(7))),
  logger(miner((3),(9),(4),(7))),
  logger(clampper((-5),(0),(10))),
  logger(sorter((3,1,4,2),("desc"))),
  logger(flatter((1,(2,(3,4))), (2))),
  logger(upperer(("Garrigale"))),
  assigner(p3, partialer(add2,(3))),
  logger(applyer(p3,(10))),
  logger(entrieser(o)),
  logger(matPowerer(eyer((3)),(5))),
  logger(tracer(matrixer((1,0,0),(0,2,0),(0,0,3)))),

  // --- OOP demo ---
  assigner(Vector2,
    classer(
      ("Vector2"),
      objecter(
        ("init", funner((self,x,y),
          mainner(
            setFielder(self,("x"),x),
            setFielder(self,("y"),y),
            self
          )
        )),
        ("move", funner((self,dx,dy),
          mainner(
            setFielder(self,("x"), adder(getFielder(self,("x"),(0)), dx)),
            setFielder(self,("y"), adder(getFielder(self,("y"),(0)), dy)),
            self
          )
        )),
        ("len2", funner((self),
          adder(
            powerer(getFielder(self,("x"),(0)),(2)),
            powerer(getFielder(self,("y"),(0)),(2))
          )
        ))
      ),
      undefined,
      objecter(
        ("fromScalar", funner((cls,v), newer(cls,v,v)))
      )
    )
  ),
  assigner(ColoredVector2,
    subclasser(
      Vector2,
      ("ColoredVector2"),
      objecter(
        ("init", funner((self,x,y,color),
          mainner(
            superCaller(self,("init"),x,y),
            setFielder(self,("color"), defaultor(color,("white"))),
            self
          )
        )),
        ("paint", funner((self,color), setFielder(self,("color"),color)))
      ),
      objecter(
        ("kind", funner((cls), classNameer(cls)))
      )
    )
  ),
  assigner(v2, caller(Vector2,("fromScalar"),(3))),
  logger(classNameer(v2)),
  logger(caller(v2,("len2"))),
  logger(caller(v2,("move"),(1),(2))),
  assigner(cv2, newer(ColoredVector2,(2),(4),("red"))),
  logger(instanceOfer(cv2, Vector2)),
  logger(methodNameser(cv2,true)),
  logger(getFielder(cv2,("color"),("none"))),
  logger(caller(cv2,("paint"),("blue"))),
  logger(getFielder(cv2,("color"),("none"))),
  logger(caller(ColoredVector2,("kind"))),
  logger(staticSeter(ColoredVector2,("version"),("1.0"))),
  logger(staticGeter(ColoredVector2,("version"),("0.0"))),

  // --- Newly extended helpers demo ---
  logger(meaner((1,2,3,4,5))),
  logger(stddever((1,2,3,4,5),true)),
  logger(chunker((1,2,3,4,5), (2))),
  logger(zipper((1,2,3),("a","b","c"))),
  assigner(users, (
    objecter(("name","alice"),("team","A")),
    objecter(("name","bob"),("team","B")),
    objecter(("name","amy"),("team","A"))
  )),
  logger(groupByer(users,("team"))),
  assigner(cfg, objecter(("app", objecter(("name","Garrigale"))))),
  logger(pathGeter(cfg,("app.name"),("none"))),
  logger(pathSeter(cfg,("app.version"),("2.0"))),
  logger(pathGeter(cfg,("app.version"),("none"))),
  logger(regexTester(("abc-123"),("^[a-z]+-[0-9]+$"),(""))),
  logger(regexAller(("a1b22c333"),("[0-9]+"),("g"))),
  logger(base64Decoder(base64Encoder(("hello Garrigale")))),
  logger(ranker(matrixer((1,2,3),(2,4,6),(1,1,1)))),
  logger(outerProducter((1,2),(3,4,5))),
  logger(diagVectorer(matrixer((9,1,2),(3,8,4),(5,6,7)))),
  logger(kroneckerer(matrixer((1,2),(3,4)), matrixer((0,5),(6,7))))
)
`;

if (typeof module !== "undefined" && require.main === module) {
  const result = runGarrigale(sample, {}, { strictArity: true });
  console.log("---- final ----");
  console.log("value:", result.value);
  console.log("vars:", result.vars);
  console.log("output:", result.output);
}

// Export for browser/other modules
if (typeof module !== "undefined") {
  module.exports = {
    tokenize,
    parse,
    compile,
    runGarrigale,
    createGarrigaleRunner,
    clearGarrigaleCache,
  };
}


