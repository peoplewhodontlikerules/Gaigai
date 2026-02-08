// gaigai.js
// Gaigai: tokenizer -> parser(AST) -> closure-compiler -> run
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
    strictArity = false, // 互換不要なのでON推奨
  } = options;
  const hasOwn = Object.prototype.hasOwnProperty;

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
   * - Gaigai の tuple (= JS Array) をそのまま使う
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
  const GAIGAI_CLASS_KIND = "gaigai.class";
  const GAIGAI_INSTANCE_KIND = "gaigai.instance";

  const isPlainObject = (v) => isObj(v) && !isArr(v);
  const isGaigaiClass = (v) => isPlainObject(v) && v.__gaigaiKind === GAIGAI_CLASS_KIND;
  const isGaigaiInstance = (v) => isPlainObject(v) && v.__gaigaiKind === GAIGAI_INSTANCE_KIND;
  const asGaigaiClass = (v, name = "class") => {
    if (!isGaigaiClass(v)) throw new Error(`${name}: expected class object`);
    return v;
  };
  const asGaigaiInstance = (v, name = "instance") => {
    if (!isGaigaiInstance(v)) throw new Error(`${name}: expected instance object`);
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
    const base = baseValue === undefined || baseValue === null ? null : asGaigaiClass(baseValue, "classer(base)");
    const methods = normalizeCallableMap(methodsValue, "classer(methods)");
    const staticMethods = normalizeCallableMap(staticMethodsValue, "classer(statics)");
    return {
      __gaigaiKind: GAIGAI_CLASS_KIND,
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
    __gaigaiKind: GAIGAI_INSTANCE_KIND,
    cls,
    fields: Object.create(null),
  });

  const invokeClassMethod = (inst, methodName, args, superOnly = false) => {
    const self = asGaigaiInstance(inst, "caller(target)");
    const mName = String(methodName);
    const startClass = superOnly ? self.cls.base : self.cls;
    if (!startClass) throw new Error(`superCaller: no base class for '${self.cls.name}'`);
    const found = findClassMethod(startClass, mName);
    if (!found) throw new Error(`caller: method '${mName}' not found on class '${self.cls.name}'`);
    return found.fn(self, ...args);
  };

  const invokeStaticMethod = (cls, methodName, args) => {
    const ccls = asGaigaiClass(cls, "caller(target)");
    const mName = String(methodName);
    const found = findClassStaticMethod(ccls, mName);
    if (!found) throw new Error(`caller: static method '${mName}' not found on class '${ccls.name}'`);
    return found.fn(ccls, ...args);
  };

  const isInstanceOfClass = (inst, cls) => {
    if (!isGaigaiInstance(inst) || !isGaigaiClass(cls)) return false;
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
        const evalArgs = (ctx) => argFns.map((fn) => fn(ctx));

        /* ---------- core builtins ---------- */

        if (name === "mainner") {
          return (ctx) => {
            let v = undefined;
            for (const fn of argFns) v = fn(ctx);
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
            while (condFn(ctx)) bodyFn(ctx);
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
            const n = Math.max(0, toInt(nFn(ctx)));
            let out = undefined;
            for (let i = 0; i < n; i++) {
              const bodyVal = bodyExprFn(ctx);
              if (isFn(bodyVal)) out = bodyVal(i);
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
            const start = toInt(sFn(ctx));
            const end = toInt(eFn(ctx));
            let step = stFn ? toInt(stFn(ctx)) : start <= end ? 1 : -1;
            if (step === 0) throw new Error("rangeer: step must not be 0");
            const forward = step > 0;
            const out = [];
            for (let k = start; forward ? k < end : k > end; k += step) out.push(k);
            return out;
          };
        }

        if (name === "mapper") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = aFn(ctx);
            const fn = fFn(ctx);
            if (!Array.isArray(a)) throw new Error("mapper: first arg must be an array");
            if (typeof fn !== "function") throw new Error("mapper: second arg must be a function");
            return a.map((x, idx) => fn(x, idx, a));
          };
        }

        if (name === "filterer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = aFn(ctx);
            const fn = fFn(ctx);
            if (!Array.isArray(a)) throw new Error("filterer: first arg must be an array");
            if (typeof fn !== "function") throw new Error("filterer: second arg must be a function");
            return a.filter((x, idx) => !!fn(x, idx, a));
          };
        }

        if (name === "reducer") {
          arity(name, node.args.length, 3, 3);
          const aFn = argFns[0];
          const fFn = argFns[1];
          const initFn = argFns[2];
          return (ctx) => {
            const a = aFn(ctx);
            const fn = fFn(ctx);
            if (!Array.isArray(a)) throw new Error("reducer: first arg must be an array");
            if (typeof fn !== "function") throw new Error("reducer: second arg must be a function");
            let acc = initFn(ctx);
            for (let i = 0; i < a.length; i++) acc = fn(acc, a[i], i, a);
            return acc;
          };
        }

        if (name === "everyer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "everyer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("everyer: second arg must be a function");
            for (let i = 0; i < a.length; i++) if (!fn(a[i], i, a)) return false;
            return true;
          };
        }

        if (name === "somer") {
          arity(name, node.args.length, 2, 2);
          const aFn = argFns[0];
          const fFn = argFns[1];
          return (ctx) => {
            const a = asArray(aFn(ctx), "somer");
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("somer: second arg must be a function");
            for (let i = 0; i < a.length; i++) if (fn(a[i], i, a)) return true;
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
            const cls = asGaigaiClass(clsFn(ctx), "newer(first arg)");
            const inst = makeInstance(cls);
            const initMethod = findClassMethod(cls, "init");
            if (!initMethod) return inst;
            const args = restFns.map((fn) => fn(ctx));
            const ret = initMethod.fn(inst, ...args);
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
            const target = targetFn(ctx);
            const mName = String(nameFn(ctx));
            const args = restFns.map((fn) => fn(ctx));
            if (isGaigaiInstance(target)) return invokeClassMethod(target, mName, args, false);
            if (isGaigaiClass(target)) return invokeStaticMethod(target, mName, args);
            if (isPlainObject(target) && isFn(target[mName])) return target[mName](...args);
            throw new Error("caller: target must be Gaigai instance/class or object with callable property");
          };
        }

        if (name === "superCaller") {
          // superCaller(instance, methodName, ...args)
          arity(name, node.args.length, 2, Infinity);
          const instFn = argFns[0];
          const nameFn = argFns[1];
          const restFns = argFns.slice(2);
          return (ctx) => {
            const inst = asGaigaiInstance(instFn(ctx), "superCaller(first arg)");
            const mName = String(nameFn(ctx));
            const args = restFns.map((fn) => fn(ctx));
            return invokeClassMethod(inst, mName, args, true);
          };
        }

        if (name === "methoder") {
          // methoder(classObj, methodName, fn) -> fn
          arity(name, node.args.length, 3, 3);
          const clsFn = argFns[0], nameFn = argFns[1], fnFn = argFns[2];
          return (ctx) => {
            const cls = asGaigaiClass(clsFn(ctx), "methoder(first arg)");
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
            const cls = asGaigaiClass(clsFn(ctx), "staticMethoder(first arg)");
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
            const inst = asGaigaiInstance(instFn(ctx), "getFielder(first arg)");
            const key = String(keyFn(ctx));
            return hasOwn.call(inst.fields, key) ? inst.fields[key] : fbFn(ctx);
          };
        }

        if (name === "setFielder") {
          arity(name, node.args.length, 3, 3);
          const instFn = argFns[0], keyFn = argFns[1], valFn = argFns[2];
          return (ctx) => {
            const inst = asGaigaiInstance(instFn(ctx), "setFielder(first arg)");
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
            const inst = asGaigaiInstance(instFn(ctx), "hasFielder(first arg)");
            return hasOwn.call(inst.fields, String(keyFn(ctx)));
          };
        }

        if (name === "delFielder") {
          arity(name, node.args.length, 2, 2);
          const instFn = argFns[0], keyFn = argFns[1];
          return (ctx) => {
            const inst = asGaigaiInstance(instFn(ctx), "delFielder(first arg)");
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
            const cls = asGaigaiClass(clsFn(ctx), "staticGeter(first arg)");
            const key = String(keyFn(ctx));
            const found = findClassStaticField(cls, key);
            return found ? found.value : fbFn(ctx);
          };
        }

        if (name === "staticSeter") {
          arity(name, node.args.length, 3, 3);
          const clsFn = argFns[0], keyFn = argFns[1], valFn = argFns[2];
          return (ctx) => {
            const cls = asGaigaiClass(clsFn(ctx), "staticSeter(first arg)");
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
            if (isGaigaiClass(x)) return x.name;
            if (isGaigaiInstance(x)) return x.cls.name;
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
            const cls = isGaigaiClass(target) ? target : isGaigaiInstance(target) ? target.cls : null;
            if (!cls) return [];
            return collectMethodNames(cls, includeInherited);
          };
        }

        if (name === "isClasser") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => isGaigaiClass(xFn(ctx));
        }

        if (name === "isInstanceer") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => isGaigaiInstance(xFn(ctx));
        }

        if (name === "classBaseer") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const x = xFn(ctx);
            if (isGaigaiClass(x)) return x.base;
            if (isGaigaiInstance(x)) return x.cls.base;
            return null;
          };
        }

        if (name === "classChainser") {
          arity(name, node.args.length, 1, 1);
          const xFn = argFns[0];
          return (ctx) => {
            const x = xFn(ctx);
            let cur = isGaigaiClass(x) ? x : isGaigaiInstance(x) ? x.cls : null;
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
            const inst = asGaigaiInstance(iFn(ctx), "fieldKeyser(first arg)");
            return Object.keys(inst.fields);
          };
        }

        if (name === "staticKeyser") {
          arity(name, node.args.length, 1, 1);
          const cFn = argFns[0];
          return (ctx) => {
            const cls = asGaigaiClass(cFn(ctx), "staticKeyser(first arg)");
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
            const inst = asGaigaiInstance(iFn(ctx), "bindMethoder(first arg)");
            const mName = String(mFn(ctx));
            return (...args) => invokeClassMethod(inst, mName, args, false);
          };
        }

        if (name === "constructer") {
          arity(name, node.args.length, 1, Infinity);
          const clsFn = argFns[0];
          const restFns = argFns.slice(1);
          return (ctx) => {
            const cls = asGaigaiClass(clsFn(ctx), "constructer(first arg)");
            const inst = makeInstance(cls);
            const initMethod = findClassMethod(cls, "init");
            if (!initMethod) return inst;
            const args = restFns.map((fn) => fn(ctx));
            const ret = initMethod.fn(inst, ...args);
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
              }
            };
          };
        }

        if (name === "applyer") {
          arity(name, node.args.length, 1, Infinity);
          const fFn = argFns[0];
          const restFns = argFns.slice(1);
          return (ctx) => {
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("applyer: first arg must be a function");
            const args = restFns.map((g) => g(ctx));
            return fn(...args);
          };
        }

        if (name === "partialer") {
          arity(name, node.args.length, 1, Infinity);
          const fFn = argFns[0];
          const boundFns = argFns.slice(1);
          return (ctx) => {
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("partialer: first arg must be a function");
            const bound = boundFns.map((g) => g(ctx));
            return (...rest) => fn(...bound, ...rest);
          };
        }

        if (name === "pipeer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            const fns = argFns.map((g) => g(ctx));
            for (let i = 0; i < fns.length; i++) if (!isFn(fns[i])) throw new Error("pipeer: all args must be functions");
            return (...args) => {
              let v = fns[0](...args);
              for (let i = 1; i < fns.length; i++) v = fns[i](v);
              return v;
            };
          };
        }

        if (name === "composeer") {
          arity(name, node.args.length, 1, Infinity);
          return (ctx) => {
            const fns = argFns.map((g) => g(ctx));
            for (let i = 0; i < fns.length; i++) if (!isFn(fns[i])) throw new Error("composeer: all args must be functions");
            return (...args) => {
              let v = fns[fns.length - 1](...args);
              for (let i = fns.length - 2; i >= 0; i--) v = fns[i](v);
              return v;
            };
          };
        }

        if (name === "memoizer") {
          arity(name, node.args.length, 1, 2);
          const fFn = argFns[0];
          const keyFnExpr = argFns[1] || null;
          return (ctx) => {
            const fn = fFn(ctx);
            const keyFn = keyFnExpr ? keyFnExpr(ctx) : null;
            if (!isFn(fn)) throw new Error("memoizer: first arg must be a function");
            if (keyFn !== null && !isFn(keyFn)) throw new Error("memoizer: second arg must be a function");
            const cache = new Map();
            return (...args) => {
              let key;
              if (keyFn) key = keyFn(...args);
              else {
                try {
                  key = JSON.stringify(args);
                } catch (_) {
                  key = args.map((a) => `${typeof a}:${String(a)}`).join("|");
                }
              }
              if (cache.has(key)) return cache.get(key);
              const v = fn(...args);
              cache.set(key, v);
              return v;
            };
          };
        }

        if (name === "onceer") {
          arity(name, node.args.length, 1, 1);
          const fFn = argFns[0];
          return (ctx) => {
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("onceer: first arg must be a function");
            let called = false;
            let value = undefined;
            return (...args) => {
              if (!called) {
                called = true;
                value = fn(...args);
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
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("curryer: first arg must be a function");
            const expected = nFn ? Math.max(0, toInt(nFn(ctx))) : Math.max(0, fn.length);
            const curry = (collected) => (...args) => {
              const next = collected.concat(args);
              if (next.length >= expected) return fn(...next);
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
            const fn = fFn(ctx);
            const args = asArray(aFn(ctx), "spreadApplyer(second arg)");
            if (!isFn(fn)) throw new Error("spreadApplyer: first arg must be a function");
            return fn(...args);
          };
        }

        if (name === "tapper") {
          arity(name, node.args.length, 2, 2);
          const xFn = argFns[0], fFn = argFns[1];
          return (ctx) => {
            const x = xFn(ctx);
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("tapper: second arg must be a function");
            fn(x);
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
            const fn = fFn(ctx);
            if (!isFn(fn)) throw new Error("predicateNoter: first arg must be a function");
            return (...args) => !fn(...args);
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
        return () => {
          throw new Error(`Unknown function '${name}'`);
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
const __gaigaiProgramCache = new Map();
const __gaigaiFnIds = new WeakMap();
let __gaigaiFnSeq = 0;

function getFnCacheId(fn) {
  if (!fn || typeof fn !== "function") return "nofn";
  if (__gaigaiFnIds.has(fn)) return __gaigaiFnIds.get(fn);
  __gaigaiFnSeq += 1;
  const id = `fn${__gaigaiFnSeq}`;
  __gaigaiFnIds.set(fn, id);
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
  const loggerId = getFnCacheId(compileOnly.loggerFn);
  return `${strictVars}|${strictArity}|${String(maxOutput)}|${loggerId}|${source}`;
}

function createGaigaiRunner(source, compileOptions = {}) {
  const ast = parse(source);
  const program = compile(ast, compileOptions);
  return (initialVars = {}) => {
    const ctx = { vars: { ...initialVars }, output: [] };
    const value = program(ctx);
    return { value, vars: ctx.vars, output: ctx.output, ast };
  };
}

function clearGaigaiCache() {
  __gaigaiProgramCache.clear();
}

function runGaigai(source, initialVars = {}, compileOptions = {}) {
  const { cache, compileOnly } = normalizeRunOptions(compileOptions);
  let ast;
  let program;

  if (cache) {
    const cacheKey = makeProgramCacheKey(source, compileOnly);
    const hit = __gaigaiProgramCache.get(cacheKey);
    if (hit) {
      ast = hit.ast;
      program = hit.program;
    } else {
      ast = parse(source);
      program = compile(ast, compileOnly);
      __gaigaiProgramCache.set(cacheKey, { ast, program });
    }
  } else {
    ast = parse(source);
    program = compile(ast, compileOnly);
  }

  const ctx = { vars: { ...initialVars }, output: [] };
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

  // while demo (ガードなし)
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
  logger(concatener(("hello "),("gaigai"))),

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
  logger(upperer(("gaigai"))),
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
  assigner(cfg, objecter(("app", objecter(("name","gaigai"))))),
  logger(pathGeter(cfg,("app.name"),("none"))),
  logger(pathSeter(cfg,("app.version"),("2.0"))),
  logger(pathGeter(cfg,("app.version"),("none"))),
  logger(regexTester(("abc-123"),("^[a-z]+-[0-9]+$"),(""))),
  logger(regexAller(("a1b22c333"),("[0-9]+"),("g"))),
  logger(base64Decoder(base64Encoder(("hello gaigai")))),
  logger(ranker(matrixer((1,2,3),(2,4,6),(1,1,1)))),
  logger(outerProducter((1,2),(3,4,5))),
  logger(diagVectorer(matrixer((9,1,2),(3,8,4),(5,6,7)))),
  logger(kroneckerer(matrixer((1,2),(3,4)), matrixer((0,5),(6,7))))
)
`;

if (typeof module !== "undefined" && require.main === module) {
  const result = runGaigai(sample, {}, { strictArity: true });
  console.log("---- final ----");
  console.log("value:", result.value);
  console.log("vars:", result.vars);
  console.log("output:", result.output);
}

// Export for browser/other modules
if (typeof module !== "undefined") {
  module.exports = { tokenize, parse, compile, runGaigai, createGaigaiRunner, clearGaigaiCache };
}
