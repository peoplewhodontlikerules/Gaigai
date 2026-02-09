"use strict";

function buildLineMap(src) {
  const starts = [0];
  for (let i = 0; i < src.length; i++) if (src[i] === "\n") starts.push(i + 1);
  return starts;
}

function posToLineCol(starts, pos) {
  let lo = 0;
  let hi = starts.length - 1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (starts[mid] <= pos) lo = mid + 1;
    else hi = mid - 1;
  }
  const line = Math.max(0, hi);
  return { line: line + 1, col: pos - starts[line] + 1 };
}

function formatAround(src, pos, radius = 42) {
  const a = Math.max(0, pos - radius);
  const b = Math.min(src.length, pos + radius);
  const frag = src.slice(a, b).replace(/\t/g, "  ");
  return `${frag}\n${" ".repeat(Math.max(0, pos - a))}^`;
}

function makeSyntaxError(src, starts, pos, msg) {
  const lc = posToLineCol(starts, Math.max(0, pos));
  return new SyntaxError(`${msg} at ${Math.max(0, pos)} (line ${lc.line}, col ${lc.col})\n${formatAround(src, pos)}`);
}

const T_ANY = { kind: "primitive", name: "any" };
const T_NUMBER = { kind: "primitive", name: "number" };
const T_STRING = { kind: "primitive", name: "string" };
const T_BOOL = { kind: "primitive", name: "bool" };
const T_NULL = { kind: "primitive", name: "null" };
const T_VOID = { kind: "primitive", name: "void" };

const DEFAULT_GUARDS = Object.freeze({
  maxOps: 1_000_000,
  maxRuntimeMs: 2_000,
  maxLoopIters: 100_000,
  maxLoopStallIters: 2_048,
  maxCallDepth: 256,
  maxCalls: 200_000,
  maxInferenceSteps: 4_096,
  maxConstraintNodes: 50_000
});

function tList(inner) {
  return { kind: "list", inner };
}

function tFn(params, ret) {
  return { kind: "fn", params, ret };
}

function normalizeType(type) {
  if (!type || typeof type !== "object") return T_ANY;
  if (type.kind === "primitive") return type;
  if (type.kind === "list") return tList(normalizeType(type.inner));
  if (type.kind === "fn") return tFn(type.params.map(normalizeType), normalizeType(type.ret));
  return T_ANY;
}

function typeKey(type) {
  const t = normalizeType(type);
  if (t.kind === "primitive") return t.name;
  if (t.kind === "list") return `list<${typeKey(t.inner)}>`;
  if (t.kind === "fn") return `fn(${t.params.map(typeKey).join(",")}):${typeKey(t.ret)}`;
  return "any";
}

function typeStr(type) {
  const t = normalizeType(type);
  if (t.kind === "primitive") return t.name;
  if (t.kind === "list") return `list<${typeStr(t.inner)}>`;
  if (t.kind === "fn") return `fn(${t.params.map(typeStr).join(", ")}): ${typeStr(t.ret)}`;
  return "any";
}

function isSameType(a, b) {
  return typeKey(a) === typeKey(b);
}

function isAnyType(type) {
  const t = normalizeType(type);
  return t.kind === "primitive" && t.name === "any";
}

function isAssignable(from, to) {
  const a = normalizeType(from);
  const b = normalizeType(to);
  if (a.kind === "primitive" && a.name === "any") return true;
  if (b.kind === "primitive" && b.name === "any") return true;
  if (a.kind === "primitive" && b.kind === "primitive") return a.name === b.name;
  if (a.kind === "list" && b.kind === "list") return isAssignable(a.inner, b.inner);
  if (a.kind === "fn" && b.kind === "fn") {
    if (a.params.length !== b.params.length) return false;
    for (let i = 0; i < a.params.length; i++) if (!isAssignable(a.params[i], b.params[i])) return false;
    return isAssignable(a.ret, b.ret);
  }
  return false;
}

function inferTypeFromValue(v) {
  if (v === undefined) return T_VOID;
  if (v === null) return T_NULL;
  if (typeof v === "number" && Number.isFinite(v)) return T_NUMBER;
  if (typeof v === "string") return T_STRING;
  if (typeof v === "boolean") return T_BOOL;
  if (Array.isArray(v)) {
    if (v.length === 0) return tList(T_ANY);
    let inner = inferTypeFromValue(v[0]);
    for (let i = 1; i < v.length; i++) {
      const it = inferTypeFromValue(v[i]);
      if (!isSameType(inner, it)) {
        inner = T_ANY;
        break;
      }
    }
    return tList(inner);
  }
  if (isRaggiraleFn(v) || typeof v === "function") return tFn([], T_ANY);
  return T_ANY;
}

function matchesType(v, type) {
  const t = normalizeType(type);
  if (t.kind === "primitive") {
    if (t.name === "any") return true;
    if (t.name === "number") return typeof v === "number" && Number.isFinite(v);
    if (t.name === "string") return typeof v === "string";
    if (t.name === "bool") return typeof v === "boolean";
    if (t.name === "null") return v === null;
    if (t.name === "void") return v === undefined;
    return true;
  }
  if (t.kind === "list") return Array.isArray(v) && v.every((x) => matchesType(x, t.inner));
  if (t.kind === "fn") return isRaggiraleFn(v) || typeof v === "function";
  return true;
}

function assertType(v, type, label) {
  if (!matchesType(v, type)) {
    throw new Error(`${label}: expected ${typeStr(type)}, got ${typeStr(inferTypeFromValue(v))}`);
  }
}

const KEYWORDS = new Set([
  "let", "const", "fn", "if", "else", "while", "for", "in", "return",
  "true", "false", "null", "number", "string", "bool", "any", "void", "list"
]);
const MULTI_OPS = ["|>", "=>", "..", "==", "!=", "<=", ">=", "&&", "||", "+=", "-=", "*=", "/="];
const PUNC = new Set(["(", ")", "{", "}", "[", "]", ",", ";", ":"]);
const OPS = new Set(["+", "-", "*", "/", "%", "=", "!", "<", ">"]);

function tokenize(src) {
  const starts = buildLineMap(src);
  const tokens = [];
  let i = 0;

  const push = (type, value, pos) => {
    const lc = posToLineCol(starts, pos);
    tokens.push({ type, value, pos, line: lc.line, col: lc.col });
  };
  const err = (msg, pos = i) => {
    throw makeSyntaxError(src, starts, pos, msg);
  };

  const isSpace = (c) => /\s/.test(c);
  const isDigit = (c) => /[0-9]/.test(c);
  const isIdStart = (c) => /[A-Za-z_]/.test(c);
  const isId = (c) => /[A-Za-z0-9_]/.test(c);

  while (i < src.length) {
    const c = src[i];
    if (isSpace(c)) {
      i++;
      continue;
    }

    if (c === "/" && src[i + 1] === "/") {
      i += 2;
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }
    if (c === "/" && src[i + 1] === "*") {
      const start = i;
      const end = src.indexOf("*/", i + 2);
      if (end === -1) err("Unterminated block comment", start);
      i = end + 2;
      continue;
    }

    let matched = false;
    for (const op of MULTI_OPS) {
      if (src.startsWith(op, i)) {
        push("op", op, i);
        i += op.length;
        matched = true;
        break;
      }
    }
    if (matched) continue;

    if (PUNC.has(c)) {
      push("punc", c, i);
      i++;
      continue;
    }

    if (OPS.has(c)) {
      push("op", c, i);
      i++;
      continue;
    }

    if (c === '"' || c === "'") {
      const quote = c;
      const start = i;
      i++;
      let s = "";
      while (i < src.length) {
        const ch = src[i];
        if (ch === "\\") {
          const n = src[i + 1];
          if (n === quote || n === "\\" || n === "n" || n === "t" || n === "r") {
            if (n === "n") s += "\n";
            else if (n === "t") s += "\t";
            else if (n === "r") s += "\r";
            else s += n;
            i += 2;
            continue;
          }
        }
        if (ch === quote) {
          i++;
          push("string", s, start);
          s = null;
          break;
        }
        s += ch;
        i++;
      }
      if (s !== null) err("Unterminated string literal", start);
      continue;
    }

    if ((c === "-" && isDigit(src[i + 1] || "")) || isDigit(c)) {
      const start = i;
      if (src[i] === "-") i++;
      let sawDigit = false;
      while (i < src.length && isDigit(src[i])) {
        i++;
        sawDigit = true;
      }
      if (src[i] === "." && src[i + 1] !== ".") {
        i++;
        while (i < src.length && isDigit(src[i])) {
          i++;
          sawDigit = true;
        }
      }
      if (!sawDigit) err("Invalid number literal", start);
      if (src[i] === "e" || src[i] === "E") {
        i++;
        if (src[i] === "+" || src[i] === "-") i++;
        const expStart = i;
        while (i < src.length && isDigit(src[i])) i++;
        if (i === expStart) err("Invalid exponent", start);
      }
      const raw = src.slice(start, i);
      const n = Number(raw);
      if (!Number.isFinite(n)) err(`Invalid number literal '${raw}'`, start);
      push("number", n, start);
      continue;
    }

    if (isIdStart(c)) {
      const start = i;
      i++;
      while (i < src.length && isId(src[i])) i++;
      const word = src.slice(start, i);
      if (KEYWORDS.has(word)) push("keyword", word, start);
      else push("id", word, start);
      continue;
    }

    err(`Unexpected character '${c}'`);
  }

  push("eof", "", i);
  return { tokens, starts, src };
}

class Parser {
  constructor(tokenized) {
    this.tokens = tokenized.tokens;
    this.starts = tokenized.starts;
    this.src = tokenized.src;
    this.pos = 0;
  }

  peek(offset = 0) {
    return this.tokens[this.pos + offset] || this.tokens[this.tokens.length - 1];
  }

  next() {
    return this.tokens[this.pos++];
  }

  fail(tok, msg) {
    const p = tok?.pos ?? this.peek()?.pos ?? -1;
    throw makeSyntaxError(this.src, this.starts, p, `${msg} (token: '${tok?.value ?? tok?.type ?? "?"}')`);
  }

  isKeyword(k, tok = this.peek()) {
    return tok.type === "keyword" && tok.value === k;
  }

  matchKeyword(k) {
    if (this.isKeyword(k)) {
      this.next();
      return true;
    }
    return false;
  }

  expectKeyword(k) {
    const t = this.next();
    if (t.type !== "keyword" || t.value !== k) this.fail(t, `Expected keyword '${k}'`);
    return t;
  }

  isPunc(ch, tok = this.peek()) {
    return tok.type === "punc" && tok.value === ch;
  }

  matchPunc(ch) {
    if (this.isPunc(ch)) {
      this.next();
      return true;
    }
    return false;
  }

  expectPunc(ch) {
    const t = this.next();
    if (t.type !== "punc" || t.value !== ch) this.fail(t, `Expected '${ch}'`);
    return t;
  }

  isOp(op, tok = this.peek()) {
    return tok.type === "op" && tok.value === op;
  }

  matchOp(op) {
    if (this.isOp(op)) {
      this.next();
      return true;
    }
    return false;
  }

  expectOp(op) {
    const t = this.next();
    if (t.type !== "op" || t.value !== op) this.fail(t, `Expected operator '${op}'`);
    return t;
  }

  expectId() {
    const t = this.next();
    if (t.type !== "id") this.fail(t, "Expected identifier");
    return t.value;
  }

  parseProgram() {
    const body = [];
    while (this.peek().type !== "eof") body.push(this.parseStatement());
    return { type: "Program", body };
  }

  parseStatement() {
    if (this.matchKeyword("let")) return this.parseVarDecl(true);
    if (this.matchKeyword("const")) return this.parseVarDecl(false);
    if (this.matchKeyword("fn")) return this.parseFnDecl();
    if (this.matchKeyword("if")) return this.parseIf();
    if (this.matchKeyword("while")) return this.parseWhile();
    if (this.matchKeyword("for")) return this.parseForIn();
    if (this.matchKeyword("return")) return this.parseReturn();
    if (this.isPunc("{")) return this.parseBlock();

    const expr = this.parseExpression();
    this.expectPunc(";");
    return { type: "ExprStmt", expression: expr };
  }

  parseBlock() {
    this.expectPunc("{");
    const body = [];
    while (!this.isPunc("}")) {
      if (this.peek().type === "eof") this.fail(this.peek(), "Unterminated block");
      body.push(this.parseStatement());
    }
    this.expectPunc("}");
    return { type: "BlockStmt", body };
  }

  parseVarDecl(mutable) {
    const name = this.expectId();
    let typeAnn = null;
    if (this.matchPunc(":")) typeAnn = this.parseTypeExpr();
    this.expectOp("=");
    const init = this.parseExpression();
    this.expectPunc(";");
    return { type: "LetDecl", mutable, name, typeAnn, init };
  }

  parseFnDecl() {
    const name = this.expectId();
    this.expectPunc("(");
    const params = [];
    if (!this.isPunc(")")) {
      while (true) {
        const pName = this.expectId();
        this.expectPunc(":");
        const pType = this.parseTypeExpr();
        params.push({ name: pName, typeAnn: pType });
        if (!this.matchPunc(",")) break;
      }
    }
    this.expectPunc(")");
    let ret = T_VOID;
    if (this.matchPunc(":")) ret = this.parseTypeExpr();

    if (this.matchOp("=>")) {
      const bodyExpr = this.parseExpression();
      this.expectPunc(";");
      return { type: "FnDecl", name, params, ret, exprBody: true, body: bodyExpr };
    }

    const body = this.parseBlock();
    return { type: "FnDecl", name, params, ret, exprBody: false, body };
  }

  parseIf() {
    this.expectPunc("(");
    const test = this.parseExpression();
    this.expectPunc(")");
    const consequent = this.parseBlock();
    let alternate = null;
    if (this.matchKeyword("else")) {
      alternate = this.matchKeyword("if") ? this.parseIf() : this.parseBlock();
    }
    return { type: "IfStmt", test, consequent, alternate };
  }

  parseWhile() {
    this.expectPunc("(");
    const test = this.parseExpression();
    this.expectPunc(")");
    const body = this.parseBlock();
    return { type: "WhileStmt", test, body };
  }

  parseForIn() {
    this.expectPunc("(");
    const iterator = this.expectId();
    this.expectKeyword("in");
    const iterable = this.parseExpression();
    this.expectPunc(")");
    const body = this.parseBlock();
    return { type: "ForInStmt", iterator, iterable, body };
  }

  parseReturn() {
    let argument = null;
    if (!this.isPunc(";")) argument = this.parseExpression();
    this.expectPunc(";");
    return { type: "ReturnStmt", argument };
  }

  parseExpression() {
    return this.parseAssignment();
  }

  parseAssignment() {
    const left = this.parsePipe();
    if (this.isOp("=") || this.isOp("+=") || this.isOp("-=") || this.isOp("*=") || this.isOp("/=")) {
      const op = this.next().value;
      if (left.type !== "Identifier") this.fail(this.peek(-1), "Assignment target must be an identifier");
      return { type: "AssignExpr", operator: op, target: left, value: this.parseAssignment() };
    }
    return left;
  }

  parsePipe() {
    let expr = this.parseLogicalOr();
    while (this.matchOp("|>")) expr = { type: "PipeExpr", left: expr, right: this.parseLogicalOr() };
    return expr;
  }

  parseLogicalOr() {
    let expr = this.parseLogicalAnd();
    while (this.matchOp("||")) expr = { type: "LogicalExpr", operator: "||", left: expr, right: this.parseLogicalAnd() };
    return expr;
  }

  parseLogicalAnd() {
    let expr = this.parseEquality();
    while (this.matchOp("&&")) expr = { type: "LogicalExpr", operator: "&&", left: expr, right: this.parseEquality() };
    return expr;
  }

  parseEquality() {
    let expr = this.parseComparison();
    while (this.isOp("==") || this.isOp("!=")) {
      const op = this.next().value;
      expr = { type: "BinaryExpr", operator: op, left: expr, right: this.parseComparison() };
    }
    return expr;
  }

  parseComparison() {
    let expr = this.parseRange();
    while (this.isOp("<") || this.isOp("<=") || this.isOp(">") || this.isOp(">=")) {
      const op = this.next().value;
      expr = { type: "BinaryExpr", operator: op, left: expr, right: this.parseRange() };
    }
    return expr;
  }

  parseRange() {
    let expr = this.parseAdditive();
    while (this.matchOp("..")) expr = { type: "RangeExpr", left: expr, right: this.parseAdditive() };
    return expr;
  }

  parseAdditive() {
    let expr = this.parseMultiplicative();
    while (this.isOp("+") || this.isOp("-")) {
      const op = this.next().value;
      expr = { type: "BinaryExpr", operator: op, left: expr, right: this.parseMultiplicative() };
    }
    return expr;
  }

  parseMultiplicative() {
    let expr = this.parseUnary();
    while (this.isOp("*") || this.isOp("/") || this.isOp("%")) {
      const op = this.next().value;
      expr = { type: "BinaryExpr", operator: op, left: expr, right: this.parseUnary() };
    }
    return expr;
  }

  parseUnary() {
    if (this.matchOp("!")) return { type: "UnaryExpr", operator: "!", argument: this.parseUnary() };
    if (this.matchOp("-")) return { type: "UnaryExpr", operator: "-", argument: this.parseUnary() };
    return this.parsePostfix();
  }

  parsePostfix() {
    let expr = this.parsePrimary();
    while (true) {
      if (this.matchPunc("(")) {
        const args = [];
        if (!this.isPunc(")")) {
          while (true) {
            args.push(this.parseExpression());
            if (!this.matchPunc(",")) break;
          }
        }
        this.expectPunc(")");
        expr = { type: "CallExpr", callee: expr, args };
        continue;
      }

      if (this.matchPunc("[")) {
        const index = this.parseExpression();
        this.expectPunc("]");
        expr = { type: "IndexExpr", object: expr, index };
        continue;
      }

      break;
    }
    return expr;
  }

  parsePrimary() {
    const t = this.peek();

    if (t.type === "number") {
      this.next();
      return { type: "Literal", value: t.value, valueType: T_NUMBER };
    }
    if (t.type === "string") {
      this.next();
      return { type: "Literal", value: t.value, valueType: T_STRING };
    }
    if (t.type === "keyword" && t.value === "true") {
      this.next();
      return { type: "Literal", value: true, valueType: T_BOOL };
    }
    if (t.type === "keyword" && t.value === "false") {
      this.next();
      return { type: "Literal", value: false, valueType: T_BOOL };
    }
    if (t.type === "keyword" && t.value === "null") {
      this.next();
      return { type: "Literal", value: null, valueType: T_NULL };
    }
    if (t.type === "id") {
      this.next();
      return { type: "Identifier", name: t.value };
    }

    if (this.matchPunc("(")) {
      const expr = this.parseExpression();
      this.expectPunc(")");
      return expr;
    }

    if (this.matchPunc("[")) {
      const elements = [];
      if (!this.isPunc("]")) {
        while (true) {
          elements.push(this.parseExpression());
          if (!this.matchPunc(",")) break;
        }
      }
      this.expectPunc("]");
      return { type: "ArrayExpr", elements };
    }

    this.fail(t, "Unexpected token in expression");
  }

  parseTypeExpr() {
    return normalizeType(this.parseTypeAtom());
  }

  parseTypeAtom() {
    if (this.matchPunc("[")) {
      const inner = this.parseTypeExpr();
      this.expectPunc("]");
      return tList(inner);
    }

    const t = this.next();
    if (t.type !== "keyword" && t.type !== "id") this.fail(t, "Expected type annotation");

    if (t.value === "any") return T_ANY;
    if (t.value === "number") return T_NUMBER;
    if (t.value === "string") return T_STRING;
    if (t.value === "bool") return T_BOOL;
    if (t.value === "null") return T_NULL;
    if (t.value === "void") return T_VOID;

    if (t.value === "list") {
      this.expectOp("<");
      const inner = this.parseTypeExpr();
      this.expectOp(">");
      return tList(inner);
    }

    this.fail(t, `Unknown type '${t.value}'`);
  }
}

function parse(source) {
  const p = new Parser(tokenize(source));
  const ast = p.parseProgram();
  if (p.peek().type !== "eof") p.fail(p.peek(), "Unexpected trailing token");
  return ast;
}

function parseTypeString(source) {
  const p = new Parser(tokenize(source));
  const t = p.parseTypeExpr();
  if (p.peek().type !== "eof") p.fail(p.peek(), "Unexpected trailing token in type");
  return t;
}

class TypeScope {
  constructor(parent = null) {
    this.parent = parent;
    this.vars = new Map();
  }

  define(name, type, mutable) {
    if (this.vars.has(name)) throw new Error(`Type error: '${name}' is already defined in this scope`);
    this.vars.set(name, { type: normalizeType(type), mutable: !!mutable });
  }

  get(name) {
    if (this.vars.has(name)) return this.vars.get(name);
    if (this.parent) return this.parent.get(name);
    return null;
  }
}

function isObj(v) {
  return v !== null && typeof v === "object";
}

function ensureList(v, label) {
  if (!Array.isArray(v)) throw new Error(`${label}: expected list`);
  return v;
}

function stableSerialize(v, seen = new WeakSet()) {
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
    if (Array.isArray(v)) return `[${v.map((x) => stableSerialize(x, seen)).join(",")}]`;
    const keys = Object.keys(v).sort();
    const items = [];
    for (let i = 0; i < keys.length; i++) items.push(`${JSON.stringify(keys[i])}:${stableSerialize(v[keys[i]], seen)}`);
    return `{${items.join(",")}}`;
  } finally {
    seen.delete(v);
  }
}

function valueKey(v) {
  return stableSerialize(v);
}

function scopeVisibleSnapshot(scope) {
  const out = Object.create(null);
  let cur = scope;
  while (cur) {
    for (const [name, cell] of cur.vars.entries()) {
      if (cell.builtin) continue;
      if (!Object.prototype.hasOwnProperty.call(out, name)) out[name] = cell.value;
    }
    cur = cur.parent;
  }
  return out;
}

function scopeVisibleSignature(scope) {
  const snap = scopeVisibleSnapshot(scope);
  const keys = Object.keys(snap).sort();
  const parts = new Array(keys.length);
  for (let i = 0; i < keys.length; i++) {
    const k = keys[i];
    parts[i] = `${JSON.stringify(k)}:${valueKey(snap[k])}`;
  }
  return parts.join("|");
}

function isLogicVarTerm(v) {
  if (typeof v === "string") return v.length >= 2 && v[0] === "?";
  if (!isObj(v) || Array.isArray(v)) return false;
  if (Object.prototype.hasOwnProperty.call(v, "var")) return typeof v.var === "string" || typeof v.var === "number";
  if (Object.prototype.hasOwnProperty.call(v, "Var")) return typeof v.Var === "string" || typeof v.Var === "number";
  return false;
}

function logicVarName(v) {
  if (typeof v === "string") return v;
  const raw = Object.prototype.hasOwnProperty.call(v, "var") ? v.var : v.Var;
  const s = String(raw);
  return s.startsWith("?") ? s : `?${s}`;
}

function normalizeLogicAtom(raw) {
  if (isLogicVarTerm(raw)) return logicVarName(raw);
  if (isObj(raw) && !Array.isArray(raw)) {
    if (Object.prototype.hasOwnProperty.call(raw, "atom")) return normalizeLogicAtom(raw.atom);
    if (Object.prototype.hasOwnProperty.call(raw, "goal")) return normalizeLogicAtom(raw.goal);
    if (Object.prototype.hasOwnProperty.call(raw, "pred") || Object.prototype.hasOwnProperty.call(raw, "name")) {
      const pred = Object.prototype.hasOwnProperty.call(raw, "pred") ? raw.pred : raw.name;
      let args = Object.prototype.hasOwnProperty.call(raw, "args") ? raw.args : [];
      if (!Array.isArray(args)) args = [args];
      return [pred, ...args];
    }
  }
  return raw;
}

function normalizeLogicLiteral(raw) {
  if (isObj(raw) && !Array.isArray(raw)) {
    if (Object.prototype.hasOwnProperty.call(raw, "not")) return { negated: true, atom: normalizeLogicAtom(raw.not) };
    if (Object.prototype.hasOwnProperty.call(raw, "neg")) return { negated: true, atom: normalizeLogicAtom(raw.neg) };
    if (Object.prototype.hasOwnProperty.call(raw, "atom")) return { negated: false, atom: normalizeLogicAtom(raw.atom) };
    if (Object.prototype.hasOwnProperty.call(raw, "goal")) return { negated: false, atom: normalizeLogicAtom(raw.goal) };
  }
  if (Array.isArray(raw) && raw.length === 2 && raw[0] === "not") {
    return { negated: true, atom: normalizeLogicAtom(raw[1]) };
  }
  return { negated: false, atom: normalizeLogicAtom(raw) };
}

function normalizeLogicBody(raw, forceList = false) {
  if (raw === undefined || raw === null) return [];
  if (Array.isArray(raw) && !(raw.length === 2 && raw[0] === "not")) {
    if (forceList) return raw.map((x) => normalizeLogicLiteral(x));
    if (raw.length === 0) return [];
    if (Array.isArray(raw[0]) || (isObj(raw[0]) && !isLogicVarTerm(raw[0]))) {
      return raw.map((x) => normalizeLogicLiteral(x));
    }
  }
  return [normalizeLogicLiteral(raw)];
}

function normalizeLogicRule(rule, index) {
  if (Array.isArray(rule)) {
    if (rule.length === 0) throw new Error(`logicInfer: rule[${index}] must not be empty`);
    if (rule.length === 1) return { head: normalizeLogicAtom(rule[0]), body: [] };
    return { head: normalizeLogicAtom(rule[1]), body: normalizeLogicBody(rule[0], false) };
  }
  if (isObj(rule)) {
    const hasHead = Object.prototype.hasOwnProperty.call(rule, "head");
    const hasBody = Object.prototype.hasOwnProperty.call(rule, "body");
    const hasIf = Object.prototype.hasOwnProperty.call(rule, "if")
      || Object.prototype.hasOwnProperty.call(rule, "when")
      || Object.prototype.hasOwnProperty.call(rule, "premises");
    const hasThen = Object.prototype.hasOwnProperty.call(rule, "then")
      || Object.prototype.hasOwnProperty.call(rule, "conclusion");

    if (hasHead) {
      const bodyRaw = hasBody ? rule.body : [];
      return { head: normalizeLogicAtom(rule.head), body: normalizeLogicBody(bodyRaw, false) };
    }
    if (!hasIf || !hasThen) {
      throw new Error(`logicInfer: rule[${index}] object must have head/body or if/then`);
    }
    const ifRaw = Object.prototype.hasOwnProperty.call(rule, "if") ? rule.if : Object.prototype.hasOwnProperty.call(rule, "when") ? rule.when : rule.premises;
    const thenRaw = Object.prototype.hasOwnProperty.call(rule, "then") ? rule.then : rule.conclusion;
    return { head: normalizeLogicAtom(thenRaw), body: normalizeLogicBody(ifRaw, false) };
  }
  throw new Error(`logicInfer: unsupported rule format at index ${index}`);
}

function normalizeLogicQuery(raw) {
  if (isObj(raw) && !Array.isArray(raw)) {
    if (Object.prototype.hasOwnProperty.call(raw, "all")) return normalizeLogicBody(raw.all, true);
    if (Object.prototype.hasOwnProperty.call(raw, "and")) return normalizeLogicBody(raw.and, true);
    if (Object.prototype.hasOwnProperty.call(raw, "goal")) return [normalizeLogicLiteral(raw.goal)];
    if (Object.prototype.hasOwnProperty.call(raw, "atom")) return [normalizeLogicLiteral(raw.atom)];
  }
  if (Array.isArray(raw) && raw.length > 0 && (Array.isArray(raw[0]) || (isObj(raw[0]) && !isLogicVarTerm(raw[0])))) {
    return normalizeLogicBody(raw, true);
  }
  return [normalizeLogicLiteral(raw)];
}

function cloneSubst(subst) {
  const out = Object.create(null);
  for (const k of Object.keys(subst)) out[k] = subst[k];
  return out;
}

function derefLogic(term, subst) {
  let cur = term;
  while (isLogicVarTerm(cur)) {
    const key = logicVarName(cur);
    if (!Object.prototype.hasOwnProperty.call(subst, key)) break;
    const next = subst[key];
    if (Object.is(next, cur)) break;
    cur = next;
  }
  return cur;
}

function occursLogic(varName, term, subst, seen = new WeakSet()) {
  const t = derefLogic(term, subst);
  if (isLogicVarTerm(t)) return logicVarName(t) === varName;
  if (Array.isArray(t)) {
    for (let i = 0; i < t.length; i++) if (occursLogic(varName, t[i], subst, seen)) return true;
    return false;
  }
  if (isObj(t)) {
    if (seen.has(t)) return false;
    seen.add(t);
    for (const k of Object.keys(t)) if (occursLogic(varName, t[k], subst, seen)) return true;
  }
  return false;
}

function bindLogicVar(v, x, subst) {
  const name = logicVarName(v);
  const val = derefLogic(x, subst);
  if (isLogicVarTerm(val) && logicVarName(val) === name) return subst;
  if (occursLogic(name, val, subst)) return null;
  const next = cloneSubst(subst);
  next[name] = val;
  return next;
}

function unifyLogic(a0, b0, subst0) {
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
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return null;
      for (let i = a.length - 1; i >= 0; i--) stack.push([a[i], b[i]]);
      continue;
    }
    if (isObj(a) && isObj(b) && !Array.isArray(a) && !Array.isArray(b)) {
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
}

function substituteLogic(term, subst, seen = new WeakSet()) {
  const t = derefLogic(term, subst);
  if (isLogicVarTerm(t)) return logicVarName(t);
  if (Array.isArray(t)) return t.map((x) => substituteLogic(x, subst, seen));
  if (isObj(t)) {
    if (seen.has(t)) return "[Circular]";
    seen.add(t);
    const out = Object.create(null);
    for (const k of Object.keys(t)) out[k] = substituteLogic(t[k], subst, seen);
    return out;
  }
  return t;
}

function hasLogicVars(term, subst = Object.create(null), seen = new WeakSet()) {
  const t = derefLogic(term, subst);
  if (isLogicVarTerm(t)) return true;
  if (Array.isArray(t)) {
    for (let i = 0; i < t.length; i++) if (hasLogicVars(t[i], subst, seen)) return true;
    return false;
  }
  if (isObj(t)) {
    if (seen.has(t)) return false;
    seen.add(t);
    for (const k of Object.keys(t)) if (hasLogicVars(t[k], subst, seen)) return true;
  }
  return false;
}

function collectLogicVars(term, out, seen = new WeakSet()) {
  if (isLogicVarTerm(term)) {
    out.add(logicVarName(term));
    return;
  }
  if (Array.isArray(term)) {
    for (let i = 0; i < term.length; i++) collectLogicVars(term[i], out, seen);
    return;
  }
  if (isObj(term)) {
    if (seen.has(term)) return;
    seen.add(term);
    for (const k of Object.keys(term)) collectLogicVars(term[k], out, seen);
  }
}

function freshenLogicTerm(term, suffix, table) {
  if (isLogicVarTerm(term)) {
    const k = logicVarName(term);
    if (!Object.prototype.hasOwnProperty.call(table, k)) table[k] = `${k}#${suffix}`;
    return table[k];
  }
  if (Array.isArray(term)) return term.map((x) => freshenLogicTerm(x, suffix, table));
  if (isObj(term)) {
    const out = Object.create(null);
    for (const k of Object.keys(term)) out[k] = freshenLogicTerm(term[k], suffix, table);
    return out;
  }
  return term;
}

function freshenLogicClause(clause, suffix) {
  const table = Object.create(null);
  return {
    head: freshenLogicTerm(clause.head, suffix, table),
    body: clause.body.map((lit) => ({
      negated: lit.negated,
      atom: freshenLogicTerm(lit.atom, suffix, table)
    }))
  };
}

function parseLogicOptions(raw, queryHasVars, defaultMaxSteps) {
  let maxSteps = defaultMaxSteps;
  let maxSolutions = queryHasVars ? 64 : 1;
  if (raw === undefined || raw === null) return { maxSteps, maxSolutions };
  if (typeof raw === "number" && Number.isFinite(raw)) {
    if (queryHasVars) maxSolutions = Math.max(1, raw | 0);
    else maxSteps = Math.max(1, raw | 0);
    return { maxSteps, maxSolutions };
  }
  if (Array.isArray(raw)) {
    if (raw.length > 0 && Number.isFinite(raw[0])) maxSteps = Math.max(1, raw[0] | 0);
    if (raw.length > 1 && Number.isFinite(raw[1])) maxSolutions = Math.max(1, raw[1] | 0);
    return { maxSteps, maxSolutions };
  }
  if (!isObj(raw)) throw new Error("logicInfer: options must be number, list, or object");
  if (Object.prototype.hasOwnProperty.call(raw, "maxSteps")) {
    if (raw.maxSteps === Infinity) maxSteps = Infinity;
    else if (Number.isFinite(raw.maxSteps)) maxSteps = Math.max(1, raw.maxSteps | 0);
  } else if (Object.prototype.hasOwnProperty.call(raw, "maxDepth")) {
    if (raw.maxDepth === Infinity) maxSteps = Infinity;
    else if (Number.isFinite(raw.maxDepth)) maxSteps = Math.max(1, raw.maxDepth | 0);
  }
  if (Object.prototype.hasOwnProperty.call(raw, "maxSolutions")) {
    if (Number.isFinite(raw.maxSolutions)) maxSolutions = Math.max(1, raw.maxSolutions | 0);
  } else if (Object.prototype.hasOwnProperty.call(raw, "limit")) {
    if (Number.isFinite(raw.limit)) maxSolutions = Math.max(1, raw.limit | 0);
  }
  return { maxSteps, maxSolutions };
}

function evalLogicBuiltin(atom, subst) {
  if (!Array.isArray(atom) || atom.length === 0) return null;
  const pred = derefLogic(atom[0], subst);
  if (typeof pred !== "string") return null;
  if (pred === "=" || pred === "equal" || pred === "eq") {
    if (atom.length !== 3) throw new Error("logicInfer: '=' expects 2 arguments");
    const unified = unifyLogic(atom[1], atom[2], subst);
    return unified ? [unified] : [];
  }
  if (pred === "!=" || pred === "notEqual") {
    if (atom.length !== 3) throw new Error("logicInfer: '!=' expects 2 arguments");
    const unified = unifyLogic(atom[1], atom[2], subst);
    return unified ? [] : [subst];
  }
  if (pred === "true") return [subst];
  if (pred === "false" || pred === "fail") return [];
  return null;
}

function formatLogicBinding(subst, queryVars) {
  if (queryVars.length === 0) return true;
  const out = [];
  for (let i = 0; i < queryVars.length; i++) {
    const k = queryVars[i];
    const v = substituteLogic(k, subst);
    if (isLogicVarTerm(v) && logicVarName(v) === k) continue;
    out.push([k, v]);
  }
  return out;
}

function runLogicInference(ctx, factsRaw, rulesRaw, queryRaw, optionsRaw) {
  const facts = ensureList(factsRaw, "logicInfer(facts)");
  const rules = Array.isArray(rulesRaw) ? rulesRaw : [rulesRaw];
  const queryLits = normalizeLogicQuery(queryRaw);
  const queryVarsSet = new Set();
  for (let i = 0; i < queryLits.length; i++) collectLogicVars(queryLits[i].atom, queryVarsSet);
  const queryVars = Array.from(queryVarsSet).sort();
  const opts = parseLogicOptions(optionsRaw, queryVars.length > 0, ctx.guard.maxInferenceSteps);
  const maxSteps = opts.maxSteps;
  const maxSolutions = opts.maxSolutions;

  const clauses = [];
  for (let i = 0; i < facts.length; i++) clauses.push({ head: normalizeLogicAtom(facts[i]), body: [] });
  for (let i = 0; i < rules.length; i++) clauses.push(normalizeLogicRule(rules[i], i));

  let stepCount = 0;
  let freshSeq = 0;
  const step = (phase) => {
    guardTickOps(ctx, 1);
    stepCount += 1;
    if (maxSteps !== Infinity && stepCount > maxSteps) {
      throw new Error(`logicInfer: maxSteps exceeded (${maxSteps})`);
    }
    if (phase) {
      // no-op; phase name is useful for future tracing hooks.
    }
  };

  const solutions = [];
  const seen = new Set();

  const search = (goals, subst, stopOnFirst) => {
    step("search");
    if (goals.length === 0) {
      if (stopOnFirst) return true;
      const binding = formatLogicBinding(subst, queryVars);
      const key = valueKey(binding);
      if (!seen.has(key)) {
        seen.add(key);
        solutions.push(binding);
      }
      return solutions.length >= maxSolutions;
    }

    const goal = goals[0];
    const rest = goals.slice(1);

    if (goal.negated) {
      const grounded = substituteLogic(goal.atom, subst);
      if (hasLogicVars(grounded)) throw new Error("logicInfer: negated literal must be ground");
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
      step("clause");
      freshSeq += 1;
      const clause = freshenLogicClause(clauses[i], freshSeq);
      const unified = unifyLogic(goal.atom, clause.head, subst);
      if (!unified) continue;
      if (search(clause.body.concat(rest), unified, stopOnFirst)) return true;
    }
    return false;
  };

  const stopOnFirst = queryVars.length === 0;
  const found = search(queryLits, Object.create(null), stopOnFirst);
  if (queryVars.length === 0) return !!found;
  return solutions;
}

function normalizeConstraintVars(raw, label) {
  const vars = Array.isArray(raw) ? raw : [raw];
  if (vars.length === 0) throw new Error(`${label}: vars must not be empty`);
  return vars.map((x) => String(x));
}

function makeConstraintLogic(kindRaw, varsRaw, argsRaw) {
  const out = Object.create(null);
  out.kind = String(kindRaw);
  out.vars = normalizeConstraintVars(varsRaw, "constraintLogic");
  if (argsRaw === undefined) out.args = [];
  else out.args = Array.isArray(argsRaw) ? argsRaw.slice() : [argsRaw];
  return out;
}

function toDomainMap(domainsRaw) {
  const out = Object.create(null);
  if (Array.isArray(domainsRaw)) {
    for (let i = 0; i < domainsRaw.length; i++) {
      const pair = ensureList(domainsRaw[i], `constraintSolve(domains)[${i}]`);
      if (pair.length < 2) throw new Error(`constraintSolve(domains)[${i}]: expected [name, domain]`);
      const name = String(pair[0]);
      const dom = ensureList(pair[1], `constraintSolve(domain '${name}')`).slice();
      if (dom.length === 0) throw new Error(`constraintSolve: domain '${name}' must not be empty`);
      out[name] = dom;
    }
    return out;
  }
  if (!isObj(domainsRaw) || Array.isArray(domainsRaw)) {
    throw new Error("constraintSolve: domains must be object or list of [name,domain]");
  }
  for (const [name, domain] of Object.entries(domainsRaw)) {
    const dom = ensureList(domain, `constraintSolve(domain '${name}')`).slice();
    if (dom.length === 0) throw new Error(`constraintSolve: domain '${name}' must not be empty`);
    out[name] = dom;
  }
  return out;
}

function normalizeConstraint(constraint, index) {
  if (typeof constraint === "function") return { kind: "fn", vars: null, fn: constraint };
  if (Array.isArray(constraint)) {
    if (constraint.length < 2) throw new Error(`constraintSolve: constraint[${index}] needs at least 2 items`);
    return { kind: String(constraint[0]), vars: normalizeConstraintVars(constraint[1], `constraint[${index}]`), args: constraint.slice(2) };
  }
  if (isObj(constraint)) {
    const fn = typeof constraint.fn === "function" ? constraint.fn : typeof constraint.predicate === "function" ? constraint.predicate : null;
    if (fn) {
      const vars = constraint.vars === undefined ? null : normalizeConstraintVars(constraint.vars, `constraint[${index}]`);
      return { kind: "fn", vars, fn };
    }
    const kindRaw = Object.prototype.hasOwnProperty.call(constraint, "kind")
      ? constraint.kind
      : Object.prototype.hasOwnProperty.call(constraint, "type")
        ? constraint.type
        : undefined;
    if (kindRaw === undefined) throw new Error(`constraintSolve: constraint[${index}] requires kind/type`);
    const vars = normalizeConstraintVars(constraint.vars, `constraint[${index}]`);
    let args = [];
    if (Object.prototype.hasOwnProperty.call(constraint, "args")) args = ensureList(constraint.args, `constraint[${index}].args`).slice();
    else if (Object.prototype.hasOwnProperty.call(constraint, "values")) args = ensureList(constraint.values, `constraint[${index}].values`).slice();
    else if (Object.prototype.hasOwnProperty.call(constraint, "value")) args = [constraint.value];
    return { kind: String(kindRaw), vars, args };
  }
  throw new Error(`constraintSolve: unsupported constraint format at index ${index}`);
}

function domainMinMax(domain, label) {
  let min = Infinity;
  let max = -Infinity;
  for (let i = 0; i < domain.length; i++) {
    const n = Number(domain[i]);
    if (!Number.isFinite(n)) throw new Error(`${label}: expected finite number`);
    if (n < min) min = n;
    if (n > max) max = n;
  }
  return { min, max };
}

function evalConstraint(ctx, cst, assignment, domainTable, domainVars, assignedCount) {
  if (cst.kind === "fn") {
    const ready = cst.vars === null
      ? assignedCount === domainVars.length
      : cst.vars.every((name) => Object.prototype.hasOwnProperty.call(assignment, name));
    if (!ready) return true;
    guardTickOps(ctx, 1);
    return !!cst.fn(assignment);
  }

  const vars = cst.vars;
  const args = cst.args || [];
  const values = [];
  let assigned = 0;
  for (let i = 0; i < vars.length; i++) {
    const name = vars[i];
    if (Object.prototype.hasOwnProperty.call(assignment, name)) {
      assigned += 1;
      values.push(assignment[name]);
    }
  }
  const allAssigned = assigned === vars.length;

  if (cst.kind === "allDifferent") {
    const seen = new Set();
    for (let i = 0; i < values.length; i++) {
      const k = valueKey(values[i]);
      if (seen.has(k)) return false;
      seen.add(k);
    }
    return true;
  }

  if (cst.kind === "equal") {
    if (values.length < 2) return true;
    const first = values[0];
    for (let i = 1; i < values.length; i++) if (!Object.is(first, values[i])) return false;
    return true;
  }

  if (cst.kind === "notEqual") {
    if (values.length < 2) return true;
    const seen = new Set();
    for (let i = 0; i < values.length; i++) {
      const k = valueKey(values[i]);
      if (seen.has(k)) return false;
      seen.add(k);
    }
    return true;
  }

  if (cst.kind === "less" || cst.kind === "lessEqual" || cst.kind === "greater" || cst.kind === "greaterEqual") {
    if (!allAssigned || values.length < 2) return true;
    for (let i = 0; i < values.length - 1; i++) {
      const a = Number(values[i]);
      const b = Number(values[i + 1]);
      if (!Number.isFinite(a) || !Number.isFinite(b)) return false;
      if (cst.kind === "less" && !(a < b)) return false;
      if (cst.kind === "lessEqual" && !(a <= b)) return false;
      if (cst.kind === "greater" && !(a > b)) return false;
      if (cst.kind === "greaterEqual" && !(a >= b)) return false;
    }
    return true;
  }

  if (cst.kind === "inDomain") {
    if (vars.length !== 1) throw new Error("constraintSolve: inDomain expects exactly 1 var");
    if (!Object.prototype.hasOwnProperty.call(assignment, vars[0])) return true;
    const allowed = args.length === 1 && Array.isArray(args[0]) ? args[0] : args;
    const set = new Set(allowed.map((x) => valueKey(x)));
    return set.has(valueKey(assignment[vars[0]]));
  }

  if (cst.kind === "sumEqual" || cst.kind === "sumLessEqual" || cst.kind === "sumGreaterEqual") {
    if (args.length < 1) throw new Error(`constraintSolve: ${cst.kind} requires target value`);
    const target = Number(args[0]);
    if (!Number.isFinite(target)) throw new Error(`constraintSolve: ${cst.kind} target must be finite number`);
    let sumAssigned = 0;
    for (let i = 0; i < values.length; i++) {
      const n = Number(values[i]);
      if (!Number.isFinite(n)) return false;
      sumAssigned += n;
    }
    if (allAssigned) {
      if (cst.kind === "sumEqual") return Object.is(sumAssigned, target);
      if (cst.kind === "sumLessEqual") return sumAssigned <= target;
      return sumAssigned >= target;
    }
    let minRest = 0;
    let maxRest = 0;
    for (let i = 0; i < vars.length; i++) {
      const name = vars[i];
      if (Object.prototype.hasOwnProperty.call(assignment, name)) continue;
      const mm = domainMinMax(domainTable[name], `constraintSolve:${cst.kind}:${name}`);
      minRest += mm.min;
      maxRest += mm.max;
    }
    if (cst.kind === "sumEqual") return sumAssigned + minRest <= target && sumAssigned + maxRest >= target;
    if (cst.kind === "sumLessEqual") return sumAssigned + minRest <= target;
    return sumAssigned + maxRest >= target;
  }

  throw new Error(`constraintSolve: unknown constraint kind '${cst.kind}'`);
}

function parseConstraintOptions(raw, defaultNodeLimit) {
  let maxSolutions = 1;
  let maxNodes = defaultNodeLimit;
  if (raw === undefined || raw === null) return { maxSolutions, maxNodes };
  if (typeof raw === "number" && Number.isFinite(raw)) return { maxSolutions: Math.max(1, raw | 0), maxNodes };
  if (Array.isArray(raw)) {
    if (raw.length > 0 && Number.isFinite(raw[0])) maxSolutions = Math.max(1, raw[0] | 0);
    if (raw.length > 1) {
      if (raw[1] === Infinity) maxNodes = Infinity;
      else if (Number.isFinite(raw[1])) maxNodes = Math.max(1, raw[1] | 0);
    }
    return { maxSolutions, maxNodes };
  }
  if (!isObj(raw)) throw new Error("constraintSolve: options must be number, list, or object");
  if (Object.prototype.hasOwnProperty.call(raw, "maxSolutions") && Number.isFinite(raw.maxSolutions)) {
    maxSolutions = Math.max(1, raw.maxSolutions | 0);
  } else if (Object.prototype.hasOwnProperty.call(raw, "limit") && Number.isFinite(raw.limit)) {
    maxSolutions = Math.max(1, raw.limit | 0);
  }
  if (Object.prototype.hasOwnProperty.call(raw, "maxNodes")) {
    if (raw.maxNodes === Infinity) maxNodes = Infinity;
    else if (Number.isFinite(raw.maxNodes)) maxNodes = Math.max(1, raw.maxNodes | 0);
  } else if (Object.prototype.hasOwnProperty.call(raw, "nodeLimit")) {
    if (raw.nodeLimit === Infinity) maxNodes = Infinity;
    else if (Number.isFinite(raw.nodeLimit)) maxNodes = Math.max(1, raw.nodeLimit | 0);
  }
  return { maxSolutions, maxNodes };
}

function makeSolutionPairs(domainVars, assignment) {
  const out = [];
  for (let i = 0; i < domainVars.length; i++) out.push([domainVars[i], assignment[domainVars[i]]]);
  return out;
}

function solveConstraints(ctx, domainsRaw, constraintsRaw, optionsRaw) {
  const domainTable = toDomainMap(domainsRaw);
  const domainVars = Object.keys(domainTable);
  if (domainVars.length === 0) return null;

  const rawConstraints = Array.isArray(constraintsRaw) ? constraintsRaw : [constraintsRaw];
  const constraints = rawConstraints.map((cst, i) => normalizeConstraint(cst, i));
  for (let i = 0; i < constraints.length; i++) {
    const cst = constraints[i];
    if (cst.kind === "fn" && cst.vars === null) continue;
    const vars = cst.vars || [];
    for (let j = 0; j < vars.length; j++) {
      if (!Object.prototype.hasOwnProperty.call(domainTable, vars[j])) {
        throw new Error(`constraintSolve: unknown variable '${vars[j]}' in constraint[${i}]`);
      }
    }
  }

  const opts = parseConstraintOptions(optionsRaw, ctx.guard.maxConstraintNodes);
  const maxSolutions = opts.maxSolutions;
  const maxNodes = opts.maxNodes;

  const assignment = Object.create(null);
  const solutions = [];
  let assignedCount = 0;
  let nodeCount = 0;

  const constraintsPass = () => {
    for (let i = 0; i < constraints.length; i++) {
      guardTickOps(ctx, 1);
      if (!evalConstraint(ctx, constraints[i], assignment, domainTable, domainVars, assignedCount)) return false;
    }
    return true;
  };

  const chooseNextVar = () => {
    let best = null;
    let bestSize = Infinity;
    for (let i = 0; i < domainVars.length; i++) {
      const name = domainVars[i];
      if (Object.prototype.hasOwnProperty.call(assignment, name)) continue;
      const size = domainTable[name].length;
      if (size < bestSize) {
        best = name;
        bestSize = size;
      }
    }
    return best;
  };

  const search = () => {
    guardTickOps(ctx, 1);
    nodeCount += 1;
    if (maxNodes !== Infinity && nodeCount > maxNodes) {
      throw new Error(`constraintSolve: maxNodes exceeded (${maxNodes})`);
    }
    if (!constraintsPass()) return false;

    if (assignedCount === domainVars.length) {
      solutions.push(makeSolutionPairs(domainVars, assignment));
      return solutions.length >= maxSolutions;
    }

    const name = chooseNextVar();
    const domain = domainTable[name];
    for (let i = 0; i < domain.length; i++) {
      guardTickOps(ctx, 1);
      assignment[name] = domain[i];
      assignedCount += 1;
      const done = search();
      assignedCount -= 1;
      delete assignment[name];
      if (done) return true;
    }
    return false;
  };

  search();
  if (maxSolutions === 1) return solutions.length > 0 ? solutions[0] : null;
  return solutions;
}

function makeBuiltins() {
  return {
    print: {
      type: tFn([T_ANY], T_VOID),
      fn: (args, ctx) => {
        const v = args[0];
        if (ctx.output.length < ctx.maxOutput) {
          ctx.output.push(v);
          ctx.loggerFn(v);
        }
        return undefined;
      }
    },
    len: {
      type: tFn([T_ANY], T_NUMBER),
      fn: (args) => {
        const v = args[0];
        if (typeof v === "string" || Array.isArray(v)) return v.length;
        throw new Error("len(value): value must be string or list");
      }
    },
    toString: {
      type: tFn([T_ANY], T_STRING),
      fn: (args) => String(args[0])
    },
    sum: {
      type: tFn([tList(T_NUMBER)], T_NUMBER),
      fn: (args) => args[0].reduce((a, b) => a + b, 0)
    },
    logicInfer: {
      type: tFn([T_ANY, T_ANY, T_ANY, T_ANY], T_ANY),
      fn: (args, ctx) => runLogicInference(ctx, args[0], args[1], args[2], args[3])
    },
    constraintLogic: {
      type: tFn([T_ANY, T_ANY, T_ANY], T_ANY),
      fn: (args) => makeConstraintLogic(args[0], args[1], args[2])
    },
    constraintSolve: {
      type: tFn([T_ANY, T_ANY, T_ANY], T_ANY),
      fn: (args, ctx) => solveConstraints(ctx, args[0], args[1], args[2])
    }
  };
}

const BUILTINS = makeBuiltins();

function lookupExternalType(name, tctx) {
  if (!tctx || !tctx.externalTypes) return null;
  return tctx.externalTypes[name] || null;
}

function checkExternalCall(calleeName, argTypes, tctx) {
  const fnType = lookupExternalType(calleeName, tctx);
  if (!fnType) {
    if (tctx && tctx.externalNames && tctx.externalNames.has(calleeName)) return T_ANY;
    throw new Error(`Type error: unknown function '${calleeName}'`);
  }

  if (argTypes.length !== fnType.params.length) {
    throw new Error(`Type error: external '${calleeName}' expects ${fnType.params.length} args, got ${argTypes.length}`);
  }
  for (let i = 0; i < argTypes.length; i++) {
    if (!isAssignable(argTypes[i], fnType.params[i])) {
      throw new Error(
        `Type error: external '${calleeName}' arg ${i + 1} expects ${typeStr(fnType.params[i])}, got ${typeStr(argTypes[i])}`
      );
    }
  }
  return fnType.ret;
}

function checkExpr(node, scope, tctx) {
  switch (node.type) {
    case "Literal":
      return node.valueType;

    case "Identifier": {
      const rec = scope.get(node.name);
      if (!rec) throw new Error(`Type error: unknown variable '${node.name}'`);
      return rec.type;
    }

    case "ArrayExpr": {
      if (node.elements.length === 0) return tList(T_ANY);
      let inner = checkExpr(node.elements[0], scope, tctx);
      for (let i = 1; i < node.elements.length; i++) {
        const it = checkExpr(node.elements[i], scope, tctx);
        if (!isSameType(inner, it)) {
          inner = T_ANY;
          break;
        }
      }
      return tList(inner);
    }

    case "UnaryExpr": {
      const arg = checkExpr(node.argument, scope, tctx);
      if (node.operator === "-") {
        if (!isAssignable(arg, T_NUMBER)) throw new Error("Type error: unary '-' requires number");
        return T_NUMBER;
      }
      if (node.operator === "!") {
        if (!isAssignable(arg, T_BOOL)) throw new Error("Type error: unary '!' requires bool");
        return T_BOOL;
      }
      throw new Error(`Type error: unknown unary operator '${node.operator}'`);
    }

    case "BinaryExpr": {
      const lt = checkExpr(node.left, scope, tctx);
      const rt = checkExpr(node.right, scope, tctx);
      const op = node.operator;
      if (op === "+") {
        if (isAssignable(lt, T_STRING) && isAssignable(rt, T_STRING)) return T_STRING;
        if (isAssignable(lt, T_NUMBER) && isAssignable(rt, T_NUMBER)) return T_NUMBER;
        throw new Error("Type error: '+' requires number+number or string+string");
      }
      if (op === "-" || op === "*" || op === "/" || op === "%") {
        if (!isAssignable(lt, T_NUMBER) || !isAssignable(rt, T_NUMBER)) throw new Error(`Type error: '${op}' requires numbers`);
        return T_NUMBER;
      }
      if (op === "<" || op === "<=" || op === ">" || op === ">=") {
        if (!isAssignable(lt, T_NUMBER) || !isAssignable(rt, T_NUMBER)) throw new Error(`Type error: '${op}' requires numbers`);
        return T_BOOL;
      }
      if (op === "==" || op === "!=") return T_BOOL;
      throw new Error(`Type error: unknown binary operator '${op}'`);
    }

    case "LogicalExpr": {
      const lt = checkExpr(node.left, scope, tctx);
      const rt = checkExpr(node.right, scope, tctx);
      if (!isAssignable(lt, T_BOOL) || !isAssignable(rt, T_BOOL)) {
        throw new Error(`Type error: '${node.operator}' requires bool operands`);
      }
      return T_BOOL;
    }

    case "RangeExpr": {
      const lt = checkExpr(node.left, scope, tctx);
      const rt = checkExpr(node.right, scope, tctx);
      if (!isAssignable(lt, T_NUMBER) || !isAssignable(rt, T_NUMBER)) throw new Error("Type error: '..' requires numbers");
      return tList(T_NUMBER);
    }

    case "IndexExpr": {
      const obj = checkExpr(node.object, scope, tctx);
      const idx = checkExpr(node.index, scope, tctx);
      if (!isAssignable(idx, T_NUMBER)) throw new Error("Type error: index must be number");
      if (obj.kind !== "list" && !isAnyType(obj)) throw new Error("Type error: indexing requires list");
      return obj.kind === "list" ? obj.inner : T_ANY;
    }

    case "AssignExpr": {
      const target = scope.get(node.target.name);
      if (!target) throw new Error(`Type error: unknown variable '${node.target.name}'`);
      if (!target.mutable) throw new Error(`Type error: cannot assign to const '${node.target.name}'`);
      const rhs = checkExpr(node.value, scope, tctx);
      if (node.operator === "=") {
        if (!isAssignable(rhs, target.type)) {
          throw new Error(`Type error: '${node.target.name}' expects ${typeStr(target.type)}, got ${typeStr(rhs)}`);
        }
        return target.type;
      }
      if (!isAssignable(target.type, T_NUMBER) || !isAssignable(rhs, T_NUMBER)) {
        throw new Error(`Type error: '${node.operator}' requires number variable and number value`);
      }
      return T_NUMBER;
    }

    case "CallExpr": {
      const argTypes = node.args.map((a) => checkExpr(a, scope, tctx));

      if (node.callee.type === "Identifier" && node.callee.name === "external") {
        if (argTypes.length < 1) throw new Error("Type error: external(name, ...args) requires at least 1 arg");
        if (!isAssignable(argTypes[0], T_STRING)) throw new Error("Type error: external first arg must be string");
        if (node.args[0].type === "Literal" && typeof node.args[0].value === "string") {
          const extName = node.args[0].value;
          return checkExternalCall(extName, argTypes.slice(1), tctx);
        }
        return T_ANY;
      }

      if (node.callee.type === "Identifier") {
        const local = scope.get(node.callee.name);
        if (!local) {
          return checkExternalCall(node.callee.name, argTypes, tctx);
        }
      }

      const callee = checkExpr(node.callee, scope, tctx);
      if (callee.kind !== "fn" && !isAnyType(callee)) throw new Error("Type error: callee is not callable");
      if (callee.kind === "fn") {
        if (argTypes.length !== callee.params.length) {
          throw new Error(`Type error: function expects ${callee.params.length} args, got ${argTypes.length}`);
        }
        for (let i = 0; i < argTypes.length; i++) {
          if (!isAssignable(argTypes[i], callee.params[i])) {
            throw new Error(`Type error: arg ${i + 1} expects ${typeStr(callee.params[i])}, got ${typeStr(argTypes[i])}`);
          }
        }
        return callee.ret;
      }
      return T_ANY;
    }

    case "PipeExpr": {
      const leftType = checkExpr(node.left, scope, tctx);
      if (node.right.type === "Identifier") {
        if (!scope.get(node.right.name)) {
          return checkExternalCall(node.right.name, [leftType], tctx);
        }
        const fnType = checkExpr(node.right, scope, tctx);
        if (fnType.kind !== "fn") throw new Error("Type error: right side of '|>' must be function");
        if (fnType.params.length < 1) throw new Error("Type error: piped function needs at least 1 param");
        if (!isAssignable(leftType, fnType.params[0])) throw new Error("Type error: piped value type mismatch");
        return fnType.ret;
      }
      if (node.right.type === "CallExpr") {
        if (node.right.callee.type === "Identifier" && !scope.get(node.right.callee.name)) {
          const restExt = node.right.args.map((a) => checkExpr(a, scope, tctx));
          return checkExternalCall(node.right.callee.name, [leftType, ...restExt], tctx);
        }
        const fnType = checkExpr(node.right.callee, scope, tctx);
        if (fnType.kind !== "fn") throw new Error("Type error: right side of '|>' must be function call");
        const rest = node.right.args.map((a) => checkExpr(a, scope, tctx));
        if (fnType.params.length !== rest.length + 1) throw new Error("Type error: piped call arity mismatch");
        if (!isAssignable(leftType, fnType.params[0])) throw new Error("Type error: piped value type mismatch");
        for (let i = 0; i < rest.length; i++) {
          if (!isAssignable(rest[i], fnType.params[i + 1])) throw new Error(`Type error: piped arg ${i + 1} type mismatch`);
        }
        return fnType.ret;
      }
      throw new Error("Type error: right side of '|>' must be function or function call");
    }

    default:
      throw new Error(`Type error: unknown expr '${node.type}'`);
  }
}

function registerFnSignatures(ast, scope) {
  for (const stmt of ast.body) {
    if (stmt.type !== "FnDecl") continue;
    scope.define(stmt.name, tFn(stmt.params.map((p) => p.typeAnn), stmt.ret), false);
  }
}

function checkBlock(block, scope, ctx, tctx) {
  let last = T_VOID;
  for (const stmt of block.body) last = checkStmt(stmt, scope, ctx, tctx);
  return last;
}

function checkStmt(stmt, scope, ctx, tctx) {
  switch (stmt.type) {
    case "LetDecl": {
      const initType = checkExpr(stmt.init, scope, tctx);
      const finalType = stmt.typeAnn ? normalizeType(stmt.typeAnn) : initType;
      if (!isAssignable(initType, finalType)) {
        throw new Error(`Type error: '${stmt.name}' expects ${typeStr(finalType)}, got ${typeStr(initType)}`);
      }
      scope.define(stmt.name, finalType, stmt.mutable);
      return T_VOID;
    }

    case "FnDecl": {
      const fnRec = scope.get(stmt.name);
      const fnType = fnRec ? fnRec.type : tFn(stmt.params.map((p) => p.typeAnn), stmt.ret);
      const fnScope = new TypeScope(scope);
      for (const p of stmt.params) fnScope.define(p.name, p.typeAnn, true);
      const fnCtx = { inFunction: true, returnType: fnType.ret };
      if (stmt.exprBody) {
        const bodyType = checkExpr(stmt.body, fnScope, tctx);
        if (!isAssignable(bodyType, fnType.ret)) {
          throw new Error(`Type error: function '${stmt.name}' returns ${typeStr(bodyType)}, expected ${typeStr(fnType.ret)}`);
        }
      } else {
        checkBlock(stmt.body, fnScope, fnCtx, tctx);
      }
      return T_VOID;
    }

    case "BlockStmt":
      return checkBlock(stmt, new TypeScope(scope), ctx, tctx);

    case "IfStmt": {
      const testType = checkExpr(stmt.test, scope, tctx);
      if (!isAssignable(testType, T_BOOL)) throw new Error("Type error: if condition must be bool");
      checkBlock(stmt.consequent, new TypeScope(scope), ctx, tctx);
      if (stmt.alternate) {
        if (stmt.alternate.type === "IfStmt") checkStmt(stmt.alternate, new TypeScope(scope), ctx, tctx);
        else checkBlock(stmt.alternate, new TypeScope(scope), ctx, tctx);
      }
      return T_VOID;
    }

    case "WhileStmt": {
      const testType = checkExpr(stmt.test, scope, tctx);
      if (!isAssignable(testType, T_BOOL)) throw new Error("Type error: while condition must be bool");
      checkBlock(stmt.body, new TypeScope(scope), ctx, tctx);
      return T_VOID;
    }

    case "ForInStmt": {
      const iterType = checkExpr(stmt.iterable, scope, tctx);
      if (iterType.kind !== "list" && !isAnyType(iterType)) throw new Error("Type error: for-in iterable must be list");
      const forScope = new TypeScope(scope);
      forScope.define(stmt.iterator, iterType.kind === "list" ? iterType.inner : T_ANY, true);
      checkBlock(stmt.body, forScope, ctx, tctx);
      return T_VOID;
    }

    case "ReturnStmt": {
      if (!ctx.inFunction) throw new Error("Type error: return can only be used inside function");
      const actual = stmt.argument ? checkExpr(stmt.argument, scope, tctx) : T_VOID;
      if (!isAssignable(actual, ctx.returnType)) {
        throw new Error(`Type error: return expects ${typeStr(ctx.returnType)}, got ${typeStr(actual)}`);
      }
      return ctx.returnType;
    }

    case "ExprStmt":
      return checkExpr(stmt.expression, scope, tctx);

    default:
      throw new Error(`Type error: unknown stmt '${stmt.type}'`);
  }
}

function normalizeGlobalTypes(raw = {}) {
  if (!raw || typeof raw !== "object") return {};
  const out = {};
  for (const [k, v] of Object.entries(raw)) {
    if (typeof v === "string") out[k] = parseTypeString(v);
    else if (v && typeof v === "object" && v.kind) out[k] = normalizeType(v);
    else out[k] = T_ANY;
  }
  return out;
}

function normalizeExternalTypes(raw = {}) {
  if (!raw || typeof raw !== "object") return {};
  const out = {};
  for (const [name, spec] of Object.entries(raw)) {
    if (!spec || typeof spec !== "object") continue;
    const paramsRaw = Array.isArray(spec.params) ? spec.params : [];
    const params = paramsRaw.map((p) => {
      if (typeof p === "string") return parseTypeString(p);
      if (p && typeof p === "object" && p.kind) return normalizeType(p);
      return T_ANY;
    });
    let ret = T_ANY;
    if (typeof spec.returnType === "string") ret = parseTypeString(spec.returnType);
    else if (spec.returnType && typeof spec.returnType === "object" && spec.returnType.kind) ret = normalizeType(spec.returnType);
    out[name] = tFn(params, ret);
  }
  return out;
}

function typeCheck(ast, options = {}) {
  const scope = new TypeScope(null);
  for (const [name, spec] of Object.entries(BUILTINS)) scope.define(name, spec.type, false);
  const globalTypes = normalizeGlobalTypes(options.globalTypes || {});
  for (const [name, type] of Object.entries(globalTypes)) scope.define(name, type, true);
  const externalNames = new Set();
  for (const name of Object.keys(options.externals || {})) externalNames.add(name);

  const tctx = {
    externalTypes: normalizeExternalTypes(options.externalTypes || {}),
    externalNames
  };
  for (const name of Object.keys(tctx.externalTypes)) externalNames.add(name);

  registerFnSignatures(ast, scope);
  const ctx = { inFunction: false, returnType: T_VOID };
  let last = T_VOID;
  for (const stmt of ast.body) last = checkStmt(stmt, scope, ctx, tctx);
  return { ok: true, type: last };
}

class RuntimeScope {
  constructor(parent = null) {
    this.parent = parent;
    this.vars = new Map();
  }

  define(name, value, type, mutable, builtin = false) {
    if (this.vars.has(name)) throw new Error(`Runtime error: '${name}' is already defined in this scope`);
    this.vars.set(name, { value, type: normalizeType(type), mutable: !!mutable, builtin: !!builtin });
  }

  getCell(name) {
    if (this.vars.has(name)) return this.vars.get(name);
    if (this.parent) return this.parent.getCell(name);
    return null;
  }

  get(name) {
    const cell = this.getCell(name);
    if (!cell) throw new Error(`Runtime error: unknown variable '${name}'`);
    return cell.value;
  }

  assign(name, value, strictTypes) {
    if (this.vars.has(name)) {
      const cell = this.vars.get(name);
      if (!cell.mutable) throw new Error(`Runtime error: cannot assign to const '${name}'`);
      if (strictTypes) assertType(value, cell.type, `assign '${name}'`);
      cell.value = value;
      return value;
    }
    if (this.parent) return this.parent.assign(name, value, strictTypes);
    throw new Error(`Runtime error: unknown variable '${name}'`);
  }

  exportVars() {
    const out = {};
    for (const [name, cell] of this.vars.entries()) if (!cell.builtin) out[name] = cell.value;
    return out;
  }
}

class ReturnSignal {
  constructor(value) {
    this.value = value;
  }
}

function normalizeExternals(raw = {}) {
  if (!raw || typeof raw !== "object") return {};
  const out = {};
  for (const [name, fn] of Object.entries(raw)) {
    if (typeof fn !== "function") throw new Error(`externals['${name}'] must be a function`);
    out[name] = fn;
  }
  return out;
}

function normalizeGuardOptions(options = {}) {
  const out = { ...DEFAULT_GUARDS };
  if (Number.isFinite(options.maxOps)) out.maxOps = Math.max(1, Math.floor(options.maxOps));
  if (Number.isFinite(options.maxRuntimeMs)) out.maxRuntimeMs = Math.max(1, Math.floor(options.maxRuntimeMs));
  if (Number.isFinite(options.maxLoopIters)) out.maxLoopIters = Math.max(1, Math.floor(options.maxLoopIters));
  if (options.maxLoopStallIters === Infinity) out.maxLoopStallIters = Infinity;
  else if (Number.isFinite(options.maxLoopStallIters)) out.maxLoopStallIters = Math.max(1, Math.floor(options.maxLoopStallIters));
  if (Number.isFinite(options.maxCallDepth)) out.maxCallDepth = Math.max(1, Math.floor(options.maxCallDepth));
  if (Number.isFinite(options.maxCalls)) out.maxCalls = Math.max(1, Math.floor(options.maxCalls));
  if (options.maxInferenceSteps === Infinity) out.maxInferenceSteps = Infinity;
  else if (Number.isFinite(options.maxInferenceSteps)) out.maxInferenceSteps = Math.max(1, Math.floor(options.maxInferenceSteps));
  if (options.maxConstraintNodes === Infinity) out.maxConstraintNodes = Infinity;
  else if (Number.isFinite(options.maxConstraintNodes)) out.maxConstraintNodes = Math.max(1, Math.floor(options.maxConstraintNodes));
  return out;
}

function guardCheckTime(ctx) {
  if (Date.now() - ctx.guard.startedAt > ctx.guard.maxRuntimeMs) {
    throw new Error(`GuardError: maxRuntimeMs exceeded (${ctx.guard.maxRuntimeMs})`);
  }
}

function guardTickOps(ctx, n = 1) {
  ctx.guard.ops += n;
  if (ctx.guard.ops > ctx.guard.maxOps) throw new Error(`GuardError: maxOps exceeded (${ctx.guard.maxOps})`);
  guardCheckTime(ctx);
}

function guardTickLoop(ctx) {
  ctx.guard.loopIters += 1;
  if (ctx.guard.loopIters > ctx.guard.maxLoopIters) {
    throw new Error(`GuardError: maxLoopIters exceeded (${ctx.guard.maxLoopIters})`);
  }
  guardTickOps(ctx, 1);
}

function guardEnterCall(ctx) {
  ctx.guard.callDepth += 1;
  ctx.guard.calls += 1;
  if (ctx.guard.callDepth > ctx.guard.maxCallDepth) {
    throw new Error(`GuardError: maxCallDepth exceeded (${ctx.guard.maxCallDepth})`);
  }
  if (ctx.guard.calls > ctx.guard.maxCalls) {
    throw new Error(`GuardError: maxCalls exceeded (${ctx.guard.maxCalls})`);
  }
  guardTickOps(ctx, 1);
}

function guardLeaveCall(ctx) {
  ctx.guard.callDepth = Math.max(0, ctx.guard.callDepth - 1);
}

function isRaggiraleFn(v) {
  return !!(v && typeof v === "object" && v.__raggiraleFn === true);
}

function makeBuiltinFn(name, spec) {
  return {
    __raggiraleFn: true,
    name,
    params: spec.type.params,
    ret: spec.type.ret,
    call(args, ctx) {
      return spec.fn(args, ctx);
    }
  };
}

function callFn(fn, args, ctx) {
  guardEnterCall(ctx);
  try {
    if (isRaggiraleFn(fn)) {
      if (ctx.strictTypes && !fn.variadic) {
        if (args.length !== fn.params.length) {
          throw new Error(`Runtime error: function '${fn.name}' expects ${fn.params.length} args, got ${args.length}`);
        }
      }
      if (ctx.strictTypes) {
        const bound = Math.min(args.length, fn.params.length);
        for (let i = 0; i < bound; i++) assertType(args[i], fn.params[i], `arg ${i + 1} of '${fn.name}'`);
      }
      const ret = fn.call(args, ctx);
      if (ctx.strictTypes) assertType(ret, fn.ret, `return value of '${fn.name}'`);
      return ret;
    }

    if (typeof fn === "function") return fn(...args);
    throw new Error("Runtime error: value is not callable");
  } finally {
    guardLeaveCall(ctx);
  }
}

function hasExternal(ctx, name) {
  return Object.prototype.hasOwnProperty.call(ctx.externals, name);
}

function callExternalByName(name, args, ctx) {
  if (!hasExternal(ctx, name)) throw new Error(`Unknown function '${name}'`);
  const fn = ctx.externals[name];
  const extType = ctx.externalTypes[name] || null;

  if (ctx.strictTypes && extType) {
    if (args.length !== extType.params.length) {
      throw new Error(`Runtime error: external '${name}' expects ${extType.params.length} args, got ${args.length}`);
    }
    for (let i = 0; i < args.length; i++) {
      assertType(args[i], extType.params[i], `arg ${i + 1} of external '${name}'`);
    }
  }

  const ret = callFn(fn, args, ctx);

  if (ctx.strictTypes && extType) {
    assertType(ret, extType.ret, `return value of external '${name}'`);
  }
  return ret;
}

function makeRangeInclusive(a, b) {
  const start = a | 0;
  const end = b | 0;
  const out = [];
  if (start <= end) for (let i = start; i <= end; i++) out.push(i);
  else for (let i = start; i >= end; i--) out.push(i);
  return out;
}

function evalExpr(node, scope, ctx) {
  guardTickOps(ctx, 1);

  switch (node.type) {
    case "Literal":
      return node.value;

    case "Identifier":
      return scope.get(node.name);

    case "ArrayExpr":
      return node.elements.map((e) => evalExpr(e, scope, ctx));

    case "UnaryExpr": {
      const v = evalExpr(node.argument, scope, ctx);
      if (node.operator === "-") return -v;
      if (node.operator === "!") return !v;
      throw new Error(`Runtime error: unknown unary operator '${node.operator}'`);
    }

    case "BinaryExpr": {
      const l = evalExpr(node.left, scope, ctx);
      const r = evalExpr(node.right, scope, ctx);
      if (node.operator === "+") return l + r;
      if (node.operator === "-") return l - r;
      if (node.operator === "*") return l * r;
      if (node.operator === "/") return l / r;
      if (node.operator === "%") return l % r;
      if (node.operator === "<") return l < r;
      if (node.operator === "<=") return l <= r;
      if (node.operator === ">") return l > r;
      if (node.operator === ">=") return l >= r;
      if (node.operator === "==") return l === r;
      if (node.operator === "!=") return l !== r;
      throw new Error(`Runtime error: unknown binary operator '${node.operator}'`);
    }

    case "LogicalExpr": {
      if (node.operator === "&&") {
        const left = !!evalExpr(node.left, scope, ctx);
        return left ? !!evalExpr(node.right, scope, ctx) : false;
      }
      if (node.operator === "||") {
        const left = !!evalExpr(node.left, scope, ctx);
        return left ? true : !!evalExpr(node.right, scope, ctx);
      }
      throw new Error(`Runtime error: unknown logical operator '${node.operator}'`);
    }

    case "RangeExpr": {
      const a = evalExpr(node.left, scope, ctx);
      const b = evalExpr(node.right, scope, ctx);
      return makeRangeInclusive(a, b);
    }

    case "IndexExpr": {
      const obj = evalExpr(node.object, scope, ctx);
      const idx = evalExpr(node.index, scope, ctx);
      if (!Array.isArray(obj)) throw new Error("Runtime error: index target must be list");
      return obj[idx | 0];
    }

    case "AssignExpr": {
      const name = node.target.name;
      const rhs = evalExpr(node.value, scope, ctx);
      if (node.operator === "=") return scope.assign(name, rhs, ctx.strictTypes);
      const cur = scope.get(name);
      if (node.operator === "+=") return scope.assign(name, cur + rhs, ctx.strictTypes);
      if (node.operator === "-=") return scope.assign(name, cur - rhs, ctx.strictTypes);
      if (node.operator === "*=") return scope.assign(name, cur * rhs, ctx.strictTypes);
      if (node.operator === "/=") return scope.assign(name, cur / rhs, ctx.strictTypes);
      throw new Error(`Runtime error: unknown assignment operator '${node.operator}'`);
    }

    case "CallExpr": {
      if (node.callee.type === "Identifier" && node.callee.name === "external") {
        if (node.args.length < 1) throw new Error("Runtime error: external(name, ...args) requires at least 1 arg");
        const fnNameRaw = evalExpr(node.args[0], scope, ctx);
        const fnName = String(fnNameRaw);
        const callArgs = node.args.slice(1).map((a) => evalExpr(a, scope, ctx));
        return callExternalByName(fnName, callArgs, ctx);
      }

      if (node.callee.type === "Identifier") {
        const local = scope.getCell(node.callee.name);
        if (!local) {
          const extArgs = node.args.map((a) => evalExpr(a, scope, ctx));
          return callExternalByName(node.callee.name, extArgs, ctx);
        }
      }

      const callee = evalExpr(node.callee, scope, ctx);
      const args = node.args.map((a) => evalExpr(a, scope, ctx));
      return callFn(callee, args, ctx);
    }

    case "PipeExpr": {
      const v = evalExpr(node.left, scope, ctx);
      if (node.right.type === "Identifier") {
        if (!scope.getCell(node.right.name)) {
          return callExternalByName(node.right.name, [v], ctx);
        }
        const fn = evalExpr(node.right, scope, ctx);
        return callFn(fn, [v], ctx);
      }
      if (node.right.type === "CallExpr") {
        const args = node.right.args.map((a) => evalExpr(a, scope, ctx));
        if (node.right.callee.type === "Identifier" && !scope.getCell(node.right.callee.name)) {
          return callExternalByName(node.right.callee.name, [v, ...args], ctx);
        }
        const fn = evalExpr(node.right.callee, scope, ctx);
        return callFn(fn, [v, ...args], ctx);
      }
      throw new Error("Runtime error: right side of '|>' must be callable");
    }

    default:
      throw new Error(`Runtime error: unknown expr '${node.type}'`);
  }
}

function evalBlock(block, parent, ctx) {
  const scope = new RuntimeScope(parent);
  let last = undefined;
  for (const stmt of block.body) {
    guardTickOps(ctx, 1);
    last = evalStmt(stmt, scope, ctx);
  }
  return last;
}

function makeUserFn(decl, closure) {
  return {
    __raggiraleFn: true,
    name: decl.name,
    params: decl.params.map((p) => p.typeAnn),
    ret: decl.ret,
    call(args, ctx) {
      if (args.length !== decl.params.length) {
        throw new Error(`Runtime error: function '${decl.name}' expects ${decl.params.length} args, got ${args.length}`);
      }

      const fnScope = new RuntimeScope(closure);
      for (let i = 0; i < decl.params.length; i++) {
        const p = decl.params[i];
        if (ctx.strictTypes) assertType(args[i], p.typeAnn, `arg '${p.name}' of '${decl.name}'`);
        fnScope.define(p.name, args[i], p.typeAnn, true, false);
      }

      let out = undefined;
      try {
        if (decl.exprBody) out = evalExpr(decl.body, fnScope, ctx);
        else evalBlock(decl.body, fnScope, ctx);
      } catch (err) {
        if (err instanceof ReturnSignal) out = err.value;
        else throw err;
      }

      if (ctx.strictTypes) assertType(out, decl.ret, `return value of '${decl.name}'`);
      return out;
    }
  };
}

function evalStmt(stmt, scope, ctx) {
  guardTickOps(ctx, 1);

  switch (stmt.type) {
    case "LetDecl": {
      const v = evalExpr(stmt.init, scope, ctx);
      const t = stmt.typeAnn || inferTypeFromValue(v);
      if (ctx.strictTypes) assertType(v, t, `let '${stmt.name}'`);
      scope.define(stmt.name, v, t, stmt.mutable, false);
      return v;
    }

    case "FnDecl": {
      const fn = makeUserFn(stmt, scope);
      scope.define(stmt.name, fn, tFn(stmt.params.map((p) => p.typeAnn), stmt.ret), false, false);
      return fn;
    }

    case "ExprStmt":
      return evalExpr(stmt.expression, scope, ctx);

    case "BlockStmt":
      return evalBlock(stmt, scope, ctx);

    case "IfStmt": {
      const cond = !!evalExpr(stmt.test, scope, ctx);
      if (cond) return evalBlock(stmt.consequent, scope, ctx);
      if (stmt.alternate) {
        if (stmt.alternate.type === "IfStmt") return evalStmt(stmt.alternate, scope, ctx);
        return evalBlock(stmt.alternate, scope, ctx);
      }
      return undefined;
    }

    case "WhileStmt": {
      let last = undefined;
      let stallCount = 0;
      let prevSig = scopeVisibleSignature(scope);
      while (evalExpr(stmt.test, scope, ctx)) {
        guardTickLoop(ctx);
        last = evalBlock(stmt.body, scope, ctx);
        if (ctx.guard.maxLoopStallIters !== Infinity) {
          guardTickOps(ctx, 1);
          const nextSig = scopeVisibleSignature(scope);
          if (nextSig === prevSig) {
            stallCount += 1;
            if (stallCount > ctx.guard.maxLoopStallIters) {
              throw new Error(`GuardError: maxLoopStallIters exceeded (${ctx.guard.maxLoopStallIters})`);
            }
          } else {
            stallCount = 0;
          }
          prevSig = nextSig;
        }
      }
      return last;
    }

    case "ForInStmt": {
      const iterable = evalExpr(stmt.iterable, scope, ctx);
      if (!Array.isArray(iterable)) throw new Error("Runtime error: for-in iterable must be list");
      let last = undefined;
      for (const item of iterable) {
        guardTickLoop(ctx);
        const loopScope = new RuntimeScope(scope);
        loopScope.define(stmt.iterator, item, inferTypeFromValue(item), true, false);
        last = evalBlock(stmt.body, loopScope, ctx);
      }
      return last;
    }

    case "ReturnStmt": {
      const v = stmt.argument ? evalExpr(stmt.argument, scope, ctx) : undefined;
      throw new ReturnSignal(v);
    }

    default:
      throw new Error(`Runtime error: unknown stmt '${stmt.type}'`);
  }
}

function executeProgram(ast, initialVars = {}, options = {}) {
  const strictTypes = options.strictTypes !== false;
  const loggerFn = typeof options.loggerFn === "function" ? options.loggerFn : console.log;
  const maxOutput = Number.isFinite(options.maxOutput) ? Math.max(0, options.maxOutput) : Infinity;
  const globalTypes = normalizeGlobalTypes(options.globalTypes || {});
  const externals = normalizeExternals(options.externals || {});
  const externalTypes = normalizeExternalTypes(options.externalTypes || {});
  const guard = normalizeGuardOptions(options);

  const ctx = {
    strictTypes,
    loggerFn,
    maxOutput,
    output: [],
    externals,
    externalTypes,
    guard: {
      ...guard,
      ops: 0,
      loopIters: 0,
      callDepth: 0,
      calls: 0,
      startedAt: Date.now()
    }
  };
  const globalScope = new RuntimeScope(null);

  for (const [name, spec] of Object.entries(BUILTINS)) {
    globalScope.define(name, makeBuiltinFn(name, spec), spec.type, false, true);
  }

  for (const [name, value] of Object.entries(initialVars || {})) {
    const declared = globalTypes[name] || inferTypeFromValue(value);
    if (strictTypes) assertType(value, declared, `initial var '${name}'`);
    globalScope.define(name, value, declared, true, false);
  }

  let last = undefined;
  for (const stmt of ast.body) {
    guardTickOps(ctx, 1);
    last = evalStmt(stmt, globalScope, ctx);
  }

  return { value: last, vars: globalScope.exportVars(), output: ctx.output, ast };
}

function compile(ast, options = {}) {
  typeCheck(ast, options);
  return (ctx = {}) => executeProgram(ast, ctx.vars || {}, options).value;
}

const PROGRAM_CACHE = new Map();
const FN_CACHE_ID = new WeakMap();
const OBJ_CACHE_ID = new WeakMap();
let FN_CACHE_NEXT = 1;
let OBJ_CACHE_NEXT = 1;

function fnCacheId(fn) {
  if (!fn || typeof fn !== "function") return "nofn";
  if (!FN_CACHE_ID.has(fn)) FN_CACHE_ID.set(fn, FN_CACHE_NEXT++);
  return `fn#${FN_CACHE_ID.get(fn)}`;
}

function objCacheId(obj) {
  if (!obj || typeof obj !== "object") return "noobj";
  if (!OBJ_CACHE_ID.has(obj)) OBJ_CACHE_ID.set(obj, OBJ_CACHE_NEXT++);
  return `obj#${OBJ_CACHE_ID.get(obj)}`;
}

function cacheKey(source, opts) {
  const globalTypes = normalizeGlobalTypes(opts.globalTypes || {});
  const externalTypes = normalizeExternalTypes(opts.externalTypes || {});
  const guard = normalizeGuardOptions(opts);
  const gt = Object.keys(globalTypes)
    .sort()
    .map((k) => `${k}:${typeKey(globalTypes[k])}`)
    .join(",");
  const et = Object.keys(externalTypes)
    .sort()
    .map((k) => `${k}:${typeKey(externalTypes[k])}`)
    .join(",");
  return JSON.stringify({
    source,
    strictTypes: opts.strictTypes !== false,
    maxOutput: Number.isFinite(opts.maxOutput) ? opts.maxOutput : "Infinity",
    loggerFn: fnCacheId(opts.loggerFn),
    globalTypes: gt,
    externalTypes: et,
    externals: objCacheId(opts.externals),
    maxOps: guard.maxOps,
    maxRuntimeMs: guard.maxRuntimeMs,
    maxLoopIters: guard.maxLoopIters,
    maxLoopStallIters: Number.isFinite(guard.maxLoopStallIters) ? guard.maxLoopStallIters : "Infinity",
    maxCallDepth: guard.maxCallDepth,
    maxCalls: guard.maxCalls,
    maxInferenceSteps: Number.isFinite(guard.maxInferenceSteps) ? guard.maxInferenceSteps : "Infinity",
    maxConstraintNodes: Number.isFinite(guard.maxConstraintNodes) ? guard.maxConstraintNodes : "Infinity"
  });
}

function createRaggiraleRunner(source, compileOptions = {}) {
  const ast = parse(source);
  typeCheck(ast, compileOptions);
  return function runner(initialVars = {}) {
    return executeProgram(ast, initialVars, compileOptions);
  };
}

function clearRaggiraleCache() {
  PROGRAM_CACHE.clear();
}

function runRaggirale(source, initialVars = {}, compileOptions = {}) {
  const opts = { ...compileOptions };
  if (!opts.globalTypes) {
    const inferred = {};
    for (const [k, v] of Object.entries(initialVars || {})) inferred[k] = inferTypeFromValue(v);
    opts.globalTypes = inferred;
  }

  const useCache = opts.cache !== false;
  const key = cacheKey(source, opts);
  let runner = null;

  if (useCache && PROGRAM_CACHE.has(key)) runner = PROGRAM_CACHE.get(key);
  else {
    runner = createRaggiraleRunner(source, opts);
    if (useCache) PROGRAM_CACHE.set(key, runner);
  }

  return runner(initialVars);
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = {
    tokenize,
    parse,
    parseTypeString,
    typeCheck,
    compile,
    runRaggirale,
    createRaggiraleRunner,
    clearRaggiraleCache
  };
}

