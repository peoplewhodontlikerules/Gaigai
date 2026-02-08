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
  if (isRaigaiFn(v) || typeof v === "function") return tFn([], T_ANY);
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
  if (t.kind === "fn") return isRaigaiFn(v) || typeof v === "function";
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
    }
  };
}

const BUILTINS = makeBuiltins();

function checkExpr(node, scope) {
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
      let inner = checkExpr(node.elements[0], scope);
      for (let i = 1; i < node.elements.length; i++) {
        const it = checkExpr(node.elements[i], scope);
        if (!isSameType(inner, it)) {
          inner = T_ANY;
          break;
        }
      }
      return tList(inner);
    }

    case "UnaryExpr": {
      const arg = checkExpr(node.argument, scope);
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
      const lt = checkExpr(node.left, scope);
      const rt = checkExpr(node.right, scope);
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
      const lt = checkExpr(node.left, scope);
      const rt = checkExpr(node.right, scope);
      if (!isAssignable(lt, T_BOOL) || !isAssignable(rt, T_BOOL)) {
        throw new Error(`Type error: '${node.operator}' requires bool operands`);
      }
      return T_BOOL;
    }

    case "RangeExpr": {
      const lt = checkExpr(node.left, scope);
      const rt = checkExpr(node.right, scope);
      if (!isAssignable(lt, T_NUMBER) || !isAssignable(rt, T_NUMBER)) throw new Error("Type error: '..' requires numbers");
      return tList(T_NUMBER);
    }

    case "IndexExpr": {
      const obj = checkExpr(node.object, scope);
      const idx = checkExpr(node.index, scope);
      if (!isAssignable(idx, T_NUMBER)) throw new Error("Type error: index must be number");
      if (obj.kind !== "list" && !isAnyType(obj)) throw new Error("Type error: indexing requires list");
      return obj.kind === "list" ? obj.inner : T_ANY;
    }

    case "AssignExpr": {
      const target = scope.get(node.target.name);
      if (!target) throw new Error(`Type error: unknown variable '${node.target.name}'`);
      if (!target.mutable) throw new Error(`Type error: cannot assign to const '${node.target.name}'`);
      const rhs = checkExpr(node.value, scope);
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
      const callee = checkExpr(node.callee, scope);
      if (callee.kind !== "fn" && !isAnyType(callee)) throw new Error("Type error: callee is not callable");
      const args = node.args.map((a) => checkExpr(a, scope));
      if (callee.kind === "fn") {
        if (args.length !== callee.params.length) {
          throw new Error(`Type error: function expects ${callee.params.length} args, got ${args.length}`);
        }
        for (let i = 0; i < args.length; i++) {
          if (!isAssignable(args[i], callee.params[i])) {
            throw new Error(`Type error: arg ${i + 1} expects ${typeStr(callee.params[i])}, got ${typeStr(args[i])}`);
          }
        }
        return callee.ret;
      }
      return T_ANY;
    }

    case "PipeExpr": {
      const leftType = checkExpr(node.left, scope);
      if (node.right.type === "Identifier") {
        const fnType = checkExpr(node.right, scope);
        if (fnType.kind !== "fn") throw new Error("Type error: right side of '|>' must be function");
        if (fnType.params.length < 1) throw new Error("Type error: piped function needs at least 1 param");
        if (!isAssignable(leftType, fnType.params[0])) throw new Error("Type error: piped value type mismatch");
        return fnType.ret;
      }
      if (node.right.type === "CallExpr") {
        const fnType = checkExpr(node.right.callee, scope);
        if (fnType.kind !== "fn") throw new Error("Type error: right side of '|>' must be function call");
        const rest = node.right.args.map((a) => checkExpr(a, scope));
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

function checkBlock(block, scope, ctx) {
  let last = T_VOID;
  for (const stmt of block.body) last = checkStmt(stmt, scope, ctx);
  return last;
}

function checkStmt(stmt, scope, ctx) {
  switch (stmt.type) {
    case "LetDecl": {
      const initType = checkExpr(stmt.init, scope);
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
        const bodyType = checkExpr(stmt.body, fnScope);
        if (!isAssignable(bodyType, fnType.ret)) {
          throw new Error(`Type error: function '${stmt.name}' returns ${typeStr(bodyType)}, expected ${typeStr(fnType.ret)}`);
        }
      } else {
        checkBlock(stmt.body, fnScope, fnCtx);
      }
      return T_VOID;
    }

    case "BlockStmt":
      return checkBlock(stmt, new TypeScope(scope), ctx);

    case "IfStmt": {
      const testType = checkExpr(stmt.test, scope);
      if (!isAssignable(testType, T_BOOL)) throw new Error("Type error: if condition must be bool");
      checkBlock(stmt.consequent, new TypeScope(scope), ctx);
      if (stmt.alternate) {
        if (stmt.alternate.type === "IfStmt") checkStmt(stmt.alternate, new TypeScope(scope), ctx);
        else checkBlock(stmt.alternate, new TypeScope(scope), ctx);
      }
      return T_VOID;
    }

    case "WhileStmt": {
      const testType = checkExpr(stmt.test, scope);
      if (!isAssignable(testType, T_BOOL)) throw new Error("Type error: while condition must be bool");
      checkBlock(stmt.body, new TypeScope(scope), ctx);
      return T_VOID;
    }

    case "ForInStmt": {
      const iterType = checkExpr(stmt.iterable, scope);
      if (iterType.kind !== "list" && !isAnyType(iterType)) throw new Error("Type error: for-in iterable must be list");
      const forScope = new TypeScope(scope);
      forScope.define(stmt.iterator, iterType.kind === "list" ? iterType.inner : T_ANY, true);
      checkBlock(stmt.body, forScope, ctx);
      return T_VOID;
    }

    case "ReturnStmt": {
      if (!ctx.inFunction) throw new Error("Type error: return can only be used inside function");
      const actual = stmt.argument ? checkExpr(stmt.argument, scope) : T_VOID;
      if (!isAssignable(actual, ctx.returnType)) {
        throw new Error(`Type error: return expects ${typeStr(ctx.returnType)}, got ${typeStr(actual)}`);
      }
      return ctx.returnType;
    }

    case "ExprStmt":
      return checkExpr(stmt.expression, scope);

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

function typeCheck(ast, options = {}) {
  const scope = new TypeScope(null);
  for (const [name, spec] of Object.entries(BUILTINS)) scope.define(name, spec.type, false);
  const globalTypes = normalizeGlobalTypes(options.globalTypes || {});
  for (const [name, type] of Object.entries(globalTypes)) scope.define(name, type, true);

  registerFnSignatures(ast, scope);
  const ctx = { inFunction: false, returnType: T_VOID };
  let last = T_VOID;
  for (const stmt of ast.body) last = checkStmt(stmt, scope, ctx);
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

function isRaigaiFn(v) {
  return !!(v && typeof v === "object" && v.__raigaiFn === true);
}

function makeBuiltinFn(name, spec) {
  return {
    __raigaiFn: true,
    name,
    params: spec.type.params,
    ret: spec.type.ret,
    call(args, ctx) {
      return spec.fn(args, ctx);
    }
  };
}

function callFn(fn, args, ctx) {
  if (isRaigaiFn(fn)) {
    if (ctx.strictTypes) {
      if (args.length !== fn.params.length) {
        throw new Error(`Runtime error: function '${fn.name}' expects ${fn.params.length} args, got ${args.length}`);
      }
      for (let i = 0; i < args.length; i++) assertType(args[i], fn.params[i], `arg ${i + 1} of '${fn.name}'`);
    }
    const ret = fn.call(args, ctx);
    if (ctx.strictTypes) assertType(ret, fn.ret, `return value of '${fn.name}'`);
    return ret;
  }
  if (typeof fn === "function") return fn(...args);
  throw new Error("Runtime error: value is not callable");
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
      const callee = evalExpr(node.callee, scope, ctx);
      const args = node.args.map((a) => evalExpr(a, scope, ctx));
      return callFn(callee, args, ctx);
    }

    case "PipeExpr": {
      const v = evalExpr(node.left, scope, ctx);
      if (node.right.type === "Identifier") {
        const fn = evalExpr(node.right, scope, ctx);
        return callFn(fn, [v], ctx);
      }
      if (node.right.type === "CallExpr") {
        const fn = evalExpr(node.right.callee, scope, ctx);
        const args = node.right.args.map((a) => evalExpr(a, scope, ctx));
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
  for (const stmt of block.body) last = evalStmt(stmt, scope, ctx);
  return last;
}

function makeUserFn(decl, closure) {
  return {
    __raigaiFn: true,
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
      while (evalExpr(stmt.test, scope, ctx)) last = evalBlock(stmt.body, scope, ctx);
      return last;
    }

    case "ForInStmt": {
      const iterable = evalExpr(stmt.iterable, scope, ctx);
      if (!Array.isArray(iterable)) throw new Error("Runtime error: for-in iterable must be list");
      let last = undefined;
      for (const item of iterable) {
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

  const ctx = { strictTypes, loggerFn, maxOutput, output: [] };
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
  for (const stmt of ast.body) last = evalStmt(stmt, globalScope, ctx);

  return { value: last, vars: globalScope.exportVars(), output: ctx.output, ast };
}

function compile(ast, options = {}) {
  typeCheck(ast, options);
  return (ctx = {}) => executeProgram(ast, ctx.vars || {}, options).value;
}

const PROGRAM_CACHE = new Map();
const FN_CACHE_ID = new WeakMap();
let FN_CACHE_NEXT = 1;

function fnCacheId(fn) {
  if (!fn || typeof fn !== "function") return "nofn";
  if (!FN_CACHE_ID.has(fn)) FN_CACHE_ID.set(fn, FN_CACHE_NEXT++);
  return `fn#${FN_CACHE_ID.get(fn)}`;
}

function cacheKey(source, opts) {
  const globalTypes = normalizeGlobalTypes(opts.globalTypes || {});
  const gt = Object.keys(globalTypes)
    .sort()
    .map((k) => `${k}:${typeKey(globalTypes[k])}`)
    .join(",");
  return JSON.stringify({
    source,
    strictTypes: opts.strictTypes !== false,
    maxOutput: Number.isFinite(opts.maxOutput) ? opts.maxOutput : "Infinity",
    loggerFn: fnCacheId(opts.loggerFn),
    globalTypes: gt
  });
}

function createRaigaiRunner(source, compileOptions = {}) {
  const ast = parse(source);
  typeCheck(ast, compileOptions);
  return function runner(initialVars = {}) {
    return executeProgram(ast, initialVars, compileOptions);
  };
}

function clearRaigaiCache() {
  PROGRAM_CACHE.clear();
}

function runRaigai(source, initialVars = {}, compileOptions = {}) {
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
    runner = createRaigaiRunner(source, opts);
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
    runRaigai,
    createRaigaiRunner,
    clearRaigaiCache
  };
}
