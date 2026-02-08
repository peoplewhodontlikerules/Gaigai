"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseSyntaxErrorLocation = parseSyntaxErrorLocation;
exports.analyzeGaigaiSource = analyzeGaigaiSource;
const SYNTAX_LOCATION_RE = /line\s+(\d+)\s*,\s*col\s+(\d+)/i;
function createLineStarts(source) {
    const starts = [0];
    for (let i = 0; i < source.length; i++) {
        if (source.charCodeAt(i) === 10) {
            starts.push(i + 1);
        }
    }
    return starts;
}
function lineColToOffset(source, line, col) {
    const lineStarts = createLineStarts(source);
    const lineIndex = Math.max(0, line - 1);
    if (lineIndex >= lineStarts.length) {
        return source.length;
    }
    const base = lineStarts[lineIndex];
    return Math.max(0, Math.min(source.length, base + Math.max(0, col - 1)));
}
function parseSyntaxErrorLocation(errorText) {
    const match = errorText.match(SYNTAX_LOCATION_RE);
    if (!match) {
        return null;
    }
    const line = Number(match[1]);
    const col = Number(match[2]);
    if (!Number.isInteger(line) || !Number.isInteger(col)) {
        return null;
    }
    return { line, col };
}
function collectCallSites(node, out) {
    if (!node || typeof node !== "object") {
        return;
    }
    if (node.type === "Call") {
        const args = Array.isArray(node.args) ? node.args : [];
        out.push({
            name: typeof node.name === "string" ? node.name : "",
            argCount: args.length,
            pos: typeof node.pos === "number" ? node.pos : 0,
        });
        for (const arg of args) {
            collectCallSites(arg, out);
        }
        return;
    }
    if (node.type === "Tuple") {
        const items = Array.isArray(node.items) ? node.items : [];
        for (const item of items) {
            collectCallSites(item, out);
        }
    }
}
function normalizeRange(sourceLength, startOffset, endOffset) {
    const start = Math.max(0, Math.min(sourceLength, startOffset));
    const rawEnd = Math.max(start + 1, endOffset);
    const end = Math.max(0, Math.min(sourceLength, rawEnd));
    return { start, end: end > start ? end : Math.min(sourceLength, start + 1) };
}
function analyzeGaigaiSource(source, runtime, builtins) {
    const diagnostics = [];
    let ast;
    try {
        ast = runtime.parse(source);
    }
    catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        const location = parseSyntaxErrorLocation(message);
        const offset = location ? lineColToOffset(source, location.line, location.col) : 0;
        const range = normalizeRange(source.length, offset, offset + 1);
        diagnostics.push({
            code: "gaigai.syntax",
            severity: "error",
            message,
            startOffset: range.start,
            endOffset: range.end,
        });
        return { diagnostics };
    }
    const calls = [];
    collectCallSites(ast, calls);
    for (const call of calls) {
        if (!call.name) {
            continue;
        }
        const start = call.pos;
        const end = call.pos + call.name.length;
        const range = normalizeRange(source.length, start, end);
        const meta = builtins.get(call.name);
        if (!meta) {
            diagnostics.push({
                code: "gaigai.unknownFunction",
                severity: "error",
                message: `Unknown function '${call.name}'.`,
                startOffset: range.start,
                endOffset: range.end,
            });
            continue;
        }
        if (meta.minArity === null || meta.maxArity === null) {
            continue;
        }
        const min = meta.minArity;
        const max = meta.maxArity === "Infinity" ? Number.POSITIVE_INFINITY : meta.maxArity;
        if (call.argCount < min || call.argCount > max) {
            diagnostics.push({
                code: "gaigai.arity",
                severity: "error",
                message: `Arity mismatch for '${call.name}': expected ${meta.arityText}, got ${call.argCount}.`,
                startOffset: range.start,
                endOffset: range.end,
            });
        }
    }
    return { diagnostics };
}
//# sourceMappingURL=analysis.js.map