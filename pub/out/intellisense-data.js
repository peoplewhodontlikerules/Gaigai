"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.completionSnippetForBuiltin = completionSnippetForBuiltin;
exports.parameterLabelsForBuiltin = parameterLabelsForBuiltin;
exports.signatureLabelForBuiltin = signatureLabelForBuiltin;
exports.hoverTextForBuiltin = hoverTextForBuiltin;
exports.findActiveCallContext = findActiveCallContext;
const IDENTIFIER_RE = /[A-Za-z0-9_]/;
const IDENTIFIER_START_RE = /[A-Za-z_]/;
function completionSnippetForBuiltin(entry, maxPlaceholders = 4) {
    if (entry.minArity === null || entry.maxArity === null) {
        return `${entry.name}($1)`;
    }
    if (entry.minArity === 0 && entry.maxArity === 0) {
        return `${entry.name}()`;
    }
    let placeholderCount = entry.minArity;
    if (placeholderCount <= 0) {
        placeholderCount = entry.maxArity === "Infinity" ? 1 : Math.min(entry.maxArity, 1);
    }
    placeholderCount = Math.max(1, Math.min(maxPlaceholders, placeholderCount));
    const args = [];
    for (let i = 1; i <= placeholderCount; i++) {
        args.push(`\${${i}:arg${i}}`);
    }
    return `${entry.name}(${args.join(", ")})`;
}
function parameterLabelsForBuiltin(entry, maxParameters = 6) {
    if (entry.minArity === null || entry.maxArity === null) {
        return ["arg1"];
    }
    const labels = [];
    const min = entry.minArity;
    const finiteMax = entry.maxArity === "Infinity" ? null : entry.maxArity;
    if (finiteMax !== null && min === finiteMax) {
        for (let i = 1; i <= min; i++) {
            labels.push(`arg${i}`);
        }
        return labels;
    }
    const guaranteed = Math.max(1, min);
    for (let i = 1; i <= guaranteed && labels.length < maxParameters; i++) {
        labels.push(`arg${i}`);
    }
    if (finiteMax === null) {
        labels.push("...rest");
        return labels;
    }
    for (let i = guaranteed + 1; i <= finiteMax && labels.length < maxParameters; i++) {
        labels.push(`arg${i}?`);
    }
    return labels;
}
function signatureLabelForBuiltin(entry) {
    const labels = parameterLabelsForBuiltin(entry);
    return `${entry.name}(${labels.join(", ")})`;
}
function hoverTextForBuiltin(entry) {
    return `${signatureLabelForBuiltin(entry)}\ncategory: ${entry.category}\nsource: gaigai.js generated metadata`;
}
function extractIdentifierBefore(source, openParenOffset) {
    let i = openParenOffset - 1;
    while (i >= 0 && /\s/.test(source[i])) {
        i -= 1;
    }
    const end = i + 1;
    while (i >= 0 && IDENTIFIER_RE.test(source[i])) {
        i -= 1;
    }
    const start = i + 1;
    if (start >= end) {
        return null;
    }
    const name = source.slice(start, end);
    if (!IDENTIFIER_START_RE.test(name[0])) {
        return null;
    }
    return name;
}
function findActiveCallContext(source, offset) {
    const limit = Math.max(0, Math.min(source.length, offset));
    const stack = [];
    let inSingle = false;
    let inDouble = false;
    let inLineComment = false;
    let inBlockComment = false;
    let escaped = false;
    for (let i = 0; i < limit; i++) {
        const ch = source[i];
        const next = source[i + 1] ?? "";
        if (inLineComment) {
            if (ch === "\n") {
                inLineComment = false;
            }
            continue;
        }
        if (inBlockComment) {
            if (ch === "*" && next === "/") {
                inBlockComment = false;
                i += 1;
            }
            continue;
        }
        if (inSingle) {
            if (escaped) {
                escaped = false;
            }
            else if (ch === "\\") {
                escaped = true;
            }
            else if (ch === "'") {
                inSingle = false;
            }
            continue;
        }
        if (inDouble) {
            if (escaped) {
                escaped = false;
            }
            else if (ch === "\\") {
                escaped = true;
            }
            else if (ch === '"') {
                inDouble = false;
            }
            continue;
        }
        if (ch === "/" && next === "/") {
            inLineComment = true;
            i += 1;
            continue;
        }
        if (ch === "/" && next === "*") {
            inBlockComment = true;
            i += 1;
            continue;
        }
        if (ch === "'") {
            inSingle = true;
            continue;
        }
        if (ch === '"') {
            inDouble = true;
            continue;
        }
        if (ch === "(") {
            stack.push({
                name: extractIdentifierBefore(source, i),
                commaCount: 0,
                openParenOffset: i,
            });
            continue;
        }
        if (ch === ")") {
            stack.pop();
            continue;
        }
        if (ch === "," && stack.length > 0) {
            stack[stack.length - 1].commaCount += 1;
        }
    }
    for (let i = stack.length - 1; i >= 0; i--) {
        const frame = stack[i];
        if (frame.name) {
            return {
                name: frame.name,
                activeParameter: frame.commaCount,
                openParenOffset: frame.openParenOffset,
            };
        }
    }
    return null;
}
//# sourceMappingURL=intellisense-data.js.map