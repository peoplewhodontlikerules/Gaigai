"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.formatRunOutput = formatRunOutput;
function formatJson(value) {
    try {
        return JSON.stringify(value, null, 2);
    }
    catch {
        return String(value);
    }
}
function formatRunOutput(filePath, result, timingMs, hasDiagnosticsErrors) {
    const lines = [];
    if (hasDiagnosticsErrors) {
        lines.push("warning: diagnostics contain errors; execution may fail.");
    }
    lines.push(`file: ${filePath}`);
    lines.push("");
    lines.push("value:");
    lines.push(formatJson(result.value));
    lines.push("");
    lines.push("output:");
    lines.push(formatJson(result.output));
    lines.push("");
    lines.push("vars:");
    lines.push(formatJson(result.vars));
    lines.push("");
    lines.push(`timing(ms): ${timingMs.toFixed(3)}`);
    return lines;
}
//# sourceMappingURL=run-format.js.map