"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const diagnostics_1 = require("./diagnostics");
const metadata_1 = require("./metadata");
const completion_1 = require("./providers/completion");
const hover_1 = require("./providers/hover");
const signature_1 = require("./providers/signature");
const run_format_1 = require("./run-format");
const runtime_1 = require("./runtime");
const DOCUMENT_SELECTOR = [
    { language: "gaigai", scheme: "file" },
    { language: "gaigai", scheme: "untitled" },
];
function isGaigaiDocument(document) {
    return document.languageId === "gaigai";
}
function activate(context) {
    const output = vscode.window.createOutputChannel("Gaigai");
    context.subscriptions.push(output);
    let runtime = null;
    const getRuntime = () => {
        if (!runtime) {
            runtime = (0, runtime_1.loadGaigaiRuntime)(context.extensionPath);
        }
        return runtime;
    };
    let builtinEntries = [];
    try {
        const metadata = (0, metadata_1.loadBuiltinsMetadata)(context.extensionPath);
        builtinEntries = metadata.builtins.slice().sort((a, b) => a.name.localeCompare(b.name));
    }
    catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        vscode.window.showWarningMessage(`Gaigai: builtin metadata unavailable. ${message}`);
    }
    const builtinIndex = (0, metadata_1.createBuiltinIndex)(builtinEntries);
    let diagnosticsController = null;
    try {
        diagnosticsController = (0, diagnostics_1.registerGaigaiDiagnostics)(context, getRuntime(), builtinIndex);
    }
    catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        vscode.window.showWarningMessage(`Gaigai: diagnostics unavailable. ${message}`);
    }
    context.subscriptions.push(vscode.languages.registerCompletionItemProvider(DOCUMENT_SELECTOR, (0, completion_1.createGaigaiCompletionProvider)(builtinEntries), ...completion_1.COMPLETION_TRIGGER_CHARACTERS), vscode.languages.registerHoverProvider(DOCUMENT_SELECTOR, (0, hover_1.createGaigaiHoverProvider)(builtinIndex)), vscode.languages.registerSignatureHelpProvider(DOCUMENT_SELECTOR, (0, signature_1.createGaigaiSignatureProvider)(builtinIndex), ...signature_1.SIGNATURE_TRIGGER_CHARACTERS));
    const runCommand = vscode.commands.registerCommand("gaigai.runCurrentFile", async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showWarningMessage("Gaigai: no active editor.");
            return;
        }
        const document = editor.document;
        if (!isGaigaiDocument(document)) {
            const pick = await vscode.window.showWarningMessage("Gaigai: active file is not in Gaigai language mode. Run anyway?", "Run", "Cancel");
            if (pick !== "Run") {
                return;
            }
        }
        try {
            const loadedRuntime = getRuntime();
            const source = document.getText();
            const started = performance.now();
            const result = loadedRuntime.runGaigai(source, {}, { strictArity: true, loggerFn: () => undefined });
            const timingMs = performance.now() - started;
            const hasErrors = diagnosticsController !== null &&
                (diagnosticsController.collection.get(document.uri) ?? []).some((diagnostic) => diagnostic.severity === vscode.DiagnosticSeverity.Error);
            output.clear();
            for (const line of (0, run_format_1.formatRunOutput)(document.fileName, result, timingMs, hasErrors)) {
                output.appendLine(line);
            }
            output.show(true);
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            output.appendLine(`Gaigai run failed: ${message}`);
            output.show(true);
            vscode.window.showErrorMessage(`Gaigai run failed: ${message}`);
        }
    });
    context.subscriptions.push(runCommand);
}
function deactivate() {
    // no-op
}
//# sourceMappingURL=extension.js.map