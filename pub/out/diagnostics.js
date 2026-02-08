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
exports.registerGaigaiDiagnostics = registerGaigaiDiagnostics;
const vscode = __importStar(require("vscode"));
const analysis_1 = require("./analysis");
const DEBOUNCE_MS = 150;
function toSeverity(level) {
    return level === "error" ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning;
}
function toRange(document, startOffset, endOffset) {
    const textLength = document.getText().length;
    const safeStart = Math.max(0, Math.min(textLength, startOffset));
    const safeEnd = Math.max(safeStart + 1, Math.min(textLength, endOffset));
    return new vscode.Range(document.positionAt(safeStart), document.positionAt(safeEnd));
}
function buildDiagnostics(document, runtime, builtinIndex) {
    const source = document.getText();
    const result = (0, analysis_1.analyzeGaigaiSource)(source, runtime, builtinIndex);
    return result.diagnostics.map((item) => {
        const diagnostic = new vscode.Diagnostic(toRange(document, item.startOffset, item.endOffset), item.message, toSeverity(item.severity));
        diagnostic.code = item.code;
        diagnostic.source = "gaigai";
        return diagnostic;
    });
}
function registerGaigaiDiagnostics(context, runtime, builtinIndex) {
    const collection = vscode.languages.createDiagnosticCollection("gaigai");
    context.subscriptions.push(collection);
    const pending = new Map();
    const refresh = (document) => {
        if (document.languageId !== "gaigai") {
            return;
        }
        const diagnostics = buildDiagnostics(document, runtime, builtinIndex);
        collection.set(document.uri, diagnostics);
    };
    const queueRefresh = (document) => {
        if (document.languageId !== "gaigai") {
            return;
        }
        const key = document.uri.toString();
        const timer = pending.get(key);
        if (timer) {
            clearTimeout(timer);
        }
        pending.set(key, setTimeout(() => {
            pending.delete(key);
            refresh(document);
        }, DEBOUNCE_MS));
    };
    const clearQueued = (uri) => {
        const key = uri.toString();
        const timer = pending.get(key);
        if (timer) {
            clearTimeout(timer);
            pending.delete(key);
        }
    };
    for (const document of vscode.workspace.textDocuments) {
        if (document.languageId === "gaigai") {
            queueRefresh(document);
        }
    }
    context.subscriptions.push(vscode.workspace.onDidOpenTextDocument((document) => queueRefresh(document)), vscode.workspace.onDidChangeTextDocument((event) => queueRefresh(event.document)), vscode.workspace.onDidSaveTextDocument((document) => queueRefresh(document)), vscode.workspace.onDidCloseTextDocument((document) => {
        clearQueued(document.uri);
        collection.delete(document.uri);
    }));
    return { collection };
}
//# sourceMappingURL=diagnostics.js.map