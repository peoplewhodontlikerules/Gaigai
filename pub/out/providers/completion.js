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
exports.COMPLETION_TRIGGER_CHARACTERS = void 0;
exports.createGaigaiCompletionProvider = createGaigaiCompletionProvider;
const vscode = __importStar(require("vscode"));
const intellisense_data_1 = require("../intellisense-data");
const IDENTIFIER_PART_RE = /[A-Za-z_][A-Za-z0-9_]*/;
const LETTER_TRIGGERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".split("");
exports.COMPLETION_TRIGGER_CHARACTERS = ["(", ...LETTER_TRIGGERS];
function createGaigaiCompletionProvider(builtins) {
    return {
        provideCompletionItems(document, position) {
            const wordRange = document.getWordRangeAtPosition(position, IDENTIFIER_PART_RE);
            const activeRange = wordRange ? new vscode.Range(wordRange.start, position) : undefined;
            const prefix = activeRange ? document.getText(activeRange) : "";
            const candidates = prefix.length > 0
                ? builtins.filter((item) => item.name.startsWith(prefix))
                : builtins;
            return candidates.map((entry) => {
                const item = new vscode.CompletionItem(entry.name, vscode.CompletionItemKind.Function);
                item.insertText = new vscode.SnippetString((0, intellisense_data_1.completionSnippetForBuiltin)(entry));
                item.detail = `${entry.name}(${entry.arityText})`;
                item.documentation = new vscode.MarkdownString(`category: \`${entry.category}\`\n\nsource: gaigai.js generated metadata`);
                item.sortText = `0_${entry.name}`;
                item.filterText = entry.name;
                item.range = activeRange;
                item.commitCharacters = ["("];
                return item;
            });
        },
    };
}
//# sourceMappingURL=completion.js.map