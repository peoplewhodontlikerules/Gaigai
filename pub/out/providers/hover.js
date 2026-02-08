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
exports.createGaigaiHoverProvider = createGaigaiHoverProvider;
const vscode = __importStar(require("vscode"));
const intellisense_data_1 = require("../intellisense-data");
const IDENTIFIER_RE = /[A-Za-z_][A-Za-z0-9_]*/;
function createGaigaiHoverProvider(index) {
    return {
        provideHover(document, position) {
            const range = document.getWordRangeAtPosition(position, IDENTIFIER_RE);
            if (!range) {
                return null;
            }
            const name = document.getText(range);
            const entry = index.get(name);
            if (!entry) {
                return null;
            }
            const markdown = new vscode.MarkdownString();
            markdown.appendCodeblock((0, intellisense_data_1.signatureLabelForBuiltin)(entry), "gaigai");
            markdown.appendMarkdown(`\n\ncategory: \`${entry.category}\``);
            markdown.appendMarkdown("\n\nsource: gaigai.js generated metadata");
            markdown.isTrusted = false;
            return new vscode.Hover(markdown, range);
        },
    };
}
//# sourceMappingURL=hover.js.map