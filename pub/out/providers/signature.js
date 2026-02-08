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
exports.SIGNATURE_TRIGGER_CHARACTERS = void 0;
exports.createGaigaiSignatureProvider = createGaigaiSignatureProvider;
const vscode = __importStar(require("vscode"));
const intellisense_data_1 = require("../intellisense-data");
exports.SIGNATURE_TRIGGER_CHARACTERS = ["(", ","];
function createGaigaiSignatureProvider(index) {
    return {
        provideSignatureHelp(document, position) {
            const source = document.getText();
            const offset = document.offsetAt(position);
            const context = (0, intellisense_data_1.findActiveCallContext)(source, offset);
            if (!context) {
                return null;
            }
            const entry = index.get(context.name);
            if (!entry) {
                return null;
            }
            const signature = new vscode.SignatureInformation((0, intellisense_data_1.signatureLabelForBuiltin)(entry));
            const parameterLabels = (0, intellisense_data_1.parameterLabelsForBuiltin)(entry);
            signature.parameters = parameterLabels.map((label) => new vscode.ParameterInformation(label));
            signature.documentation = `category: ${entry.category} | source: gaigai.js generated metadata`;
            const help = new vscode.SignatureHelp();
            help.signatures = [signature];
            help.activeSignature = 0;
            help.activeParameter = Math.min(context.activeParameter, Math.max(0, parameterLabels.length - 1));
            return help;
        },
    };
}
//# sourceMappingURL=signature.js.map