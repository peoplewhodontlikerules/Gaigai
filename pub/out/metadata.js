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
exports.loadBuiltinsMetadata = loadBuiltinsMetadata;
exports.createBuiltinIndex = createBuiltinIndex;
exports.maxArityToNumber = maxArityToNumber;
exports.getArityLabel = getArityLabel;
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
let cachedMetadataPath = null;
let cachedMetadata = null;
function loadBuiltinsMetadata(extensionPath) {
    const metadataPath = path.join(extensionPath, "src", "generated", "builtins.json");
    if (cachedMetadata && cachedMetadataPath === metadataPath) {
        return cachedMetadata;
    }
    if (!fs.existsSync(metadataPath)) {
        throw new Error(`Missing builtins metadata: ${metadataPath}`);
    }
    const text = fs.readFileSync(metadataPath, "utf8");
    const parsed = JSON.parse(text);
    if (!parsed || !Array.isArray(parsed.builtins)) {
        throw new Error(`Invalid builtins metadata format: ${metadataPath}`);
    }
    cachedMetadataPath = metadataPath;
    cachedMetadata = parsed;
    return parsed;
}
function createBuiltinIndex(entries) {
    const index = new Map();
    for (const entry of entries) {
        index.set(entry.name, entry);
    }
    return index;
}
function maxArityToNumber(entry) {
    if (entry.maxArity === null) {
        return null;
    }
    if (entry.maxArity === "Infinity") {
        return Number.POSITIVE_INFINITY;
    }
    return entry.maxArity;
}
function getArityLabel(entry) {
    return `${entry.name}(${entry.arityText})`;
}
//# sourceMappingURL=metadata.js.map