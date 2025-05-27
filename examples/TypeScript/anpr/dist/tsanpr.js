"use strict";
/**
 * The MIT License (MIT)
 * Copyright © 2022-2025 TS-Solution Corp.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to all conditions.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.loadTSANPR = loadTSANPR;
const path_1 = __importDefault(require("path"));
const fs_1 = __importDefault(require("fs"));
function getAddonPath() {
    // 1. Packaged/production: same directory as executable
    const exeDir = path_1.default.dirname(process.execPath);
    let candidate = path_1.default.join(exeDir, 'tsanpr-addon.node');
    if (fs_1.default.existsSync(candidate))
        return candidate;
    // 2. Development: build output directory (relative to this JS file)
    candidate = path_1.default.join(__dirname, '../build/Release/tsanpr-addon.node');
    if (fs_1.default.existsSync(candidate))
        return candidate;
    // 3. Fallback: current working directory
    candidate = path_1.default.join(process.cwd(), 'tsanpr-addon.node');
    if (fs_1.default.existsSync(candidate))
        return candidate;
    throw new Error('Cannot find tsanpr-addon.node');
}
const addon = require(getAddonPath());
function loadTSANPR(enginePath) {
    if (!addon.loadLibrary(enginePath)) {
        throw new Error("Failed to load TSANPR engine");
    }
    return {
        anpr_initialize: addon.anpr_initialize,
        anpr_read_file: addon.anpr_read_file,
        anpr_read_pixels: addon.anpr_read_pixels,
    };
}
