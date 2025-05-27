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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

const path = require('path');
const fs = require('fs');

/**
 * Dynamically resolve the path to the native addon (.node file).
 * 1. Packaged (pkg) or production: same directory as executable.
 * 2. Development: typical build output directory.
 * 3. Fallback: current working directory.
 */
function getAddonPath() {
  // 1. Packaged/production: same directory as executable
  const exeDir = path.dirname(process.execPath);
  let candidate = path.join(exeDir, 'tsanpr-addon.node');
  if (fs.existsSync(candidate)) return candidate;

  // 2. Development: build output directory (relative to this JS file)
  candidate = path.join(__dirname, '../build/Release/tsanpr-addon.node');
  if (fs.existsSync(candidate)) return candidate;

  // 3. Fallback: current working directory
  candidate = path.join(process.cwd(), 'tsanpr-addon.node');
  if (fs.existsSync(candidate)) return candidate;

  throw new Error('Cannot find tsanpr-addon.node');
}

// Load the native addon
const addon = require(getAddonPath());

/**
 * Loads the TSANPR engine (DLL/SO) and returns an interface to its functions.
 * @param {string} enginePath Path to tsanpr.dll or libtsanpr.so
 * @returns {object} TSANPR interface
 */
function loadTSANPR(enginePath) {
  if (!addon.loadLibrary(enginePath)) {
    throw new Error('Failed to load TSANPR engine');
  }
  return {
    anpr_initialize: addon.anpr_initialize,
    anpr_read_file: addon.anpr_read_file,
    anpr_read_pixels: addon.anpr_read_pixels,
  };
}

module.exports = { loadTSANPR };
