% The MIT License (MIT)
% Copyright © 2022-2025 TS-Solution Corp.
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to all conditions.
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

classdef TSANPR < handle
    % TSANPR MATLAB/Octave wrapper class
    % Provides interface to the native TSANPR library using MEX functions.
    % Supports both MATLAB and GNU Octave.

    properties (Access = private)
        library_path
        is_loaded
    end

    methods
        function obj = TSANPR(library_path)
            % Initialize TSANPR with the given library path.
            %
            % Args:
            %   library_path: Path to the TSANPR library

            if ~exist(library_path, 'file')
                error('Library file not found: %s', library_path);
            end

            obj.library_path = library_path;
            obj.is_loaded = false;

            % Check if MEX file exists
            if ~obj.mex_exists()
                error(['MEX file not found. Please build it first:\n' ...
                       '  MATLAB: run build_mex.m\n' ...
                       '  Octave: run build_mex.m or mkoctfile --mex tsanpr_mex.c']);
            end

            % Load the library via MEX
            err_msg = tsanpr_mex('load_library', library_path);
            if ~isempty(err_msg)
                error('Failed to load library: %s', err_msg);
            end

            obj.is_loaded = true;
        end

        function delete(obj)
            % Destructor - unload library when object is destroyed
            if obj.is_loaded
                tsanpr_mex('unload_library');
                obj.is_loaded = false;
            end
        end

        function result = anpr_initialize(obj, mode)
            % Initialize the ANPR engine with the specified mode.
            %
            % Args:
            %   mode: Initialization mode string (e.g., "text;country=KR")
            %
            % Returns:
            %   Error message if initialization failed, empty string if successful

            if ~obj.is_loaded
                result = 'Library not loaded';
                return;
            end

            result = tsanpr_mex('initialize', mode);
        end

        function result = anpr_read_file(obj, img_file_name, output_format, options)
            % Read and process an image file.
            %
            % Args:
            %   img_file_name: Path to the image file
            %   output_format: Output format (text, json, yaml, xml, csv)
            %   options: Processing options
            %
            % Returns:
            %   Recognition result as string

            if ~obj.is_loaded
                result = 'Library not loaded';
                return;
            end

            result = tsanpr_mex('read_file', img_file_name, output_format, options);
        end

        function result = anpr_read_pixels(obj, pixels, width, height, stride, ...
                                         pixel_format, output_format, options)
            % Process pixel data directly.
            %
            % Args:
            %   pixels: Pixel data as uint8 array
            %   width: Image width
            %   height: Image height
            %   stride: Row stride in bytes
            %   pixel_format: Pixel format (BGR, BGRA, GRAY, encoded, etc.)
            %   output_format: Output format (text, json, yaml, xml, csv)
            %   options: Processing options
            %
            % Returns:
            %   Recognition result as string

            if ~obj.is_loaded
                result = 'Library not loaded';
                return;
            end

            % Ensure pixels is uint8
            if ~isa(pixels, 'uint8')
                pixels = uint8(pixels);
            end

            % Ensure pixels is a column vector
            pixels = pixels(:);

            result = tsanpr_mex('read_pixels', pixels, ...
                              double(width), double(height), double(stride), ...
                              pixel_format, output_format, options);
        end
    end

    methods (Access = private)
        function exists = mex_exists(obj)
            % Check if MEX file exists
            mex_name = 'tsanpr_mex';

            % Get MEX extension based on platform
            if ispc
                if exist('OCTAVE_VERSION', 'builtin')
                    ext = '.mex';
                else
                    ext = '.mexw64';
                    if strcmp(computer('arch'), 'win32')
                        ext = '.mexw32';
                    end
                end
            elseif ismac
                ext = '.mexmaci64';
            else
                if exist('OCTAVE_VERSION', 'builtin')
                    ext = '.mex';
                else
                    ext = '.mexa64';
                end
            end

            % Check if MEX file exists in current directory or path
            mex_file = [mex_name ext];
            exists = exist(mex_file, 'file') == 3 || exist(mex_name, 'file') == 3;
        end
    end

    methods (Static)
        function is_octave = isOctave()
            % Check if running in GNU Octave
            is_octave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
        end
    end
end
