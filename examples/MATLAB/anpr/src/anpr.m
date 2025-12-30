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

function anpr()
    % Main ANPR example function
    % Works with both MATLAB and GNU Octave

    % Print environment info
    if exist('OCTAVE_VERSION', 'builtin')
        fprintf('Running on GNU Octave %s\n', OCTAVE_VERSION);
    else
        fprintf('Running on MATLAB %s\n', version);
    end

    % Get script directory (works for both MATLAB and Octave)
    script_dir = fileparts(mfilename('fullpath'));
    if isempty(script_dir)
        script_dir = pwd;
    end

    % MEX directory is inside src/
    mex_dir = fullfile(script_dir, 'mex');

    % Add mex directory to path
    addpath(mex_dir);

    % Define examples base directory (src -> anpr -> MATLAB -> examples)
    % Use cd/pwd to resolve to absolute path
    old_dir = pwd;
    cd(fullfile(script_dir, '..', '..', '..'));
    EXAMPLES_BASE_DIR = pwd;
    cd(old_dir);

    % Check if MEX file is built
    if ~check_mex_exists(mex_dir)
        fprintf('\nMEX file not found. Building...\n');
        old_dir = pwd;
        cd(mex_dir);
        try
            build_mex();
        catch ME
            cd(old_dir);
            error('Failed to build MEX file: %s', ME.message);
        end
        cd(old_dir);
    end

    % Get engine file name based on platform
    engine_file_name = get_engine_file_name(EXAMPLES_BASE_DIR);

    if isempty(engine_file_name) || ~exist(engine_file_name, 'file')
        fprintf('Engine file not found: %s\n', engine_file_name);
        fprintf('\nPlease extract the engine files first.\n');
        fprintf('See README.md for instructions.\n');
        return;
    end

    try
        % Initialize TSANPR
        tsanpr = TSANPR(engine_file_name);

        % TODO: Try each country code as needed
        read_license_plates(tsanpr, 'KR', EXAMPLES_BASE_DIR);
        % read_license_plates(tsanpr, 'JP', EXAMPLES_BASE_DIR);
        % read_license_plates(tsanpr, 'VN', EXAMPLES_BASE_DIR);

    catch ME
        fprintf('TSANPR initialization failed: %s\n', ME.message);
    end
end

function exists = check_mex_exists(mex_dir)
    % Check if MEX file exists in the mex directory
    if exist('OCTAVE_VERSION', 'builtin')
        % Octave
        mex_file = fullfile(mex_dir, 'tsanpr_mex.mex');
    elseif ispc
        % MATLAB Windows
        if strcmp(computer('arch'), 'win64')
            mex_file = fullfile(mex_dir, 'tsanpr_mex.mexw64');
        else
            mex_file = fullfile(mex_dir, 'tsanpr_mex.mexw32');
        end
    elseif ismac
        % MATLAB macOS
        mex_file = fullfile(mex_dir, 'tsanpr_mex.mexmaci64');
    else
        % MATLAB Linux
        mex_file = fullfile(mex_dir, 'tsanpr_mex.mexa64');
    end

    exists = exist(mex_file, 'file') == 3;
end

function engine_file_name = get_engine_file_name(examples_base_dir)
    % Generate engine filename depending on platform and architecture.

    if ispc
        % Windows platform
        % Check for 64-bit: MATLAB returns 'win64', Octave returns 'mingw32-x86_64'
        arch = computer('arch');
        if strcmp(arch, 'win64') || ~isempty(strfind(arch, 'x86_64')) || ~isempty(strfind(arch, 'x64'))
            % 64-bit Windows
            engine_file_name = fullfile(examples_base_dir, 'bin', 'windows-x86_64', 'tsanpr.dll');
        else
            % 32-bit Windows
            engine_file_name = fullfile(examples_base_dir, 'bin', 'windows-x86', 'tsanpr.dll');
        end
    elseif isunix && ~ismac
        % Linux platform
        [~, uname_result] = system('uname -m');
        uname_result = strtrim(uname_result);

        if strcmp(uname_result, 'x86_64')
            % 64-bit x86 Linux
            engine_file_name = fullfile(examples_base_dir, 'bin', 'linux-x86_64', 'libtsanpr.so');
        elseif strcmp(uname_result, 'aarch64')
            % ARM64 Linux
            engine_file_name = fullfile(examples_base_dir, 'bin', 'linux-aarch64', 'libtsanpr.so');
        else
            fprintf('Unsupported Linux architecture: %s\n', uname_result);
            engine_file_name = '';
        end
    else
        engine_file_name = '';
    end
end

function read_image_file(tsanpr, imgfile, output_format, options)
    % Read an image file and call anpr_read_file.

    fprintf('%s (outputFormat="%s", options="%s") => ', imgfile, output_format, options);
    result = tsanpr.anpr_read_file(imgfile, output_format, options);
    fprintf('%s\n', result);
end

function read_encoded_image(tsanpr, imgfile, output_format, options)
    % Read an encoded image file as bytes and call tsanpr.anpr_read_pixels with 'encoded' pixel format.

    fprintf('%s (outputFormat="%s", options="%s") => ', imgfile, output_format, options);

    try
        if ~exist(imgfile, 'file')
            fprintf('File does not exist\n');
            return;
        end

        % Read file as binary data
        fid = fopen(imgfile, 'rb');
        if fid == -1
            fprintf('Failed to open file\n');
            return;
        end

        encoded_img = fread(fid, '*uint8');
        fclose(fid);

        result = tsanpr.anpr_read_pixels(encoded_img, length(encoded_img), 0, 0, ...
                                        'encoded', output_format, options);
        fprintf('%s\n', result);

    catch ME
        fprintf('ERROR: Exception - %s\n', ME.message);
    end
end

function pixel_format = get_pixel_format(channels)
    % Determine pixel format string based on image channels.

    switch channels
        case 1
            pixel_format = 'GRAY';
        case 2
            pixel_format = 'BGR565';  % or "BGR555"
        case 3
            pixel_format = 'BGR';
        case 4
            pixel_format = 'BGRA';
        otherwise
            pixel_format = '';
    end
end

function read_pixel_buffer(tsanpr, imgfile, output_format, options)
    % Use the pixel buffer-based ANPR function.
    % This implementation uses MATLAB's built-in image processing functions.

    fprintf('%s (outputFormat="%s", options="%s") => ', imgfile, output_format, options);

    try
        if ~exist(imgfile, 'file')
            fprintf('Image file does not exist\n');
            return;
        end

        % Read image using MATLAB's imread
        [img, ~, alpha] = imread(imgfile);

        % Get image dimensions
        [height, width, channels] = size(img);

        % Handle alpha channel if present
        if ~isempty(alpha)
            img = cat(3, img, alpha);
            channels = channels + 1;
        end

        % Convert RGB to BGR if needed (MATLAB uses RGB, TSANPR expects BGR)
        if channels == 3
            img = img(:, :, [3, 2, 1]); % RGB to BGR
            pixel_format = 'BGR';
        elseif channels == 4
            img = img(:, :, [3, 2, 1, 4]); % RGBA to BGRA
            pixel_format = 'BGRA';
        elseif channels == 1
            pixel_format = 'GRAY';
        else
            fprintf('Unsupported number of channels: %d\n', channels);
            return;
        end

        % Convert to uint8 column vector (MATLAB stores images as row-major, need column-major)
        img_data = reshape(permute(img, [2, 1, 3]), [], 1);

        stride = width * channels;
        result = tsanpr.anpr_read_pixels(img_data, width, height, stride, ...
                                        pixel_format, output_format, options);
        fprintf('%s\n', result);

    catch ME
        fprintf('ERROR: Exception - %s\n', ME.message);
    end
end

function read_license_plates(tsanpr, country_code, examples_base_dir)
    % NOTICE:
    % anpr_initialize should be called only once after library load.
    % Therefore, it is not possible to change the country code after anpr_initialize has been called.
    % While using the free trial license, you can try all languages.
    % When you purchase a commercial license, you can only use the selected language.

    error_msg = tsanpr.anpr_initialize(['text;country=' country_code]);
    if ~isempty(error_msg)
        fprintf('anpr_initialize() failed: %s\n', error_msg);
        return;
    end

    image_dir = fullfile(examples_base_dir, 'img', country_code);

    % TODO: Try each function as needed
    anpr_func = @read_image_file;
    % anpr_func = @read_encoded_image;
    % anpr_func = @read_pixel_buffer;

    % TODO: Try each output format as needed
    output_format = 'text';
    % output_format = 'json';
    % output_format = 'yaml';
    % output_format = 'xml';
    % output_format = 'csv';

    % Single license plate recognition (default)
    anpr_func(tsanpr, fullfile(image_dir, 'licensePlate.jpg'), output_format, '');

    % Recognize multiple license plates attached to vehicles
    anpr_func(tsanpr, fullfile(image_dir, 'multiple.jpg'), output_format, 'vm');

    % Recognize multiple license plates attached to vehicles (including motorcycles)
    anpr_func(tsanpr, fullfile(image_dir, 'multiple.jpg'), output_format, 'vmb');

    % Recognize multiple license plates attached to vehicles with surround detection
    anpr_func(tsanpr, fullfile(image_dir, 'surround.jpg'), output_format, 'vms');

    % Recognize multiple surrounding objects (vehicles)
    anpr_func(tsanpr, fullfile(image_dir, 'surround.jpg'), output_format, 'dms');

    % Recognize multiple surrounding objects (vehicles) and license plates
    anpr_func(tsanpr, fullfile(image_dir, 'surround.jpg'), output_format, 'dmsr');

    % Recognize multiple surrounding objects and license plates within RoI
    anpr_func(tsanpr, fullfile(image_dir, 'surround.jpg'), output_format, ...
             'dmsri549,700,549,2427,1289,2427,1289,700');
end
