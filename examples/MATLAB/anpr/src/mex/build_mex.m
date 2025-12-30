% Build script for TSANPR MEX file
% Works with both MATLAB and GNU Octave
%
% Usage:
%   MATLAB: >> build_mex
%   Octave: octave --eval "build_mex"

function build_mex()
    fprintf('Building TSANPR MEX file...\n');

    % Detect environment
    is_octave = exist('OCTAVE_VERSION', 'builtin') ~= 0;

    if is_octave
        fprintf('Environment: GNU Octave %s\n', OCTAVE_VERSION);
        build_octave();
    else
        fprintf('Environment: MATLAB %s\n', version);
        build_matlab();
    end
end

function build_matlab()
    % Build MEX file for MATLAB
    src_file = 'tsanpr_mex.c';

    if ~exist(src_file, 'file')
        error('Source file not found: %s', src_file);
    end

    try
        fprintf('Compiling %s...\n', src_file);

        if ispc
            % Windows - link with kernel32 for LoadLibrary
            mex(src_file);
        else
            % Linux/macOS - link with dl for dlopen
            mex(src_file, '-ldl');
        end

        fprintf('Build successful!\n');

        % Show output file
        if ispc
            if strcmp(computer('arch'), 'win64')
                output = 'tsanpr_mex.mexw64';
            else
                output = 'tsanpr_mex.mexw32';
            end
        elseif ismac
            output = 'tsanpr_mex.mexmaci64';
        else
            output = 'tsanpr_mex.mexa64';
        end

        if exist(output, 'file')
            fprintf('Output: %s\n', output);
        end

    catch ME
        fprintf('Build failed: %s\n', ME.message);
        fprintf('\nTroubleshooting:\n');
        fprintf('  1. Ensure you have a C compiler installed\n');
        fprintf('  2. Run "mex -setup" to configure compiler\n');
        rethrow(ME);
    end
end

function build_octave()
    % Build MEX file for Octave
    src_file = 'tsanpr_mex.c';

    if ~exist(src_file, 'file')
        error('Source file not found: %s', src_file);
    end

    try
        fprintf('Compiling %s...\n', src_file);

        if ispc
            % Windows
            mkoctfile('--mex', src_file);
        else
            % Linux/macOS - link with dl for dlopen
            mkoctfile('--mex', '-ldl', src_file);
        end

        fprintf('Build successful!\n');

        % Show output file
        output = 'tsanpr_mex.mex';
        if exist(output, 'file')
            fprintf('Output: %s\n', output);
        end

    catch ME
        fprintf('Build failed: %s\n', ME.message);
        fprintf('\nTroubleshooting:\n');
        fprintf('  1. Ensure you have a C compiler installed\n');
        fprintf('     Windows: Install MinGW or MSVC\n');
        fprintf('     Linux: sudo apt-get install gcc\n');
        fprintf('  2. Ensure mkoctfile is available\n');
        rethrow(ME);
    end
end
