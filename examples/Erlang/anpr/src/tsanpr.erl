%% The MIT License (MIT)
%% Copyright © 2022-2025 TS-Solution Corp.

-module(tsanpr).
-export([load/1, anpr_initialize/2, anpr_read_file/4, anpr_read_pixels/8]).

-on_load(init/0).

%% Load NIF library on module load
init() ->
    SoName = case code:priv_dir(anpr) of
        {error, bad_name} ->
            %% Development mode: try current dir paths
            case filelib:is_file("priv/tsanpr_nif") of
                true -> "priv/tsanpr_nif";
                false -> "priv/tsanpr_nif.so"
            end;
        Dir ->
            filename:join(Dir, "tsanpr_nif")
    end,
    erlang:load_nif(SoName, 0).

%%
%% Public API - these call the NIF implementations
%%

load(LibraryPath) ->
    load_nif(LibraryPath).

anpr_initialize(TSANPR, Mode) ->
    anpr_initialize_nif(TSANPR, Mode).

anpr_read_file(TSANPR, ImgFileName, OutputFormat, Options) ->
    anpr_read_file_nif(TSANPR, ImgFileName, OutputFormat, Options).

anpr_read_pixels(TSANPR, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options) ->
    anpr_read_pixels_nif(TSANPR, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options).

%%
%% NIF stubs - these will be replaced by the actual C NIF implementations
%% They are only called if NIF loading fails
%%

load_nif(_LibraryPath) ->
    erlang:nif_error(nif_library_not_loaded).

anpr_initialize_nif(_TSANPR, _Mode) ->
    erlang:nif_error(nif_library_not_loaded).

anpr_read_file_nif(_TSANPR, _ImgFileName, _OutputFormat, _Options) ->
    erlang:nif_error(nif_library_not_loaded).

anpr_read_pixels_nif(_TSANPR, _Pixels, _Width, _Height, _Stride, _PixelFormat, _OutputFormat, _Options) ->
    erlang:nif_error(nif_library_not_loaded).
