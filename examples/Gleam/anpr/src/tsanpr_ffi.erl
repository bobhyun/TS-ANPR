%% The MIT License (MIT)
%% Copyright © 2022-2025 TS-Solution Corp.

-module(tsanpr_ffi).
-export([load/1, anpr_initialize/2, anpr_read_file/4, anpr_read_pixels/8, decode_image/1]).

-on_load(init/0).

%% Load NIF library on module load
init() ->
    SoName = case code:priv_dir(anpr) of
        {error, bad_name} ->
            %% Development mode: try current dir paths
            case os:type() of
                {win32, _} ->
                    "priv/tsanpr_nif";
                _ ->
                    "priv/tsanpr_nif"
            end;
        Dir ->
            filename:join(Dir, "tsanpr_nif")
    end,
    %% Load NIF, return ok even if it fails (will be loaded later via load/1)
    case erlang:load_nif(SoName, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;  %% Already loaded
        {error, _} -> ok  %% Will load via load/1 function
    end.

%% Convert binary to list (Gleam strings are binaries, NIFs expect charlists)
to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_list(List) when is_list(List) -> List.

%% Convert list to binary (NIFs return charlists, Gleam expects binaries)
to_binary(List) when is_list(List) -> list_to_binary(List);
to_binary(Bin) when is_binary(Bin) -> Bin.

%%
%% Public API - these call the NIF implementations
%%

load(LibraryPath) ->
    load_nif(to_list(LibraryPath)).

anpr_initialize(TSANPR, Mode) ->
    Result = anpr_initialize_nif(TSANPR, to_list(Mode)),
    to_binary(Result).

anpr_read_file(TSANPR, ImgFileName, OutputFormat, Options) ->
    Result = anpr_read_file_nif(TSANPR, to_list(ImgFileName), to_list(OutputFormat), to_list(Options)),
    to_binary(Result).

anpr_read_pixels(TSANPR, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options) ->
    Result = anpr_read_pixels_nif(TSANPR, Pixels, Width, Height, Stride, to_list(PixelFormat), to_list(OutputFormat), to_list(Options)),
    to_binary(Result).

decode_image(FilePath) ->
    decode_image_nif(to_list(FilePath)).

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

decode_image_nif(_FilePath) ->
    erlang:nif_error(nif_library_not_loaded).
