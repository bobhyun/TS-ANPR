%% The MIT License (MIT)
%% Copyright © 2022-2025 TS-Solution Corp.

-module(anpr_ffi).
-export([get_engine_path/0, read_file_binary/1, file_exists/1]).

%% Convert binary to list (Gleam strings are binaries, Erlang expects charlists)
to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_list(List) when is_list(List) -> List.

%% Get engine file path based on platform
get_engine_path() ->
    BaseDir = <<"../../bin">>,
    case os:type() of
        {win32, _} ->
            case erlang:system_info(wordsize) of
                8 -> <<BaseDir/binary, "/windows-x86_64/tsanpr.dll">>;
                _ -> <<BaseDir/binary, "/windows-x86/tsanpr.dll">>
            end;
        {unix, linux} ->
            Arch = erlang:system_info(system_architecture),
            case Arch of
                "aarch64" ++ _ -> <<BaseDir/binary, "/linux-aarch64/libtsanpr.so">>;
                _ -> <<BaseDir/binary, "/linux-x86_64/libtsanpr.so">>
            end;
        _ ->
            <<"">>
    end.

%% Read file as binary
read_file_binary(Path) ->
    case file:read_file(to_list(Path)) of
        {ok, Binary} -> {ok, Binary};
        {error, _} -> {error, nil}
    end.

%% Check if file exists (wrapper for filelib:is_regular)
file_exists(Path) ->
    filelib:is_regular(to_list(Path)).
