%% The MIT License (MIT)
%% Copyright © 2022-2025 TS-Solution Corp.

-module(anpr).
-export([main/0, read_image_file/4, read_encoded_image/4, read_pixel_buffer/4]).

-define(EXAMPLES_BASE_DIR, "../..").

get_engine_file_name() ->
    case os:type() of
        {win32, _} ->
            filename:join([?EXAMPLES_BASE_DIR, "bin", "windows-x86_64", "tsanpr.dll"]);
        {unix, linux} ->
            case erlang:system_info(system_architecture) of
                "aarch64" ++ _ ->
                    filename:join([?EXAMPLES_BASE_DIR, "bin", "linux-aarch64", "libtsanpr.so"]);
                _ ->
                    filename:join([?EXAMPLES_BASE_DIR, "bin", "linux-x86_64", "libtsanpr.so"])
            end;
        _ ->
            ""
    end.

%% Read image file using engine
read_image_file(TSANPR, ImgFile, OutputFormat, Options) ->
    io:format("~s (outputFormat=\"~s\", options=\"~s\") => ~n", [ImgFile, OutputFormat, Options]),
    Result = tsanpr:anpr_read_file(TSANPR, ImgFile, OutputFormat, Options),
    io:format("~s~n", [Result]).

%% Read encoded image as a buffer
read_encoded_image(TSANPR, ImgFile, OutputFormat, Options) ->
    io:format("~s (outputFormat=\"~s\", options=\"~s\") => ~n", [ImgFile, OutputFormat, Options]),
    case file:read_file(ImgFile) of
        {ok, EncodedImg} ->
            Result = tsanpr:anpr_read_pixels(TSANPR, EncodedImg, byte_size(EncodedImg), 0, 0, "encoded", OutputFormat, Options),
            io:format("~s~n", [Result]);
        {error, Reason} ->
            io:format("File read failed: ~p~n", [Reason])
    end.

%% Read decoded pixel buffer (simplified implementation for demonstration)
read_pixel_buffer(_TSANPR, ImgFile, OutputFormat, Options) ->
    io:format("~s (outputFormat=\"~s\", options=\"~s\") => ~n", [ImgFile, OutputFormat, Options]),
    io:format("Pixel buffer processing not implemented in this example~n").

%% Recognize license plates for a given country
read_license_plates(TSANPR, CountryCode) ->
    %% NOTICE:
    %% anpr_initialize should be called only once after library load.
    %% Therefore, it is not possible to change the country code after anpr_initialize has been called.
    %% While using the free trial license, you can try all languages.
    %% When you purchase a commercial license, you can only use the selected language.
    InitParams = "text;country=" ++ CountryCode,
    case tsanpr:anpr_initialize(TSANPR, InitParams) of
        "" ->
            ImageDir = filename:join([?EXAMPLES_BASE_DIR, "img", CountryCode]),

            %% TODO: Try each function as needed
            ANPRFunc = fun read_image_file/4,
            %% ANPRFunc = fun read_encoded_image/4,
            %% ANPRFunc = fun read_pixel_buffer/4,

            %% TODO: Try each output format as needed
            OutputFormat = "text",
            %% OutputFormat = "json",
            %% OutputFormat = "yaml",
            %% OutputFormat = "xml",
            %% OutputFormat = "csv",

            ANPRFunc(TSANPR, filename:join(ImageDir, "licensePlate.jpg"), OutputFormat, ""),    %% Single license plate recognition (default)
            ANPRFunc(TSANPR, filename:join(ImageDir, "multiple.jpg"), OutputFormat, "vm"),      %% Recognize multiple license plates attached to vehicles
            ANPRFunc(TSANPR, filename:join(ImageDir, "multiple.jpg"), OutputFormat, "vmb"),     %% Recognize multiple license plates attached to vehicles (including motorcycles)
            ANPRFunc(TSANPR, filename:join(ImageDir, "surround.jpg"), OutputFormat, "vms"),     %% Recognize multiple license plates attached to vehicles with surround detection
            ANPRFunc(TSANPR, filename:join(ImageDir, "surround.jpg"), OutputFormat, "dms"),     %% Recognize multiple surrounding objects (vehicles)
            ANPRFunc(TSANPR, filename:join(ImageDir, "surround.jpg"), OutputFormat, "dmsr"),    %% Recognize multiple surrounding objects (vehicles) and license plates

            %% Recognize multiple surrounding objects and license plates within RoI
            ANPRFunc(TSANPR, filename:join(ImageDir, "surround.jpg"), OutputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");
        Error ->
            io:format("anpr_initialize() failed: ~s~n", [Error])
    end.

main() ->
    EngineFileName = get_engine_file_name(),
    case filelib:is_file(EngineFileName) of
        false ->
            io:format("Unsupported operating system or engine not found~n");
        true ->
            case tsanpr:load(EngineFileName) of
                {ok, TSANPR} ->
                    %% TODO: Try each country code as needed
                    read_license_plates(TSANPR, "KR");
                    %% read_license_plates(TSANPR, "JP");
                    %% read_license_plates(TSANPR, "VN");
                {error, Reason} ->
                    io:format("TSANPR initialization failed: ~p~n", [Reason])
            end
    end.
