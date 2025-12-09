# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.

defmodule ANPR do
  @moduledoc """
  Main ANPR example module for Elixir.
  """

  alias TSANPR

  @examples_base_dir Path.expand("../../..", __DIR__)

  def get_engine_file_name do
    # Generate engine filename depending on platform and architecture.
    case :os.type() do
      {:win32, _} ->
        case :erlang.system_info(:wordsize) do
          8 -> Path.join([@examples_base_dir, "bin", "windows-x86_64", "tsanpr.dll"])
          _ -> Path.join([@examples_base_dir, "bin", "windows-x86", "tsanpr.dll"])
        end

      {:unix, :linux} ->
        case :erlang.system_info(:system_architecture) do
          ~c"x86_64" ++ _ -> Path.join([@examples_base_dir, "bin", "linux-x86_64", "libtsanpr.so"])
          ~c"aarch64" ++ _ -> Path.join([@examples_base_dir, "bin", "linux-aarch64", "libtsanpr.so"])
          _ -> ""
        end

      _ ->
        ""
    end
  end

  def read_image_file(tsanpr, imgfile, output_format, options) do
    # Read an image file and call anpr_read_file.
    IO.write("#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => ")
    result = TSANPR.anpr_read_file(tsanpr, imgfile, output_format, options)
    IO.puts(result)
  end

  def read_encoded_image(tsanpr, imgfile, output_format, options) do
    # Read an encoded image file as bytes and call TSANPR.anpr_read_pixels with 'encoded' pixel format.
    IO.write("#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => ")

    try do
      case File.exists?(imgfile) do
        false ->
          IO.puts("File does not exist")

        true ->
          {:ok, encoded_img} = File.read(imgfile)

          result =
            TSANPR.anpr_read_pixels(
              tsanpr,
              encoded_img,
              byte_size(encoded_img),
              0,
              0,
              "encoded",
              output_format,
              options
            )

          IO.puts(result)
      end
    rescue
      e -> IO.puts("ERROR: Exception - #{Exception.message(e)}")
    end
  end

  def read_pixel_buffer(_tsanpr, imgfile, output_format, options) do
    # Use the pixel buffer-based ANPR function.
    # Note: This is a simplified version. In a real implementation,
    # you would need to integrate with an image processing library.
    IO.write("#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => ")
    IO.puts("Pixel buffer reading not implemented in this example")
  end

  def read_license_plates(tsanpr, country_code) do
    # NOTICE:
    # anpr_initialize should be called only once after library load.
    # Therefore, it is not possible to change the country code after anpr_initialize has been called.
    # While using the free trial license, you can try all languages.
    # When you purchase a commercial license, you can only use the selected language.
    error = TSANPR.anpr_initialize(tsanpr, "text;country=#{country_code}")

    case error do
      "" -> :ok
      _ ->
        IO.puts("anpr_initialize() failed: #{error}")
        :error
    end

    image_dir = Path.join([@examples_base_dir, "img", country_code])

    # TODO: Try each function as needed
    anpr_func = &read_image_file/4
    # anpr_func = &read_encoded_image/4
    # anpr_func = &read_pixel_buffer/4

    # TODO: Try each output format as needed
    output_format = "text"
    # output_format = "json"
    # output_format = "yaml"
    # output_format = "xml"
    # output_format = "csv"

    # Single license plate recognition (default)
    anpr_func.(tsanpr, Path.join(image_dir, "licensePlate.jpg"), output_format, "")
    # Recognize multiple license plates attached to vehicles
    anpr_func.(tsanpr, Path.join(image_dir, "multiple.jpg"), output_format, "vm")
    # Recognize multiple license plates attached to vehicles (including motorcycles)
    anpr_func.(tsanpr, Path.join(image_dir, "multiple.jpg"), output_format, "vmb")
    # Recognize multiple license plates attached to vehicles with surround detection
    anpr_func.(tsanpr, Path.join(image_dir, "surround.jpg"), output_format, "vms")
    # Recognize multiple surrounding objects (vehicles)
    anpr_func.(tsanpr, Path.join(image_dir, "surround.jpg"), output_format, "dms")
    # Recognize multiple surrounding objects (vehicles) and license plates
    anpr_func.(tsanpr, Path.join(image_dir, "surround.jpg"), output_format, "dmsr")

    # Recognize multiple surrounding objects and license plates within RoI
    anpr_func.(
      tsanpr,
      Path.join(image_dir, "surround.jpg"),
      output_format,
      "dmsri549,700,549,2427,1289,2427,1289,700"
    )
  end

  def main do
    engine_file_name = get_engine_file_name()

    case {engine_file_name, File.exists?(engine_file_name)} do
      {"", _} ->
        IO.puts("Unsupported operating system or engine not found")

      {_, false} ->
        IO.puts("Unsupported operating system or engine not found")

      {engine_file, true} ->
        try do
          {:ok, tsanpr} = TSANPR.load(engine_file)

          # TODO: Try each country code as needed
          read_license_plates(tsanpr, "KR")
          # read_license_plates(tsanpr, "JP")
          # read_license_plates(tsanpr, "VN")
        rescue
          e -> IO.puts("TSANPR initialization failed: #{Exception.message(e)}")
        end
    end
  end
end
