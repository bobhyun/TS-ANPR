# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.

defmodule TSANPR do
  @moduledoc """
  TSANPR wrapper module for Elixir.
  Provides interface to the native TSANPR library using NIFs.
  """

  @on_load :load_nifs

  def load_nifs do
    # Load the NIF library from priv directory (pass basename; VM adds platform ext)
    so_name =
      case :code.priv_dir(:anpr) do
        {:error, :bad_name} -> to_charlist("priv/tsanpr_nif")
        dir -> Path.join(dir, "tsanpr_nif") |> to_charlist()
      end

    :erlang.load_nif(so_name, 0)
  end

  @doc """
  Initialize TSANPR with the given library path.
  Returns {:ok, resource} on success, {:error, reason} on failure.
  """
  def load(library_path) when is_binary(library_path) do
    load_nif(to_charlist(library_path))
  end

  @doc """
  Initialize the ANPR engine with the specified mode.
  Returns empty string on success, error message on failure.
  """
  def anpr_initialize(tsanpr, mode) when is_binary(mode) do
    anpr_initialize_nif(tsanpr, to_charlist(mode))
  end

  @doc """
  Read and process an image file.
  Returns the recognition result as a string.
  """
  def anpr_read_file(tsanpr, img_file_name, output_format, options) 
      when is_binary(img_file_name) and is_binary(output_format) and is_binary(options) do
    anpr_read_file_nif(
      tsanpr,
      to_charlist(img_file_name),
      to_charlist(output_format),
      to_charlist(options)
    )
  end

  @doc """
  Process pixel data directly.
  Returns the recognition result as a string.
  """
  def anpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options)
      when is_binary(pixels) and is_binary(pixel_format) and is_binary(output_format) and is_binary(options) do
    anpr_read_pixels_nif(
      tsanpr,
      pixels,
      width,
      height,
      stride,
      to_charlist(pixel_format),
      to_charlist(output_format),
      to_charlist(options)
    )
  end

  # NIF stubs - these will be replaced by the actual C NIF implementations
  defp load_nif(_library_path) do
    :erlang.nif_error(:nif_library_not_loaded)
  end

  defp anpr_initialize_nif(_tsanpr, _mode) do
    :erlang.nif_error(:nif_library_not_loaded)
  end

  defp anpr_read_file_nif(_tsanpr, _img_file_name, _output_format, _options) do
    :erlang.nif_error(:nif_library_not_loaded)
  end

  defp anpr_read_pixels_nif(_tsanpr, _pixels, _width, _height, _stride, _pixel_format, _output_format, _options) do
    :erlang.nif_error(:nif_library_not_loaded)
  end
end
