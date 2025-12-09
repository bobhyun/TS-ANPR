defmodule Anpr.MixProject do
  use Mix.Project

  def project do
    [
      app: :anpr,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_targets: ["all"],
      make_clean: ["clean"]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to fetch and install dependencies.
  defp deps do
    [
      {:elixir_make, "~> 0.6", runtime: false}
    ]
  end
end
