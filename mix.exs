defmodule ToxicParser.MixProject do
  use Mix.Project

  def project do
    [
      app: :toxic_parser,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    toxic_path = System.get_env("TOXIC_PATH") || "/Users/lukaszsamson/claude_fun/toxic"

    [
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false},
      {:stream_data, "~> 1.2", only: :test},
      {:toxic, path: toxic_path}
    ]
  end
end
