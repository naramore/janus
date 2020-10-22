defmodule Janus.MixProject do
  use Mix.Project

  @in_production Mix.env() == :prod
  @version "0.0.1"
  @author "naramore"
  @source_url "https://github.com/naramore/janus"
  @description """
  Pathom in Elixir https://github.com/wilkerlucio/pathom
  """

  def project do
    [
      app: :janus,
      version: @version,
      elixir: "~> 1.10",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:boundary] ++ Mix.compilers(),
      build_embedded: @in_production,
      start_permanent: @in_production,
      aliases: aliases(),
      deps: deps(),
      description: @description,
      package: package(),
      name: "Janus",
      docs: docs(),
      test_coverage: [tool: ExCoveralls],
      boundary: [externals_mode: :relaxed],
      dialyzer: [
        flags: [
          :underspecs,
          :error_handling,
          :unmatched_returns,
          :unknown,
          :race_conditions
        ],
        ignore_warnings: ".dialyzer_ignore.exs",
        list_unused_filters: true
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp package do
    [
      contributors: [@author],
      maintainers: [@author],
      source_ref: "v#{@version}",
      links: %{"GitHub" => @source_url},
      files: ~w(lib .formatter.exs mix.exs README.md)
    ]
  end

  defp docs do
    [
      main: "readme",
      source_ref: "v#{@version}",
      source_url: @source_url,
      extras: ["README.md"]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(env) when env in [:dev, :test], do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:stream_data, "~> 0.5", only: [:dev, :test]},
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.4", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.13", only: [:dev, :test]},
      {:ex_doc, "~> 0.23", only: :dev, runtime: false},
      {:boundary, "~> 0.6", runtime: false},
      {:telemetry, "~> 0.4"}
    ]
  end

  defp aliases do
    [
      check: [
        "compile --warnings-as-errors",
        "credo",
        "dialyzer",
        "format"
      ]
    ]
  end
end
