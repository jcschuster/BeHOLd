defmodule BeHOLd.MixProject do
  use Mix.Project

  @version "1.0.0"
  @source_url "https://github.com/jcschuster/BeHOLd"

  def project do
    [
      app: :behold,
      licences: [:mit],
      version: @version,
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      source_url: @source_url,
      description:
        "An extention of the package HOL implementing the syntax of classical higher-order logic. Also includes a parser for TPTP TH0 syntax to the internal term representation of HOL.",
      docs: docs(),
      package: package()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:hol, "1.0.2"},
      # Parsing tool including a easy-to-use lexer/tokenizer
      {:nimble_parsec, "~> 1.4.2"},
      # Code analyzer, duplication checker and security analyzer
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      # Code analyzer and type checker
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      # Documentation generation
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: [
        "README.md",
        "demo.livemd"
      ],
      source_ref: "v#{@version}",
      source_url: @source_url,
      before_closing_body_tag: &before_closing_body_tag/1
    ]
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      maintainers: ["Johannes Schuster"],
      links: %{
        "GitHub" => @source_url
      },
      files: ~w(lib LICENSE mix.exs README.md)
    ]
  end

  # Enables LaTeX-like math rendering for markdown through KaTeX
  defp before_closing_body_tag(:html) do
    """
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.13.0/dist/katex.min.css" integrity="sha384-t5CR+zwDAROtph0PXGte6ia8heboACF9R5l/DiY+WZ3P2lxNgvJkQk5n7GPvLMYw" crossorigin="anonymous">
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.0/dist/katex.min.js" integrity="sha384-FaFLTlohFghEIZkw6VGwmf9ISTubWAVYW8tG8+w2LAIftJEULZABrF9PPFv+tVkH" crossorigin="anonymous"></script>
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.0/dist/contrib/auto-render.min.js" integrity="sha384-bHBqxz8fokvgoJ/sc17HODNxa42TlaEhB+w8ZJXTc2nZf1VgEaFZeZvT4Mznfz0v" crossorigin="anonymous"
        onload="renderMathInElement(document.body);"></script>
    """
  end

  defp before_closing_body_tag(_), do: ""
end
