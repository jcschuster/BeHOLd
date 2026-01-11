defmodule BeHOLd.MixProject do
  use Mix.Project

  @version "1.0.0"
  @source_url "https://github.com/jcschuster/BeHOLd"

  def project do
    [
      app: :behold,
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
      source_url: @source_url,
      source_ref: "v#{@version}",
      before_closing_head_tag: &before_closing_head_tag/1,
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

  defp before_closing_head_tag(:html) do
    """
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.css">
    """
  end

  defp before_closing_head_tag(_), do: ""

  defp before_closing_body_tag(:html) do
    """
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.js"></script>
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/contrib/auto-render.min.js"></script>

    <script>
      document.addEventListener("DOMContentLoaded", function() {
        var renderMath = function() {
          if (window.renderMathInElement) {
            renderMathInElement(document.body, {
              delimiters: [
                {left: "$$", right: "$$", display: true},
                {left: "$", right: "$", display: false}
              ]
            });
          }
        };

        var attempts = 0;
        var initInterval = setInterval(function() {
          if (window.renderMathInElement) {
            renderMath();
            clearInterval(initInterval);
          } else if (attempts > 20) {
            clearInterval(initInterval);
          }
          attempts++;
        }, 100);

        var observer = new MutationObserver(function(mutations) {
          observer.disconnect();
          renderMath();
          observer.observe(document.body, { childList: true, subtree: true });
        });

        observer.observe(document.body, { childList: true, subtree: true });
      });
    </script>
    """
  end

  defp before_closing_body_tag(_), do: ""
end
