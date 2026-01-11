defmodule BeHOLd do
  @moduledoc """
  Provides an overview over the included functionality in this package.

  Shorthand term constructors and useful patterns are implemented in the
  modules `BeHOLd.ClassicalHOL.Definitions` and `BeHOLd.ClassicalHOL.Patterns`.

  Functionality for parsing of TPTP syntax is given for formula strings in
  `BeHOLd.Parser` and for TPTP problem files in `BeHOLd.TPTP`. The latter also
  allows for representing the internal
  [`HOL.Data.hol_term()`](https://hexdocs.pm/hol/HOL.Data.html#t:hol_term/0)
  data structure as string in TPTP syntax.

  A type context for parsing is represented by the data structure defined by
  `BeHOLd.Data.Context`. `BeHOLd.Data.Problem` is a data structure representing
  a TPTP proof problem.

  The modules `BeHOLd.Util.Lexer` and `BeHOLd.Util.TypeInference` contain
  utility functions required by the parser.
  """
  require HOL.Data
end
