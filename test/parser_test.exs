defmodule BeHOLd.ParserTest do
  @moduledoc """
  Tests for the BeHOLd.Parser module.
  """
  use ExUnit.Case, async: true

  alias BeHOLd.Parser
  alias BeHOLd.Data.Context
  import HOL.Data
  import BeHOLd.ClassicalHOL.Definitions
  import BeHOLd.ClassicalHOL.Patterns

  describe "parse/2 - constants" do
    test "parses $true" do
      term = Parser.parse("$true")

      assert match?(hol_term(head: declaration(name: "⊤"), type: type(goal: :o)), term)
    end

    test "parses $false" do
      term = Parser.parse("$false")

      assert match?(hol_term(head: declaration(name: "⊥"), type: type(goal: :o)), term)
    end
  end

  describe "parse/2 - variables" do
    test "parses variable in boolean context with type o" do
      # Variables in boolean context (like conjunction) get type o
      term = Parser.parse("X & $true")

      conjunction(left, _right) = term
      hol_term(head: head, type: type) = left
      assert match?(declaration(kind: :fv, name: "X"), head)
      assert type == type_o()
    end

    test "parses multiple variables" do
      term = Parser.parse("X & Y")

      # X & Y should be a conjunction
      assert match?(conjunction(_, _), term)
    end
  end

  describe "parse/2 - negation" do
    test "parses simple negation" do
      term = Parser.parse("~X")

      assert match?(negated(_), term)
    end

    test "parses negation of $true" do
      term = Parser.parse("~$true")

      assert match?(negated(hol_term(head: declaration(name: "⊤"))), term)
    end

    test "parses negation of $false" do
      term = Parser.parse("~$false")

      assert match?(negated(hol_term(head: declaration(name: "⊥"))), term)
    end

    test "parses double negation" do
      term = Parser.parse("~~X")

      assert match?(negated(negated(_)), term)
    end
  end

  describe "parse/2 - conjunction" do
    test "parses simple conjunction" do
      term = Parser.parse("X & Y")

      assert match?(conjunction(_, _), term)
    end

    test "parses conjunction with constants" do
      term = Parser.parse("$true & $false")

      conjunction(left, right) = term
      assert match?(hol_term(head: declaration(name: "⊤")), left)
      assert match?(hol_term(head: declaration(name: "⊥")), right)
    end

    test "parses left-associative conjunction" do
      term = Parser.parse("X & Y & Z")

      # (X & Y) & Z
      assert match?(conjunction(conjunction(_, _), _), term)
    end

    test "parses NAND operator" do
      term = Parser.parse("X ~& Y")

      # NAND is ~(X & Y)
      assert match?(negated(conjunction(_, _)), term)
    end
  end

  describe "parse/2 - disjunction" do
    test "parses simple disjunction" do
      term = Parser.parse("X | Y")

      assert match?(disjunction(_, _), term)
    end

    test "parses left-associative disjunction" do
      term = Parser.parse("X | Y | Z")

      # (X | Y) | Z
      assert match?(disjunction(disjunction(_, _), _), term)
    end

    test "parses NOR operator" do
      term = Parser.parse("X ~| Y")

      # NOR is ~(X | Y)
      assert match?(negated(disjunction(_, _)), term)
    end
  end

  describe "parse/2 - implication" do
    test "parses implication" do
      term = Parser.parse("X => Y")

      assert match?(implication(_, _), term)
    end

    test "parses right-associative implication" do
      term = Parser.parse("X => Y => Z")

      # X => (Y => Z)
      assert match?(implication(_, implication(_, _)), term)
    end

    test "parses reverse implication" do
      term = Parser.parse("X <= Y")

      # X <= Y is Y => X
      implication(ante, conseq) = term
      hol_term(head: declaration(name: ante_name)) = ante
      hol_term(head: declaration(name: conseq_name)) = conseq
      assert ante_name == "Y"
      assert conseq_name == "X"
    end
  end

  describe "parse/2 - equivalence" do
    test "parses equivalence" do
      term = Parser.parse("X <=> Y")

      assert match?(equivalence(_, _), term)
    end

    test "parses XOR" do
      term = Parser.parse("X <~> Y")

      # XOR is ~(X <=> Y)
      assert match?(negated(equivalence(_, _)), term)
    end
  end

  describe "parse/2 - equality" do
    test "parses equality" do
      term = Parser.parse("X = Y")

      assert match?(equality(_, _), term)
    end

    test "parses inequality" do
      term = Parser.parse("X != Y")

      # Inequality is ~(X = Y)
      assert match?(negated(equality(_, _)), term)
    end
  end

  describe "parse/2 - universal quantification" do
    test "parses simple universal quantification" do
      term = Parser.parse("![X : $o]: X")

      assert match?(universal_quantification(_), term)
    end

    test "parses universal quantification with typed variable" do
      term = Parser.parse("![X : $i]: X = X")

      assert match?(universal_quantification(_), term)
    end

    test "parses multiple bound variables as nested structure" do
      term = Parser.parse("![X : $o, Y : $o]: X & Y")

      # The parsed structure may have nested quantifiers in the body
      # due to how the parser handles multiple variables
      assert match?(universal_quantification(_), term)
    end

    test "parses universal quantification with untyped variable" do
      term = Parser.parse("![X]: X & $true")

      assert match?(universal_quantification(_), term)
    end
  end

  describe "parse/2 - existential quantification" do
    test "parses simple existential quantification" do
      term = Parser.parse("?[X : $o]: X")

      assert match?(existential_quantification(_), term)
    end

    test "parses existential quantification with typed variable" do
      term = Parser.parse("?[X : $i]: X = X")

      assert match?(existential_quantification(_), term)
    end

    test "parses multiple bound variables as nested structure" do
      term = Parser.parse("?[X : $o, Y : $o]: X | Y")

      # The parsed structure may have nested quantifiers in the body
      # due to how the parser handles multiple variables
      assert match?(existential_quantification(_), term)
    end
  end

  describe "parse/2 - lambda abstraction" do
    test "parses simple lambda" do
      term = Parser.parse("^[X : $o]: X")

      hol_term(bvars: bvars) = term
      assert length(bvars) == 1
    end

    test "parses lambda with function type" do
      term = Parser.parse("^[X : $i]: X = X")

      hol_term(bvars: bvars, type: type) = term
      assert length(bvars) == 1
      # Type should be i -> o
      assert match?(type(goal: :o, args: [type(goal: :i)]), type)
    end

    test "parses multiple lambda variables" do
      term = Parser.parse("^[X : $o, Y : $o]: X & Y")

      hol_term(bvars: bvars) = term
      assert length(bvars) == 2
    end
  end

  describe "parse/2 - application" do
    test "parses application in boolean context" do
      # Application where the result is used in a boolean expression
      ctx =
        Context.new()
        |> Context.put_const("f", mk_type(:o, [type_i()]))
        |> Context.put_var("X", type_i())

      term = Parser.parse("(f @ X) & $true", ctx)

      # The result should be a conjunction
      assert match?(conjunction(_, _), term)
    end
  end

  describe "parse/2 - pi and sigma operators" do
    test "parses pi operator (!!)" do
      term = Parser.parse("!! (^[X : $o]: X)")

      # Pi is second-order universal quantification
      assert match?(universal_quantification(_), term)
    end

    test "parses sigma operator (??)" do
      term = Parser.parse("?? (^[X : $o]: X)")

      # Sigma is second-order existential quantification
      assert match?(existential_quantification(_), term)
    end
  end

  describe "parse/2 - parentheses" do
    test "parses parenthesized expression" do
      term = Parser.parse("(X & Y)")

      assert match?(conjunction(_, _), term)
    end

    test "parses nested parentheses preserving meaning" do
      term = Parser.parse("(($true))")

      assert match?(hol_term(head: declaration(name: "⊤")), term)
    end

    test "parentheses change precedence" do
      # Without parens: X | (Y & Z)
      term1 = Parser.parse("X | Y & Z")
      # With parens: (X | Y) & Z
      term2 = Parser.parse("(X | Y) & Z")

      # term1 should be disjunction at top level
      assert match?(disjunction(_, _), term1)
      # term2 should be conjunction at top level
      assert match?(conjunction(_, _), term2)
    end
  end

  describe "parse/2 - operator precedence" do
    test "conjunction binds tighter than disjunction" do
      term = Parser.parse("X | Y & Z")

      # Should parse as X | (Y & Z)
      disjunction(left, right) = term
      hol_term(head: declaration(name: left_name)) = left
      assert left_name == "X"
      assert match?(conjunction(_, _), right)
    end

    test "disjunction binds tighter than implication" do
      term = Parser.parse("X => Y | Z")

      # Should parse as X => (Y | Z)
      implication(left, right) = term
      hol_term(head: declaration(name: left_name)) = left
      assert left_name == "X"
      assert match?(disjunction(_, _), right)
    end

    test "negation binds tighter than conjunction" do
      term = Parser.parse("~X & Y")

      # Should parse as (~X) & Y
      conjunction(left, right) = term
      assert match?(negated(_), left)
      hol_term(head: declaration(name: right_name)) = right
      assert right_name == "Y"
    end

    test "equality binds tighter than negation" do
      term = Parser.parse("~X = Y")

      # Should parse as ~(X = Y)
      assert match?(negated(equality(_, _)), term)
    end
  end

  describe "parse/2 - with context" do
    test "uses variable type from context" do
      ctx = Context.new() |> Context.put_var("X", type_i())
      term = Parser.parse("X = X", ctx)

      # X should have type i, so equality should be over type i
      equality(left, right) = term
      hol_term(type: left_type) = left
      hol_term(type: right_type) = right
      assert left_type == type_i()
      assert right_type == type_i()
    end

    test "uses constant type from context" do
      ctx = Context.new() |> Context.put_const("c", type_i())
      term = Parser.parse("c = c", ctx)

      equality(left, _) = term
      hol_term(head: declaration(name: name, type: type)) = left
      assert name == "c"
      assert type == type_i()
    end
  end

  describe "parse_type/1" do
    test "parses $o type" do
      type = Parser.parse_type("$o")

      assert type == type_o()
    end

    test "parses $i type" do
      type = Parser.parse_type("$i")

      assert type == type_i()
    end

    test "parses function type" do
      type = Parser.parse_type("$i > $o")

      assert type == mk_type(:o, [type_i()])
    end

    test "parses right-associative function type" do
      type = Parser.parse_type("$i > $i > $o")

      # i -> (i -> o)
      expected = mk_type(:o, [type_i(), type_i()])
      assert type == expected
    end

    test "parses parenthesized function type" do
      type = Parser.parse_type("($i > $o) > $o")

      # (i -> o) -> o
      expected = mk_type(:o, [mk_type(:o, [type_i()])])
      assert type == expected
    end

    test "parses user-defined base type" do
      type = Parser.parse_type("mytype")

      assert match?(type(goal: :mytype, args: []), type)
    end

    test "parses type_oo (o -> o)" do
      type = Parser.parse_type("$o > $o")

      assert type == type_oo()
    end

    test "parses type_ooo (o -> o -> o)" do
      type = Parser.parse_type("$o > $o > $o")

      assert type == type_ooo()
    end

    test "parses type_ii (i -> i)" do
      type = Parser.parse_type("$i > $i")

      assert type == type_ii()
    end

    test "parses type_iii (i -> i -> i)" do
      type = Parser.parse_type("$i > $i > $i")

      assert type == type_iii()
    end

    test "parses type_io (i -> o)" do
      type = Parser.parse_type("$i > $o")

      assert type == type_io()
    end

    test "parses type_iio (i -> i -> o)" do
      type = Parser.parse_type("$i > $i > $o")

      assert type == type_iio()
    end

    test "parses type_io_o ((i -> o) -> o)" do
      type = Parser.parse_type("($i > $o) > $o")

      assert type == type_io_o()
    end

    test "parses type_io_i ((i -> o) -> i)" do
      type = Parser.parse_type("($i > $o) > $i")

      assert type == type_io_i()
    end

    test "parses type_io_io_o ((i -> o) -> (i -> o) -> o)" do
      type = Parser.parse_type("($i > $o) > ($i > $o) > $o")

      assert type == type_io_io_o()
    end

    test "parses type_io_io_io ((i -> o) -> (i -> o) -> (i -> o))" do
      type = Parser.parse_type("($i > $o) > ($i > $o) > ($i > $o)")

      assert type == type_io_io_io()
    end
  end

  describe "parse_type_tokens/1" do
    test "parses and returns remaining tokens" do
      {:ok, tokens, _, _, _, _} = BeHOLd.Util.Lexer.tokenize("$i > $o, rest")

      {type, rest} = Parser.parse_type_tokens(tokens)

      assert type == mk_type(:o, [type_i()])
      assert rest == [{:comma, ","}, {:atom, "rest"}]
    end
  end

  describe "parse/2 - complex formulas" do
    test "parses De Morgan's law formula" do
      term = Parser.parse("~(X & Y) <=> (~X | ~Y)")

      assert match?(equivalence(_, _), term)
    end

    test "parses excluded middle" do
      term = Parser.parse("X | ~X")

      disjunction(left, right) = term
      assert match?(negated(_), right)

      hol_term(head: declaration(name: left_name)) = left
      negated(hol_term(head: declaration(name: inner_name))) = right
      assert left_name == inner_name
    end

    test "parses reflexivity of equality" do
      term = Parser.parse("![X : $i]: X = X")

      assert match?(universal_quantification(_), term)
    end

    test "parses implication with quantified antecedent" do
      # This tests complex nested structures
      term = Parser.parse("(![X : $i]: X = X) => $true")

      assert match?(implication(_, _), term)
    end

    test "parses conjunction with quantifier" do
      term = Parser.parse("(![X : $i]: X = X) & $true")

      assert match?(conjunction(_, _), term)
    end
  end

  describe "parse/2 - edge cases" do
    test "parses parenthesized true" do
      term = Parser.parse("($true)")

      assert match?(hol_term(head: declaration(name: "⊤")), term)
    end

    test "parses deeply nested negation" do
      term = Parser.parse("~~~~$true")

      assert match?(negated(negated(negated(negated(_)))), term)
    end

    test "parses quantifier with implication body" do
      term = Parser.parse("![X : $o]: X => $true")

      universal_quantification(inner) = term
      # The inner structure should contain an implication
      hol_term(bvars: [_bv]) = inner
    end
  end
end
