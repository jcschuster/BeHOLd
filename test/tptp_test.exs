defmodule BeHOLd.TPTPTest do
  @moduledoc """
  Tests for the BeHOLd.TPTP module.
  """
  use ExUnit.Case, async: true

  alias BeHOLd.TPTP
  alias BeHOLd.Parser
  alias BeHOLd.Data.Context
  import HOL.Data
  import HOL.Terms
  import BeHOLd.ClassicalHOL.Definitions

  # Helper to create test terms
  defp make_var(name, type) do
    mk_term(mk_free_var(name, type))
  end

  describe "type_to_tptp/1 - atoms" do
    test "converts :o atom to $o" do
      assert TPTP.type_to_tptp(:o) == "$o"
    end

    test "converts :i atom to $i" do
      assert TPTP.type_to_tptp(:i) == "$i"
    end

    test "converts custom atom to string" do
      assert TPTP.type_to_tptp(:mytype) == "mytype"
    end
  end

  describe "type_to_tptp/1 - types" do
    test "converts type_o() to $o" do
      assert TPTP.type_to_tptp(type_o()) == "$o"
    end

    test "converts type_i() to $i" do
      assert TPTP.type_to_tptp(type_i()) == "$i"
    end

    test "converts function type i -> o" do
      type = mk_type(:o, [type_i()])
      result = TPTP.type_to_tptp(type)

      assert result == "$i > $o"
    end

    test "converts binary function type i -> i -> o" do
      type = mk_type(:o, [type_i(), type_i()])
      result = TPTP.type_to_tptp(type)

      assert result == "$i > $i > $o"
    end

    test "converts higher-order type (i -> o) -> o" do
      inner = mk_type(:o, [type_i()])
      type = mk_type(:o, [inner])
      result = TPTP.type_to_tptp(type)

      assert result == "($i > $o) > $o"
    end

    test "converts complex nested type" do
      # ((i -> o) -> o) -> i -> o
      inner = mk_type(:o, [mk_type(:o, [type_i()])])
      type = mk_type(:o, [inner, type_i()])
      result = TPTP.type_to_tptp(type)

      assert result == "(($i > $o) > $o) > $i > $o"
    end

    test "converts user-defined base type" do
      type = mk_type(:mytype, [])
      result = TPTP.type_to_tptp(type)

      assert result == "mytype"
    end
  end

  describe "term_to_tptp/1 - declarations" do
    test "converts bound variable declaration" do
      decl = mk_bound_var(1, type_o())
      result = TPTP.term_to_tptp(decl)

      assert result == "BV__1"
    end

    test "converts free variable declaration" do
      decl = mk_free_var("X", type_o())
      result = TPTP.term_to_tptp(decl)

      assert result == "X"
    end

    test "converts constant declaration" do
      decl = mk_const("myconst", type_i())
      result = TPTP.term_to_tptp(decl)

      assert result == "myconst"
    end

    test "converts skolem constant" do
      decl = mk_const("__sk_1", type_i())
      result = TPTP.term_to_tptp(decl)

      assert result == "skolem_sk_1"
    end
  end

  describe "term_to_tptp/1 - propositional constants" do
    test "converts true term to $true" do
      result = TPTP.term_to_tptp(true_term())

      assert result == "$true"
    end

    test "converts false term to $false" do
      result = TPTP.term_to_tptp(false_term())

      assert result == "$false"
    end
  end

  describe "term_to_tptp/1 - connectives" do
    test "converts negation" do
      inner = make_var("P", type_o())
      neg = mk_appl_term(neg_term(), inner)
      result = TPTP.term_to_tptp(neg)

      assert result == "~ P"
    end

    test "converts conjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)
      result = TPTP.term_to_tptp(conj)

      assert result == "( P & Q )"
    end

    test "converts disjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      disj = mk_appl_term(mk_appl_term(or_term(), p), q)
      result = TPTP.term_to_tptp(disj)

      assert result == "( P | Q )"
    end

    test "converts implication" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      impl = mk_appl_term(mk_appl_term(implies_term(), p), q)
      result = TPTP.term_to_tptp(impl)

      assert result == "( P => Q )"
    end

    test "converts equivalence" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      equiv = mk_appl_term(mk_appl_term(equivalent_term(), p), q)
      result = TPTP.term_to_tptp(equiv)

      assert result == "( P <=> Q )"
    end

    test "converts equality" do
      a = make_var("A", type_i())
      b = make_var("B", type_i())
      eq = mk_appl_term(mk_appl_term(equals_term(type_i()), a), b)
      result = TPTP.term_to_tptp(eq)

      assert result == "( A = B )"
    end
  end

  describe "term_to_tptp/1 - derived connectives" do
    test "converts NOR" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      # NOR is ~(P | Q)
      disj = mk_appl_term(mk_appl_term(or_term(), p), q)
      nor = mk_appl_term(neg_term(), disj)
      result = TPTP.term_to_tptp(nor)

      assert result == "( P ~| Q )"
    end

    test "converts NAND" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      # NAND is ~(P & Q)
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)
      nand = mk_appl_term(neg_term(), conj)
      result = TPTP.term_to_tptp(nand)

      assert result == "( P ~& Q )"
    end

    test "converts XOR" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      # XOR is ~(P <=> Q)
      equiv = mk_appl_term(mk_appl_term(equivalent_term(), p), q)
      xor = mk_appl_term(neg_term(), equiv)
      result = TPTP.term_to_tptp(xor)

      assert result == "( P <~> Q )"
    end

    test "converts inequality" do
      a = make_var("A", type_i())
      b = make_var("B", type_i())
      # != is ~(A = B)
      eq = mk_appl_term(mk_appl_term(equals_term(type_i()), a), b)
      neq = mk_appl_term(neg_term(), eq)
      result = TPTP.term_to_tptp(neq)

      assert result == "( A != B )"
    end
  end

  describe "term_to_tptp/1 - quantifiers" do
    test "converts universal quantification term" do
      # Use direct term construction instead of parsing
      pi = pi_term(type_o())

      result = TPTP.term_to_tptp(pi)

      assert String.contains?(result, "!")
    end

    test "converts existential quantification term" do
      sigma = sigma_term(type_o())

      result = TPTP.term_to_tptp(sigma)

      assert String.contains?(result, "?")
    end

    test "converts pi constant to !!" do
      pi = pi_term(type_o())

      result = TPTP.term_to_tptp(pi)

      # Pi term should output with !
      assert String.contains?(result, "!")
    end

    test "converts sigma constant to ??" do
      sigma = sigma_term(type_o())

      result = TPTP.term_to_tptp(sigma)

      # Sigma term should output with ?
      assert String.contains?(result, "?")
    end
  end

  describe "term_to_tptp/1 - lambda abstractions" do
    test "converts simple lambda abstraction" do
      # ^[X : o]: X
      term = Parser.parse("^[X : $o]: X")
      result = TPTP.term_to_tptp(term)

      assert String.contains?(result, "^")
      assert String.contains?(result, "$o")
    end
  end

  describe "term_to_tptp/1 - application" do
    test "converts function application" do
      # Build application term directly to avoid parser type inference
      f_const = mk_const("f", mk_type(:o, [type_i()]))
      f_term = mk_term(f_const)
      x_var = mk_free_var("X", type_i())
      x_term = mk_term(x_var)
      app_term = mk_appl_term(f_term, x_term)

      result = TPTP.term_to_tptp(app_term)

      assert String.contains?(result, "f")
      assert String.contains?(result, "@")
    end
  end

  describe "roundtrip - parse and convert back" do
    test "roundtrips $true" do
      original = "$true"
      term = Parser.parse(original)
      result = TPTP.term_to_tptp(term)

      assert result == "$true"
    end

    test "roundtrips $false" do
      original = "$false"
      term = Parser.parse(original)
      result = TPTP.term_to_tptp(term)

      assert result == "$false"
    end

    test "roundtrips boolean variable" do
      # Use boolean context to avoid unknown type issues
      ctx = Context.new() |> Context.put_var("X", type_o())
      term = Parser.parse("X", ctx)
      result = TPTP.term_to_tptp(term)

      assert result == "X"
    end

    test "roundtrips negation" do
      term = Parser.parse("~$true")
      result = TPTP.term_to_tptp(term)

      assert result == "~ $true"
    end

    test "roundtrips conjunction" do
      term = Parser.parse("$true & $false")
      result = TPTP.term_to_tptp(term)

      assert result == "( $true & $false )"
    end

    test "roundtrips disjunction" do
      term = Parser.parse("$true | $false")
      result = TPTP.term_to_tptp(term)

      assert result == "( $true | $false )"
    end

    test "roundtrips implication" do
      term = Parser.parse("$true => $false")
      result = TPTP.term_to_tptp(term)

      assert result == "( $true => $false )"
    end

    test "roundtrips equivalence" do
      term = Parser.parse("X <=> Y")
      result = TPTP.term_to_tptp(term)

      assert result == "( X <=> Y )"
    end

    test "roundtrips equality" do
      term = Parser.parse("X = Y")
      result = TPTP.term_to_tptp(term)

      assert result == "( X = Y )"
    end
  end

  describe "parse_string/2" do
    test "parses simple type declaration" do
      content = "thf(mytype_type, type, mytype : $tType)."

      {:ok, problem} = TPTP.parse_string(content)

      assert problem.types["mytype"] == :base_type
    end

    test "parses constant type declaration" do
      content = "thf(c_type, type, c : $i)."

      {:ok, problem} = TPTP.parse_string(content)

      assert problem.types["c"] == type_i()
    end

    test "parses function type declaration" do
      content = "thf(f_type, type, f : $i > $o)."

      {:ok, problem} = TPTP.parse_string(content)

      assert problem.types["f"] == mk_type(:o, [type_i()])
    end

    test "parses axiom" do
      content = """
      thf(ax1, axiom, $true).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert length(problem.axioms) == 1
      {name, _term} = hd(problem.axioms)
      assert name == "ax1"
    end

    test "parses conjecture" do
      content = """
      thf(goal, conjecture, X => X).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert problem.conjecture != nil
      {name, _term} = problem.conjecture
      assert name == "goal"
    end

    test "parses multiple entries" do
      content = """
      thf(t_type, type, t : $tType).
      thf(c_type, type, c : t).
      thf(ax1, axiom, c = c).
      thf(goal, conjecture, c = c).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert map_size(problem.types) == 2
      assert length(problem.axioms) == 1
      assert problem.conjecture != nil
    end

    test "stores problem path" do
      content = "thf(ax, axiom, $true)."

      {:ok, problem} = TPTP.parse_string(content, "test.p")

      assert problem.path == "test.p"
    end

    test "uses default path 'memory'" do
      content = "thf(ax, axiom, $true)."

      {:ok, problem} = TPTP.parse_string(content)

      assert problem.path == "memory"
    end

    test "parses definition" do
      content = """
      thf(c_type, type, c : $o).
      thf(c_def, definition, c = $true).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert map_size(problem.definitions) == 1
      assert Map.has_key?(problem.definitions, "c_def")
    end

    test "ignores comments in content" do
      content = """
      % This is a comment
      thf(ax, axiom, $true).
      % Another comment
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert length(problem.axioms) == 1
    end
  end

  describe "parse_string/2 - complex problems" do
    test "parses problem with typed axioms" do
      content = """
      thf(a_type, type, a : $i).
      thf(b_type, type, b : $i).
      thf(reflexivity, axiom, ![X : $i]: X = X).
      thf(goal, conjecture, a = a).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert map_size(problem.types) == 2
      assert length(problem.axioms) == 1
      assert problem.conjecture != nil
    end

    test "parses problem with function constants" do
      content = """
      thf(f_type, type, f : $i > $i).
      thf(g_type, type, g : $i > $i).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert problem.types["f"] == mk_type(:i, [type_i()])
      assert problem.types["g"] == mk_type(:i, [type_i()])
    end

    test "parses problem with binary predicates" do
      content = """
      thf(r_type, type, r : $i > $i > $o).
      thf(symmetry, axiom, ![X : $i, Y : $i]: (r @ X @ Y) => (r @ Y @ X)).
      """

      {:ok, problem} = TPTP.parse_string(content)

      expected_type = mk_type(:o, [type_i(), type_i()])
      assert problem.types["r"] == expected_type
    end
  end

  describe "Problem structure" do
    test "empty problem has correct defaults" do
      {:ok, problem} = TPTP.parse_string("")

      assert problem.path == "memory"
      assert problem.includes == []
      assert problem.types == %{}
      assert problem.definitions == %{}
      assert problem.axioms == []
      assert problem.conjecture == nil
    end

    test "problem accumulates multiple axioms" do
      content = """
      thf(ax1, axiom, $true = $true).
      thf(ax2, axiom, $true | ~$true).
      thf(ax3, axiom, ~($true & ~$true)).
      """

      {:ok, problem} = TPTP.parse_string(content)

      assert length(problem.axioms) == 3

      axiom_names = Enum.map(problem.axioms, fn {name, _} -> name end)
      assert "ax1" in axiom_names
      assert "ax2" in axiom_names
      assert "ax3" in axiom_names
    end

    test "problem preserves axiom order" do
      content = """
      thf(first, axiom, $true).
      thf(second, axiom, $false).
      thf(third, axiom, $true & $true).
      """

      {:ok, problem} = TPTP.parse_string(content)

      names = Enum.map(problem.axioms, fn {name, _} -> name end)
      assert names == ["first", "second", "third"]
    end
  end
end
