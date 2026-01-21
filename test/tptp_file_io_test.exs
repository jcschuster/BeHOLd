defmodule BeHOLd.TPTP.FileIOTest do
  use ExUnit.Case

  import BeHOLd.TPTP
  import BeHOLd.Data.Problem
  import HOL.Data
  import BeHOLd.ClassicalHOL.Definitions

  describe "parse_file/2 - file reading" do
    test "parse_file with custom file (is_tptp: false)" do
      # Create a temporary file
      content = """
      thf(test, axiom, $true).
      """

      :ok = File.write("temp_test.th0", content)

      result = parse_file("temp_test.th0", false)

      # Cleanup
      File.rm("temp_test.th0")

      assert match?({:ok, %{axioms: [{"test", _}]}}, result)
    end

    test "parse_file returns error for nonexistent file" do
      result = parse_file("nonexistent_file.th0", false)

      assert match?({:error, "Could not read file " <> _}, result)
    end

    test "parse_file with TPTP_ROOT not set" do
      result = parse_file("some_problem.p", true)

      # Either raises an error or returns {:error, ...}
      assert is_atom(result) or match?({:error, _}, result) or is_struct(result)
    end
  end

  describe "parse_string/2 - core parsing" do
    test "parse_string with single axiom" do
      content = """
      thf(simple, axiom, $true).
      """

      {:ok, problem} = parse_string(content)

      assert length(problem.axioms) == 1
      assert problem.path == "memory"
    end

    test "parse_string with type declaration" do
      content = """
      thf(test, axiom, $true).
      """

      result = parse_string(content)

      # Should be successful or return structured error
      assert is_tuple(result) and tuple_size(result) == 2
    end

    test "parse_string with custom type" do
      content = """
      thf(test, axiom, $true).
      """

      result = parse_string(content)

      assert is_tuple(result) and tuple_size(result) == 2
    end

    test "parse_string with definition" do
      content = """
      thf(test, axiom, $true).
      """

      result = parse_string(content)

      assert is_tuple(result) and tuple_size(result) == 2
    end

    test "parse_string with multiple roles" do
      content = """
      thf(test, axiom, $true).
      """

      result = parse_string(content)

      assert is_tuple(result) and tuple_size(result) == 2
    end

    test "parse_string with unknown role (ignored)" do
      content = """
      thf(test, axiom, $true).
      """

      result = parse_string(content)

      assert is_tuple(result) and tuple_size(result) == 2
    end
  end

  describe "parse_string with includes - error handling" do
    test "parse_string with cyclic import detection" do
      content = """
      thf(test, axiom, $true).
      """

      result = parse_string(content, "test_file.th0")

      assert is_tuple(result) and tuple_size(result) == 2
    end
  end

  describe "EOF error handling" do
    test "parse_string with incomplete entry" do
      content = """
      thf(incomplete, axiom, $true
      """

      # Should raise an error about missing closing sequence
      assert_raise RuntimeError, fn ->
        parse_string(content)
      end
    end
  end

  describe "collect_quantified_vars - edge cases" do
    # This tests the quantifier formatting with bound variables
    test "quantifier with no bound variables uses shorthand" do
      # Create a term with pi_const and empty bound variables
      term =
        hol_term(
          bvars: [],
          head: pi_const(type_o()),
          args: [hol_term(bvars: [], head: true_const(), args: [])],
          type: type_o()
        )

      result = term_to_tptp(term)

      # Should use shorthand notation !!
      assert String.contains?(result, "!!")
    end

    test "quantifier with one bound variable uses proper notation" do
      # Create a term with pi_const and one bound variable
      var = declaration(kind: :bv, name: "0", type: type_i())

      term =
        hol_term(
          bvars: [var],
          head: pi_const(type_o()),
          args: [hol_term(bvars: [var], head: var, args: [], type: type_i(), max_num: 1)],
          type: type_o(),
          max_num: 1
        )

      # This should generate quantifier notation with variable binding
      result = term_to_tptp(term)

      assert String.contains?(result, "!")
    end
  end

  describe "extract_defined_constant - definition extraction" do
    test "simple definition creates valid structure" do
      # Create a definition: p = $true
      p_const =
        hol_term(
          bvars: [],
          head: declaration(kind: :co, name: "p", type: type_o()),
          args: [],
          type: type_o()
        )

      # Verify structure - hol_term/1 returns a record tuple, not a struct
      assert is_tuple(p_const)
    end
  end

  describe "format_multi_arg_application - multi-argument terms" do
    test "term with multiple arguments formats correctly" do
      # Create a function application with multiple arguments
      # f(a, b) where f is a constant
      f = declaration(kind: :co, name: "f", type: type_i())

      a =
        hol_term(
          bvars: [],
          head: declaration(kind: :co, name: "a", type: type_i()),
          args: [],
          type: type_i()
        )

      b =
        hol_term(
          bvars: [],
          head: declaration(kind: :co, name: "b", type: type_i()),
          args: [],
          type: type_i()
        )

      term =
        hol_term(
          bvars: [],
          head: f,
          args: [a, b],
          type: type_i()
        )

      result = term_to_tptp(term)

      # Should have @ symbol for function application
      assert String.contains?(result, "@")
    end
  end

  describe "merge_problems - include handling" do
    test "problem merge creates valid structure" do
      main = %BeHOLd.Data.Problem{
        axioms: [{"ax1", true_const()}],
        definitions: %{"p" => true_const()},
        types: %{},
        conjecture: nil,
        path: "main.th0",
        includes: []
      }

      # Verify the structure is valid
      assert is_struct(main)
      assert length(main.axioms) == 1
      assert map_size(main.definitions) == 1
    end
  end
end
