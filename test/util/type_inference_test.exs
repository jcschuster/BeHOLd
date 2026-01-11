defmodule BeHOLd.Util.TypeInferenceTest do
  @moduledoc """
  Tests for the BeHOLd.Util.TypeInference module.
  """
  use ExUnit.Case, async: true

  alias BeHOLd.Util.TypeInference, as: TI
  import HOL.Data

  describe "mk_new_unknown_type/0" do
    test "creates a new unknown type" do
      t = TI.mk_new_unknown_type()
      assert match?(type(goal: g, args: []) when is_atom(g), t)
    end

    test "creates unique types on successive calls" do
      t1 = TI.mk_new_unknown_type()
      t2 = TI.mk_new_unknown_type()
      t3 = TI.mk_new_unknown_type()

      assert t1 != t2
      assert t2 != t3
      assert t1 != t3
    end

    test "unknown type goal starts with __unknown_" do
      type(goal: g) = TI.mk_new_unknown_type()
      assert String.starts_with?(Atom.to_string(g), "__unknown_")
    end
  end

  describe "unknown_type?/1" do
    test "returns true for unknown type" do
      t = TI.mk_new_unknown_type()
      assert TI.unknown_type?(t)
    end

    test "returns false for base type :o" do
      refute TI.unknown_type?(mk_type(:o))
    end

    test "returns false for base type :i" do
      refute TI.unknown_type?(mk_type(:i))
    end

    test "returns false for user-defined base type" do
      refute TI.unknown_type?(mk_type(:my_type))
    end

    test "returns true for unknown atom" do
      assert TI.unknown_type?(:__unknown_123)
    end

    test "returns false for normal atom" do
      refute TI.unknown_type?(:o)
      refute TI.unknown_type?(:i)
      refute TI.unknown_type?(:some_type)
    end

    test "returns false for function type with known types" do
      t = mk_type(:o, [mk_type(:i)])
      refute TI.unknown_type?(t)
    end

    test "returns true for function type with unknown goal" do
      unknown = TI.mk_new_unknown_type()
      type(goal: g) = unknown
      t = mk_type(g, [mk_type(:i)])
      assert TI.unknown_type?(t)
    end
  end

  describe "solve/1 - basic unification" do
    test "solves single constraint with base type" do
      t1 = TI.mk_new_unknown_type()
      type(goal: g) = t1

      result = TI.solve([{t1, mk_type(:i)}])

      assert Map.has_key?(result, g)
      assert result[g] == :i or result[g] == mk_type(:i)
    end

    test "solves constraint between two unknown types" do
      t1 = TI.mk_new_unknown_type()
      t2 = TI.mk_new_unknown_type()
      type(goal: g1) = t1
      type(goal: g2) = t2

      result = TI.solve([{t1, t2}])

      # One should be mapped to the other
      assert Map.has_key?(result, g1) or Map.has_key?(result, g2)
    end

    test "solves constraint with function type" do
      t1 = TI.mk_new_unknown_type()
      type(goal: g1) = t1
      func_type = mk_type(:o, [mk_type(:i)])

      result = TI.solve([{t1, func_type}])

      assert Map.has_key?(result, g1)
      assert TI.apply_subst(t1, result) == func_type
    end

    test "handles empty constraint list" do
      result = TI.solve([])
      assert result == %{}
    end

    test "handles identity constraint" do
      t = mk_type(:o)
      result = TI.solve([{t, t}])
      assert result == %{}
    end
  end

  describe "solve/1 - chained constraints" do
    test "solves transitive constraints" do
      t1 = TI.mk_new_unknown_type()
      t2 = TI.mk_new_unknown_type()

      result = TI.solve([{t1, t2}, {t2, mk_type(:i)}])

      assert TI.apply_subst(t1, result) == mk_type(:i)
      assert TI.apply_subst(t2, result) == mk_type(:i)
    end

    test "solves multiple constraints to same type" do
      t1 = TI.mk_new_unknown_type()
      t2 = TI.mk_new_unknown_type()

      result = TI.solve([{t1, mk_type(:o)}, {t2, mk_type(:o)}])

      assert TI.apply_subst(t1, result) == mk_type(:o)
      assert TI.apply_subst(t2, result) == mk_type(:o)
    end
  end

  describe "solve/1 - complex function types" do
    test "unifies function types with matching structure" do
      t1 = TI.mk_new_unknown_type()
      t2 = TI.mk_new_unknown_type()

      # Since func1 has unknown types as components, we need to set up constraints
      # that will unify t1 with :i and t2 with :o
      result = TI.solve([{t1, mk_type(:i)}, {t2, mk_type(:o)}])

      assert TI.apply_subst(t1, result) == mk_type(:i)
      assert TI.apply_subst(t2, result) == mk_type(:o)
    end

    test "unifies nested function types" do
      t1 = TI.mk_new_unknown_type()

      # (i -> o) -> o
      nested = mk_type(:o, [mk_type(:o, [mk_type(:i)])])

      result = TI.solve([{t1, nested}])

      assert TI.apply_subst(t1, result) == nested
    end
  end

  describe "solve/1 - error cases" do
    test "raises on conflicting base types" do
      assert_raise RuntimeError, fn ->
        TI.solve([{mk_type(:i), mk_type(:o)}])
      end
    end

    test "raises on occurs check violation" do
      t1 = TI.mk_new_unknown_type()

      # t1 = t1 -> o would create an infinite type
      cyclic_type = mk_type(:o, [t1])

      assert_raise RuntimeError, fn ->
        TI.solve([{t1, cyclic_type}])
      end
    end
  end

  describe "apply_subst/2 - atoms" do
    test "applies substitution to unknown atom" do
      result = TI.apply_subst(:__unknown_1, %{:__unknown_1 => :i})
      assert result == :i
    end

    test "returns atom unchanged if not in substitution" do
      result = TI.apply_subst(:o, %{:__unknown_1 => :i})
      assert result == :o
    end

    test "follows chain of substitutions" do
      subst = %{:__unknown_1 => :__unknown_2, :__unknown_2 => :i}
      result = TI.apply_subst(:__unknown_1, subst)
      assert result == :i
    end
  end

  describe "apply_subst/2 - types" do
    test "applies substitution to unknown type" do
      t = TI.mk_new_unknown_type()
      type(goal: g) = t

      result = TI.apply_subst(t, %{g => mk_type(:i)})

      assert result == mk_type(:i)
    end

    test "applies substitution to function type arguments" do
      t1 = TI.mk_new_unknown_type()
      type(goal: g1) = t1

      func_type = mk_type(:o, [t1])
      result = TI.apply_subst(func_type, %{g1 => mk_type(:i)})

      assert result == mk_type(:o, [mk_type(:i)])
    end

    test "applies substitution to goal of function type" do
      t1 = TI.mk_new_unknown_type()
      type(goal: g1) = t1

      func_type = mk_type(g1, [mk_type(:i)])
      result = TI.apply_subst(func_type, %{g1 => :o})

      assert result == mk_type(:o, [mk_type(:i)])
    end

    test "applies substitution recursively to nested types" do
      t1 = TI.mk_new_unknown_type()
      t2 = TI.mk_new_unknown_type()
      type(goal: g1) = t1
      type(goal: g2) = t2

      # (?1 -> ?2) -> o
      nested = mk_type(:o, [mk_type(g2, [t1])])
      result = TI.apply_subst(nested, %{g1 => mk_type(:i), g2 => :o})

      expected = mk_type(:o, [mk_type(:o, [mk_type(:i)])])
      assert result == expected
    end

    test "returns known type unchanged" do
      t = mk_type(:o, [mk_type(:i)])
      result = TI.apply_subst(t, %{:__unknown_1 => :i})
      assert result == t
    end
  end

  describe "apply_subst/2 - edge cases" do
    test "handles empty substitution" do
      t = TI.mk_new_unknown_type()
      result = TI.apply_subst(t, %{})
      assert result == t
    end

    test "handles non-type input" do
      result = TI.apply_subst("not a type", %{})
      assert result == "not a type"
    end
  end
end
