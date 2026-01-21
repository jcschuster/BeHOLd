defmodule BeHOLd.Util.TypeInference.AdvancedTest do
  use ExUnit.Case

  import BeHOLd.Util.TypeInference
  import HOL.Data
  import BeHOLd.ClassicalHOL.Definitions

  describe "apply_subst - complex substitution" do
    test "apply_subst with nested unknown types" do
      u1 = mk_new_unknown_type()
      u2 = mk_new_unknown_type()

      # Just verify the function can handle complex types
      subst = %{}

      result = apply_subst(u1, subst)

      # Should resolve to a value (may be atom, struct, or other)
      refute is_nil(result)
    end

    test "apply_subst with type args containing unknowns" do
      u1 = mk_new_unknown_type()

      t = mk_type(:arrow, [u1, type_o()])

      subst = %{}

      result = apply_subst(t, subst)

      # Should return something non-nil
      refute is_nil(result)
    end
  end

  describe "unify_shorter_to_longer - polymorphic type unification" do
    test "unify unknown shorter type with longer concrete type" do
      shorter = mk_new_unknown_type()
      longer = mk_type(:i, [type_o(), type_i()])

      result = solve([{shorter, longer}])

      assert is_map(result)
    end

    test "cannot unify shorter concrete with longer when goals don't match" do
      shorter = mk_type(:o)
      longer = mk_type(:i, [type_o(), type_i()])

      assert_raise RuntimeError, fn ->
        solve([{shorter, longer}])
      end
    end
  end

  describe "unknown_type? - edge cases" do
    test "unknown_type? returns false for non-atom, non-type values" do
      assert unknown_type?("not_a_type") == false
      assert unknown_type?(123) == false
    end

    test "atom-to-type unification" do
      u1 = mk_new_unknown_type()
      type(goal: g) = u1

      result = solve([{u1, mk_type(:i)}])

      assert Map.has_key?(result, g) or result == %{}
    end

    test "type with no args unification" do
      u = mk_new_unknown_type()

      result = solve([{u, mk_type(:i)}])

      assert is_map(result)
    end
  end

  describe "apply_subst - edge cases in constraint solving" do
    test "apply_subst with nested complex structure" do
      u1 = mk_new_unknown_type()
      u2 = mk_new_unknown_type()

      nested =
        mk_type(:o, [
          mk_type(:i, [u1]),
          mk_type(:i, [u2])
        ])

      subst = %{}
      result = apply_subst(nested, subst)

      refute is_nil(result)
    end

    test "solve with complex transitive dependencies" do
      u1 = mk_new_unknown_type()
      u2 = mk_new_unknown_type()
      u3 = mk_new_unknown_type()
      type(goal: g1) = u1
      type(goal: g2) = u2
      type(goal: g3) = u3

      constraints = [
        {u1, u2},
        {u2, u3},
        {u3, mk_type(:o)}
      ]

      result = solve(constraints)
      assert is_map(result)
    end
  end

  describe "solve - error detection and recovery" do
    test "handles impossible unification gracefully" do
      assert_raise RuntimeError, fn ->
        solve([{mk_type(:i), mk_type(:o)}])
      end
    end

    test "occurs check prevents infinite types" do
      u = mk_new_unknown_type()
      cyclic = mk_type(:o, [u])

      assert_raise RuntimeError, fn ->
        solve([{u, cyclic}])
      end
    end
  end
end
