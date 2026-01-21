defmodule BeHOLd.ClassicalHOL.PatternsTest do
  @moduledoc """
  Tests for the BeHOLd.ClassicalHOL.Patterns module.
  """
  use ExUnit.Case, async: true

  import HOL.Data
  import HOL.Terms
  import BeHOLd.ClassicalHOL.Definitions
  import BeHOLd.ClassicalHOL.Patterns

  # Helper to create test terms using HOL.Terms
  defp make_var(name, type) do
    mk_term(mk_free_var(name, type))
  end

  describe "negated/1 pattern" do
    test "matches a negated term" do
      inner = make_var("P", type_o())
      neg = mk_appl_term(neg_term(), inner)

      assert match?(negated(_), neg)
    end

    test "extracts inner term from negation" do
      inner = make_var("P", type_o())
      neg = mk_appl_term(neg_term(), inner)

      negated(extracted) = neg
      assert extracted == inner
    end

    test "does not match non-negated term" do
      term = make_var("P", type_o())

      refute match?(negated(_), term)
    end

    test "does not match conjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)

      refute match?(negated(_), conj)
    end
  end

  describe "conjunction/2 pattern" do
    test "matches a conjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)

      assert match?(conjunction(_, _), conj)
    end

    test "extracts both conjuncts" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)

      conjunction(left, right) = conj
      assert left == p
      assert right == q
    end

    test "does not match disjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      disj = mk_appl_term(mk_appl_term(or_term(), p), q)

      refute match?(conjunction(_, _), disj)
    end

    test "does not match simple variable" do
      p = make_var("P", type_o())

      refute match?(conjunction(_, _), p)
    end
  end

  describe "disjunction/2 pattern" do
    test "matches a disjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      disj = mk_appl_term(mk_appl_term(or_term(), p), q)

      assert match?(disjunction(_, _), disj)
    end

    test "extracts both disjuncts" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      disj = mk_appl_term(mk_appl_term(or_term(), p), q)

      disjunction(left, right) = disj
      assert left == p
      assert right == q
    end

    test "does not match conjunction" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)

      refute match?(disjunction(_, _), conj)
    end
  end

  describe "implication/2 pattern" do
    test "matches an implication" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      impl = mk_appl_term(mk_appl_term(implies_term(), p), q)

      assert match?(implication(_, _), impl)
    end

    test "extracts antecedent and consequent" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      impl = mk_appl_term(mk_appl_term(implies_term(), p), q)

      implication(ante, conseq) = impl
      assert ante == p
      assert conseq == q
    end

    test "does not match equivalence" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      equiv = mk_appl_term(mk_appl_term(equivalent_term(), p), q)

      refute match?(implication(_, _), equiv)
    end
  end

  describe "equivalence/2 pattern" do
    test "matches an equivalence" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      equiv = mk_appl_term(mk_appl_term(equivalent_term(), p), q)

      assert match?(equivalence(_, _), equiv)
    end

    test "extracts both sides" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      equiv = mk_appl_term(mk_appl_term(equivalent_term(), p), q)

      equivalence(left, right) = equiv
      assert left == p
      assert right == q
    end

    test "does not match implication" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      impl = mk_appl_term(mk_appl_term(implies_term(), p), q)

      refute match?(equivalence(_, _), impl)
    end
  end

  describe "equality/2 pattern" do
    test "matches equality of type o" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      eq = mk_appl_term(mk_appl_term(equals_term(type_o()), p), q)

      assert match?(equality(_, _), eq)
    end

    test "matches equality of type i" do
      a = make_var("A", type_i())
      b = make_var("B", type_i())
      eq = mk_appl_term(mk_appl_term(equals_term(type_i()), a), b)

      assert match?(equality(_, _), eq)
    end

    test "extracts both sides of equality" do
      a = make_var("A", type_i())
      b = make_var("B", type_i())
      eq = mk_appl_term(mk_appl_term(equals_term(type_i()), a), b)

      equality(left, right) = eq
      assert left == a
      assert right == b
    end

    test "does not match equivalence" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      equiv = mk_appl_term(mk_appl_term(equivalent_term(), p), q)

      refute match?(equality(_, _), equiv)
    end
  end

  describe "typed_equality/3 pattern" do
    test "matches equality with specific type o" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      eq = mk_appl_term(mk_appl_term(equals_term(type_o()), p), q)

      assert match?(typed_equality(_, _, type_o()), eq)
    end

    test "matches equality with specific type i" do
      a = make_var("A", type_i())
      b = make_var("B", type_i())
      eq = mk_appl_term(mk_appl_term(equals_term(type_i()), a), b)

      assert match?(typed_equality(_, _, type_i()), eq)
    end

    test "does not match equality of different type" do
      a = make_var("A", type_i())
      b = make_var("B", type_i())
      eq = mk_appl_term(mk_appl_term(equals_term(type_i()), a), b)

      # Try to match with type_o - should fail
      refute match?(typed_equality(_, _, type_o()), eq)
    end
  end

  describe "existential_quantification/1 pattern" do
    test "matches existential quantification" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      p = make_var("P", pred_type)
      exists = mk_appl_term(sigma_term(element_type), p)

      assert match?(existential_quantification(_), exists)
    end

    test "extracts the predicate" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      p = make_var("P", pred_type)
      exists = mk_appl_term(sigma_term(element_type), p)

      existential_quantification(pred) = exists
      assert pred == p
    end

    test "does not match universal quantification" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      p = make_var("P", pred_type)
      forall = mk_appl_term(pi_term(element_type), p)

      refute match?(existential_quantification(_), forall)
    end
  end

  describe "typed_existential_quantification/2 pattern" do
    test "matches existential when constructing directly" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      # Construct the pattern directly instead of via mk_appl_term
      exists = hol_term(bvars: [], head: sigma_const(pred_type), args: [make_var("P", pred_type)])

      assert match?(typed_existential_quantification(_, ^pred_type), exists)
    end
  end

  describe "universal_quantification/1 pattern" do
    test "matches universal quantification" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      p = make_var("P", pred_type)
      forall = mk_appl_term(pi_term(element_type), p)

      assert match?(universal_quantification(_), forall)
    end

    test "extracts the predicate" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      p = make_var("P", pred_type)
      forall = mk_appl_term(pi_term(element_type), p)

      universal_quantification(pred) = forall
      assert pred == p
    end

    test "does not match existential quantification" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      p = make_var("P", pred_type)
      exists = mk_appl_term(sigma_term(element_type), p)

      refute match?(universal_quantification(_), exists)
    end
  end

  describe "typed_universal_quantification/2 pattern" do
    test "matches universal when constructing directly" do
      element_type = type_i()
      pred_type = mk_type(:o, [element_type])
      # Construct the pattern directly instead of via mk_appl_term
      forall = hol_term(bvars: [], head: pi_const(pred_type), args: [make_var("P", pred_type)])

      assert match?(typed_universal_quantification(_, ^pred_type), forall)
    end
  end

  describe "pattern matching in case statements" do
    test "can use patterns in case expressions" do
      p = make_var("P", type_o())
      q = make_var("Q", type_o())
      conj = mk_appl_term(mk_appl_term(and_term(), p), q)

      result =
        case conj do
          negated(_) -> :negation
          conjunction(_, _) -> :conjunction
          disjunction(_, _) -> :disjunction
          _ -> :other
        end

      assert result == :conjunction
    end

    test "can use patterns to deconstruct nested formulas" do
      p = make_var("P", type_o())
      neg_p = mk_appl_term(neg_term(), p)
      q = make_var("Q", type_o())
      impl = mk_appl_term(mk_appl_term(implies_term(), neg_p), q)

      implication(ante, _conseq) = impl
      assert match?(negated(_), ante)

      negated(inner) = ante
      assert inner == p
    end
  end

  describe "patterns in function heads" do
    test "can define function clauses with patterns" do
      # Define a helper that uses patterns
      formula_type = fn
        term when is_tuple(term) ->
          case term do
            negated(_) -> :negation
            conjunction(_, _) -> :conjunction
            disjunction(_, _) -> :disjunction
            implication(_, _) -> :implication
            equivalence(_, _) -> :equivalence
            equality(_, _) -> :equality
            _ -> :other
          end

        _ ->
          :unknown
      end

      p = make_var("P", type_o())
      q = make_var("Q", type_o())

      assert formula_type.(mk_appl_term(neg_term(), p)) == :negation
      assert formula_type.(mk_appl_term(mk_appl_term(and_term(), p), q)) == :conjunction
      assert formula_type.(mk_appl_term(mk_appl_term(or_term(), p), q)) == :disjunction
      assert formula_type.(mk_appl_term(mk_appl_term(implies_term(), p), q)) == :implication
      assert formula_type.(mk_appl_term(mk_appl_term(equivalent_term(), p), q)) == :equivalence
    end
  end
end
