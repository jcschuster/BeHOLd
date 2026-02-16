defmodule BeHOLd.ClassicalHOL.DefinitionsTest do
  @moduledoc """
  Tests for the BeHOLd.ClassicalHOL.Definitions module.
  """
  use ExUnit.Case, async: true

  import HOL.Data
  import BeHOLd.ClassicalHOL.Definitions

  describe "signature_symbols/0" do
    test "returns all signature symbols" do
      symbols = signature_symbols()

      assert is_list(symbols)
      assert length(symbols) == 10
    end

    test "contains truth and falsity symbols" do
      symbols = signature_symbols()

      assert "⊤" in symbols
      assert "⊥" in symbols
    end

    test "contains logical connectives" do
      symbols = signature_symbols()

      assert "¬" in symbols
      assert "∨" in symbols
      assert "∧" in symbols
      assert "⊃" in symbols
      assert "≡" in symbols
    end

    test "contains quantifiers" do
      symbols = signature_symbols()

      assert "Π" in symbols
      assert "Σ" in symbols
    end

    test "contains equality" do
      symbols = signature_symbols()

      assert "=" in symbols
    end
  end

  describe "type macros" do
    test "type_o creates boolean type" do
      t = type_o()

      assert match?(type(goal: :o, args: []), t)
    end

    test "type_i creates individual type" do
      t = type_i()

      assert match?(type(goal: :i, args: []), t)
    end

    test "type_oo creates unary connective type" do
      t = type_oo()

      assert match?(type(goal: :o, args: [type(goal: :o, args: [])]), t)
    end

    test "type_ooo creates binary connective type" do
      t = type_ooo()

      assert match?(type(goal: :o, args: [type(goal: :o, args: []), type(goal: :o, args: [])]), t)
    end

    test "type_io creates predicate over individuals type" do
      t = type_io()

      assert match?(type(goal: :o, args: [type(goal: :i, args: [])]), t)
    end

    test "type_io_o creates higher-order predicate type" do
      t = type_io_o()

      type(goal: goal, args: args) = t
      assert goal == :o
      assert length(args) == 1
    end
  end

  describe "constant macros - propositional" do
    test "true_const creates truth constant" do
      c = true_const()

      assert match?(declaration(kind: :co, name: "⊤", type: type(goal: :o, args: [])), c)
    end

    test "false_const creates falsity constant" do
      c = false_const()

      assert match?(declaration(kind: :co, name: "⊥", type: type(goal: :o, args: [])), c)
    end

    test "neg_const creates negation constant" do
      c = neg_const()

      assert match?(declaration(kind: :co, name: "¬"), c)
      declaration(type: t) = c
      assert t == type_oo()
    end

    test "or_const creates disjunction constant" do
      c = or_const()

      assert match?(declaration(kind: :co, name: "∨"), c)
      declaration(type: t) = c
      assert t == type_ooo()
    end

    test "and_const creates conjunction constant" do
      c = and_const()

      assert match?(declaration(kind: :co, name: "∧"), c)
      declaration(type: t) = c
      assert t == type_ooo()
    end

    test "implies_const creates implication constant" do
      c = implies_const()

      assert match?(declaration(kind: :co, name: "⊃"), c)
      declaration(type: t) = c
      assert t == type_ooo()
    end

    test "equivalent_const creates equivalence constant" do
      c = equivalent_const()

      assert match?(declaration(kind: :co, name: "≡"), c)
      declaration(type: t) = c
      assert t == type_ooo()
    end
  end

  describe "constant macros - polymorphic" do
    test "equals_const creates equality constant for type o" do
      c = equals_const(type_o())

      assert match?(declaration(kind: :co, name: "="), c)
      declaration(type: t) = c
      # Type should be o -> o -> o
      assert match?(type(goal: :o, args: [_, _]), t)
    end

    test "equals_const creates equality constant for type i" do
      c = equals_const(type_i())

      declaration(type: t) = c
      type(args: [arg1, arg2]) = t
      assert arg1 == type_i()
      assert arg2 == type_i()
    end

    test "equals_const creates equality constant for custom type" do
      custom_type = mk_type(:person)
      c = equals_const(custom_type)

      declaration(type: t) = c
      type(goal: goal, args: args) = t
      assert goal == :o
      assert length(args) == 2
      assert Enum.all?(args, &(&1 == custom_type))
    end

    test "equals_const creates equality constant for function type" do
      func_type = mk_type(:o, [type_i()])
      c = equals_const(func_type)

      declaration(type: t, name: name) = c
      assert name == "="
      type(goal: goal, args: args) = t
      assert goal == :o
      assert length(args) == 2
      assert Enum.all?(args, &(&1 == func_type))
    end

    test "equals_const preserves type in declaration" do
      t1 = type_i()
      c1 = equals_const(t1)

      t2 = type_o()
      c2 = equals_const(t2)

      declaration(type: type1) = c1
      declaration(type: type2) = c2

      refute type1 == type2
    end

    test "pi_const creates universal quantification constant" do
      pred_type = mk_type(:o, [type_i()])
      c = pi_const(pred_type)

      assert match?(declaration(kind: :co, name: "Π"), c)
    end

    test "sigma_const creates existential quantification constant" do
      pred_type = mk_type(:o, [type_o()])
      c = sigma_const(pred_type)

      assert match?(declaration(kind: :co, name: "Σ"), c)
    end
  end

  describe "term macros - basic terms" do
    test "true_term creates truth term" do
      t = true_term()

      assert match?(hol_term(head: declaration(name: "⊤"), args: [], type: type(goal: :o)), t)
    end

    test "false_term creates falsity term" do
      t = false_term()

      assert match?(hol_term(head: declaration(name: "⊥"), args: [], type: type(goal: :o)), t)
    end

    test "neg_term creates negation term" do
      t = neg_term()

      assert match?(hol_term(head: declaration(name: "¬")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 1
    end

    test "or_term creates disjunction term" do
      t = or_term()

      assert match?(hol_term(head: declaration(name: "∨")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
    end

    test "and_term creates conjunction term" do
      t = and_term()

      assert match?(hol_term(head: declaration(name: "∧")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
    end

    test "implies_term creates implication term" do
      t = implies_term()

      assert match?(hol_term(head: declaration(name: "⊃")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
    end

    test "equivalent_term creates equivalence term" do
      t = equivalent_term()

      assert match?(hol_term(head: declaration(name: "≡")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
    end
  end

  describe "term macros - polymorphic terms" do
    test "equals_term creates equality term for type o" do
      t = equals_term(type_o())

      hol_term(head: head, bvars: bvars) = t
      assert match?(declaration(name: "="), head)
      assert length(bvars) == 2
    end

    test "equals_term creates equality term for type i" do
      t = equals_term(type_i())

      hol_term(bvars: bvars) = t
      [bv1, bv2] = bvars
      declaration(type: t1) = bv1
      declaration(type: t2) = bv2
      assert t1 == type_i()
      assert t2 == type_i()
    end

    test "equals_term creates equality term for custom type" do
      custom_type = mk_type(:entity)
      t = equals_term(custom_type)

      hol_term(head: head, bvars: bvars) = t
      assert match?(declaration(name: "="), head)
      assert length(bvars) == 2

      [bv1, bv2] = bvars
      declaration(type: t1) = bv1
      declaration(type: t2) = bv2
      assert t1 == custom_type
      assert t2 == custom_type
    end

    test "equals_term creates equality term for function type" do
      func_type = mk_type(:i, [type_i()])
      t = equals_term(func_type)

      hol_term(bvars: bvars, type: result_type) = t
      assert length(bvars) == 2
      # Type should be func_type -> func_type -> o
      expected_type = mk_type(:o, [func_type, func_type])
      assert result_type == expected_type
    end

    test "equals_term bound variables have correct indices" do
      t = equals_term(type_i())

      hol_term(bvars: bvars) = t
      [bv1, bv2] = bvars
      declaration(name: n1) = bv1
      declaration(name: n2) = bv2
      assert n1 == 2
      assert n2 == 1
    end

    test "equals_term has correct argument structure" do
      t = equals_term(type_o())

      hol_term(args: args) = t
      assert length(args) == 2

      # Arguments should be bound variables
      Enum.each(args, fn arg ->
        assert match?(hol_term(head: declaration(kind: :bv)), arg)
      end)
    end

    test "pi_term creates universal quantification term" do
      pred_type = mk_type(:o, [type_i()])
      t = pi_term(pred_type)

      hol_term(head: head, bvars: bvars) = t
      assert match?(declaration(name: "Π"), head)
      assert length(bvars) == 1
    end

    test "sigma_term creates existential quantification term" do
      pred_type = mk_type(:o, [type_o()])
      t = sigma_term(pred_type)

      hol_term(head: head, bvars: bvars) = t
      assert match?(declaration(name: "Σ"), head)
      assert length(bvars) == 1
    end
  end

  describe "term macros - derived operators" do
    test "implied_by_term creates reverse implication term" do
      t = implied_by_term()

      # Reverse implication is defined using implication with swapped args
      assert match?(hol_term(head: declaration(name: "⊃")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
    end

    test "xor_term creates exclusive or term" do
      t = xor_term()

      # XOR is defined as negated equivalence
      assert match?(hol_term(head: declaration(name: "¬")), t)
    end

    test "nand_term creates NAND term" do
      t = nand_term()

      # NAND is negated conjunction
      assert match?(hol_term(head: declaration(name: "¬")), t)
    end

    test "nor_term creates NOR term" do
      t = nor_term()

      # NOR is negated disjunction
      assert match?(hol_term(head: declaration(name: "¬")), t)
    end

    test "not_equals_term creates inequality term" do
      t = not_equals_term(type_o())

      # Inequality is negated equality
      assert match?(hol_term(head: declaration(name: "¬")), t)
      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
    end

    test "not_equals_term creates inequality for custom type" do
      custom_type = mk_type(:color)
      t = not_equals_term(custom_type)

      # Inequality is negated equality
      assert match?(hol_term(head: declaration(name: "¬")), t)
      hol_term(args: [inner]) = t
      assert match?(hol_term(head: declaration(name: "=")), inner)
    end

    test "not_equals_term bound variables match type" do
      t = not_equals_term(type_i())

      hol_term(bvars: bvars) = t
      assert length(bvars) == 2
      [bv1, bv2] = bvars
      declaration(type: t1) = bv1
      declaration(type: t2) = bv2
      assert t1 == type_i()
      assert t2 == type_i()
    end

    test "not_equals_term structure matches negated equals_term" do
      eq_term = equals_term(type_o())
      neq_term = not_equals_term(type_o())

      # Both should have same bound variables
      hol_term(bvars: eq_bvars) = eq_term
      hol_term(bvars: neq_bvars) = neq_term
      assert eq_bvars == neq_bvars
    end
  end

  describe "term structure properties" do
    test "binary terms have correct arg structure" do
      t = and_term()

      hol_term(args: args, bvars: bvars) = t
      assert length(args) == 2
      assert length(bvars) == 2
    end

    test "unary term has correct arg structure" do
      t = neg_term()

      hol_term(args: args, bvars: bvars) = t
      assert length(args) == 1
      assert length(bvars) == 1
    end

    test "nullary terms have no args" do
      t = true_term()

      hol_term(args: args, bvars: bvars) = t
      assert args == []
      assert bvars == []
    end

    test "terms have correct max_num" do
      t_true = true_term()
      t_neg = neg_term()
      t_and = and_term()

      hol_term(max_num: max0) = t_true
      hol_term(max_num: max1) = t_neg
      hol_term(max_num: max2) = t_and

      assert max0 == 0
      assert max1 == 1
      assert max2 == 2
    end

    test "terms have empty fvars" do
      t = and_term()

      hol_term(fvars: fvars) = t
      assert fvars == []
    end
  end
end
