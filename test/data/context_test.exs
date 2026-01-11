defmodule BeHOLd.Data.ContextTest do
  @moduledoc """
  Tests for the BeHOLd.Data.Context module.
  """
  use ExUnit.Case, async: true

  alias BeHOLd.Data.Context
  import HOL.Data

  describe "new/0" do
    test "creates an empty context" do
      ctx = Context.new()

      assert ctx.vars == %{}
      assert ctx.consts == %{}
      assert MapSet.size(ctx.constraints) == 0
    end
  end

  describe "put_var/3" do
    test "adds a variable to the context" do
      ctx = Context.new() |> Context.put_var("X", mk_type(:o))

      assert Map.has_key?(ctx.vars, "X")
      assert ctx.vars["X"] == mk_type(:o)
    end

    test "adds multiple variables" do
      ctx =
        Context.new()
        |> Context.put_var("X", mk_type(:o))
        |> Context.put_var("Y", mk_type(:i))

      assert ctx.vars["X"] == mk_type(:o)
      assert ctx.vars["Y"] == mk_type(:i)
    end

    test "overwrites existing variable" do
      ctx =
        Context.new()
        |> Context.put_var("X", mk_type(:o))
        |> Context.put_var("X", mk_type(:i))

      assert ctx.vars["X"] == mk_type(:i)
    end

    test "preserves constants when adding variables" do
      ctx =
        Context.new()
        |> Context.put_const("f", mk_type(:o, [mk_type(:i)]))
        |> Context.put_var("X", mk_type(:o))

      assert ctx.consts["f"] == mk_type(:o, [mk_type(:i)])
      assert ctx.vars["X"] == mk_type(:o)
    end
  end

  describe "put_const/3" do
    test "adds a constant to the context" do
      ctx = Context.new() |> Context.put_const("myconst", mk_type(:i))

      assert Map.has_key?(ctx.consts, "myconst")
      assert ctx.consts["myconst"] == mk_type(:i)
    end

    test "adds function type constant" do
      func_type = mk_type(:o, [mk_type(:i)])
      ctx = Context.new() |> Context.put_const("pred", func_type)

      assert ctx.consts["pred"] == func_type
    end

    test "adds multiple constants" do
      ctx =
        Context.new()
        |> Context.put_const("c1", mk_type(:i))
        |> Context.put_const("c2", mk_type(:o))

      assert ctx.consts["c1"] == mk_type(:i)
      assert ctx.consts["c2"] == mk_type(:o)
    end

    test "overwrites existing constant" do
      ctx =
        Context.new()
        |> Context.put_const("c", mk_type(:o))
        |> Context.put_const("c", mk_type(:i))

      assert ctx.consts["c"] == mk_type(:i)
    end

    test "preserves variables when adding constants" do
      ctx =
        Context.new()
        |> Context.put_var("X", mk_type(:o))
        |> Context.put_const("f", mk_type(:o, [mk_type(:i)]))

      assert ctx.vars["X"] == mk_type(:o)
      assert ctx.consts["f"] == mk_type(:o, [mk_type(:i)])
    end
  end

  describe "add_constraint/3" do
    test "adds a type constraint" do
      t1 = mk_type(:o)
      t2 = mk_type(:o)
      ctx = Context.new() |> Context.add_constraint(t1, t2)

      assert MapSet.member?(ctx.constraints, {t1, t2})
    end

    test "adds multiple constraints" do
      t1 = mk_type(:o)
      t2 = mk_type(:i)
      t3 = mk_type(:o, [mk_type(:i)])

      ctx =
        Context.new()
        |> Context.add_constraint(t1, t2)
        |> Context.add_constraint(t2, t3)

      assert MapSet.size(ctx.constraints) == 2
      assert MapSet.member?(ctx.constraints, {t1, t2})
      assert MapSet.member?(ctx.constraints, {t2, t3})
    end

    test "does not duplicate identical constraints" do
      t1 = mk_type(:o)
      t2 = mk_type(:i)

      ctx =
        Context.new()
        |> Context.add_constraint(t1, t2)
        |> Context.add_constraint(t1, t2)

      assert MapSet.size(ctx.constraints) == 1
    end

    test "preserves vars and consts when adding constraints" do
      ctx =
        Context.new()
        |> Context.put_var("X", mk_type(:o))
        |> Context.put_const("f", mk_type(:i))
        |> Context.add_constraint(mk_type(:o), mk_type(:o))

      assert ctx.vars["X"] == mk_type(:o)
      assert ctx.consts["f"] == mk_type(:i)
    end
  end

  describe "get_type/2" do
    test "returns type of known variable" do
      ctx = Context.new() |> Context.put_var("X", mk_type(:o))

      assert Context.get_type(ctx, "X") == mk_type(:o)
    end

    test "returns type of known constant" do
      ctx = Context.new() |> Context.put_const("c", mk_type(:i))

      assert Context.get_type(ctx, "c") == mk_type(:i)
    end

    test "returns nil for unknown name" do
      ctx = Context.new()

      assert Context.get_type(ctx, "unknown") == nil
    end

    test "variable takes precedence if same name exists as both" do
      # This tests the OR logic: vars are checked first
      ctx =
        Context.new()
        |> Context.put_const("x", mk_type(:i))
        |> Context.put_var("x", mk_type(:o))

      # Variable should be returned (vars are checked first due to || operator)
      assert Context.get_type(ctx, "x") == mk_type(:o)
    end

    test "returns constant type if only constant exists" do
      ctx = Context.new() |> Context.put_const("f", mk_type(:o, [mk_type(:i)]))

      assert Context.get_type(ctx, "f") == mk_type(:o, [mk_type(:i)])
    end
  end

  describe "integration scenarios" do
    test "build context for simple formula" do
      # Simulate building context for: X & Y
      ctx =
        Context.new()
        |> Context.put_var("X", mk_type(:o))
        |> Context.put_var("Y", mk_type(:o))

      assert Context.get_type(ctx, "X") == mk_type(:o)
      assert Context.get_type(ctx, "Y") == mk_type(:o)
    end

    test "build context with function constant" do
      # Simulate: f @ x = y where f : i -> o, x : i, y : o
      ctx =
        Context.new()
        |> Context.put_const("f", mk_type(:o, [mk_type(:i)]))
        |> Context.put_var("x", mk_type(:i))
        |> Context.put_var("y", mk_type(:o))

      assert Context.get_type(ctx, "f") == mk_type(:o, [mk_type(:i)])
      assert Context.get_type(ctx, "x") == mk_type(:i)
      assert Context.get_type(ctx, "y") == mk_type(:o)
    end

    test "build context with type constraints" do
      # Simulate type inference with constraints
      t_unknown = mk_type(:__unknown_1)

      ctx =
        Context.new()
        |> Context.put_var("X", t_unknown)
        |> Context.add_constraint(t_unknown, mk_type(:o))

      assert Context.get_type(ctx, "X") == t_unknown
      assert MapSet.member?(ctx.constraints, {t_unknown, mk_type(:o)})
    end

    test "complex context with multiple symbols" do
      # Build context for: ![X : i]: ?[Y : o]: f @ X = g @ Y
      ctx =
        Context.new()
        |> Context.put_const("f", mk_type(:o, [mk_type(:i)]))
        |> Context.put_const("g", mk_type(:o, [mk_type(:o)]))
        |> Context.put_var("X", mk_type(:i))
        |> Context.put_var("Y", mk_type(:o))
        |> Context.add_constraint(mk_type(:o), mk_type(:o))

      assert Map.keys(ctx.consts) == ["f", "g"]
      assert Map.keys(ctx.vars) == ["X", "Y"]
    end
  end
end
