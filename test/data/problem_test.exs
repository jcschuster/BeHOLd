defmodule BeHOLd.Data.ProblemTest do
  @moduledoc """
  Tests for the BeHOLd.Data.Problem module.
  """
  use ExUnit.Case, async: true

  alias BeHOLd.Data.Problem
  import HOL.Data

  describe "struct creation" do
    test "creates empty problem with defaults" do
      problem = %Problem{}

      assert problem.path == ""
      assert problem.includes == []
      assert problem.types == %{}
      assert problem.definitions == %{}
      assert problem.axioms == []
      assert problem.conjecture == nil
    end

    test "creates problem with custom path" do
      problem = %Problem{path: "/path/to/problem.p"}

      assert problem.path == "/path/to/problem.p"
    end

    test "creates problem with includes" do
      problem = %Problem{
        path: "main.p",
        includes: ["axiom1.ax", "axiom2.ax"]
      }

      assert problem.includes == ["axiom1.ax", "axiom2.ax"]
    end
  end

  describe "types field" do
    test "stores base type declaration" do
      problem = %Problem{
        types: %{"mytype" => :base_type}
      }

      assert problem.types["mytype"] == :base_type
    end

    test "stores constant type declaration" do
      func_type = mk_type(:o, [mk_type(:i)])

      problem = %Problem{
        types: %{"pred" => func_type}
      }

      assert problem.types["pred"] == func_type
    end

    test "stores multiple type declarations" do
      problem = %Problem{
        types: %{
          "mytype" => :base_type,
          "const1" => mk_type(:i),
          "pred" => mk_type(:o, [mk_type(:i)])
        }
      }

      assert map_size(problem.types) == 3
      assert problem.types["mytype"] == :base_type
      assert problem.types["const1"] == mk_type(:i)
      assert problem.types["pred"] == mk_type(:o, [mk_type(:i)])
    end
  end

  describe "definitions field" do
    test "stores single definition" do
      # Simulating a definition like: myconst = $true
      # In practice this would be an hol_term, but we use a placeholder here
      mock_term = {:term, :mock}

      problem = %Problem{
        definitions: %{"myconst" => mock_term}
      }

      assert problem.definitions["myconst"] == mock_term
    end

    test "stores multiple definitions" do
      mock_term1 = {:term, :def1}
      mock_term2 = {:term, :def2}

      problem = %Problem{
        definitions: %{
          "const1" => mock_term1,
          "const2" => mock_term2
        }
      }

      assert map_size(problem.definitions) == 2
    end
  end

  describe "axioms field" do
    test "stores single axiom" do
      mock_term = {:term, :axiom}

      problem = %Problem{
        axioms: [{"ax1", mock_term}]
      }

      assert length(problem.axioms) == 1
      assert {"ax1", mock_term} in problem.axioms
    end

    test "stores multiple axioms in order" do
      axioms = [
        {"ax1", {:term, :ax1}},
        {"ax2", {:term, :ax2}},
        {"ax3", {:term, :ax3}}
      ]

      problem = %Problem{axioms: axioms}

      assert problem.axioms == axioms
      assert length(problem.axioms) == 3
    end

    test "axioms preserve insertion order" do
      problem = %Problem{
        axioms: [{"first", :t1}, {"second", :t2}, {"third", :t3}]
      }

      [first, second, third] = problem.axioms
      assert elem(first, 0) == "first"
      assert elem(second, 0) == "second"
      assert elem(third, 0) == "third"
    end
  end

  describe "conjecture field" do
    test "stores conjecture" do
      mock_term = {:term, :conjecture}

      problem = %Problem{
        conjecture: {"goal", mock_term}
      }

      assert problem.conjecture == {"goal", mock_term}
    end

    test "conjecture is nil by default" do
      problem = %Problem{}

      assert problem.conjecture == nil
    end

    test "can have problem without conjecture" do
      problem = %Problem{
        axioms: [{"ax1", {:term, :ax}}],
        conjecture: nil
      }

      assert problem.conjecture == nil
      assert length(problem.axioms) == 1
    end
  end

  describe "complete problem structure" do
    test "creates a complete problem" do
      problem = %Problem{
        path: "Problems/SET/SET001+1.p",
        includes: ["Axioms/SET001+0.ax"],
        types: %{
          "set" => :base_type,
          "member" => mk_type(:o, [mk_type(:i), mk_type(:i)])
        },
        definitions: %{},
        axioms: [
          {"extensionality", {:term, :ext}},
          {"empty_set", {:term, :empty}}
        ],
        conjecture: {"goal", {:term, :goal}}
      }

      assert problem.path == "Problems/SET/SET001+1.p"
      assert length(problem.includes) == 1
      assert map_size(problem.types) == 2
      assert map_size(problem.definitions) == 0
      assert length(problem.axioms) == 2
      assert problem.conjecture == {"goal", {:term, :goal}}
    end

    test "problem is a struct" do
      problem = %Problem{}

      assert is_struct(problem, Problem)
      assert is_map(problem)
    end

    test "can update problem fields" do
      problem = %Problem{path: "old.p"}
      updated = %{problem | path: "new.p"}

      assert updated.path == "new.p"
    end

    test "can add axioms to existing problem" do
      problem = %Problem{axioms: [{"ax1", :t1}]}
      updated = %{problem | axioms: problem.axioms ++ [{"ax2", :t2}]}

      assert length(updated.axioms) == 2
    end

    test "can merge types from included problem" do
      main = %Problem{types: %{"a" => :base_type}}
      included = %{types: %{"b" => :base_type}}

      merged = %{main | types: Map.merge(main.types, included.types)}

      assert map_size(merged.types) == 2
      assert merged.types["a"] == :base_type
      assert merged.types["b"] == :base_type
    end
  end
end
