defmodule BeHOLd.Util.LexerTest do
  @moduledoc """
  Tests for the BeHOLd.Util.Lexer module.
  """
  use ExUnit.Case, async: true

  alias BeHOLd.Util.Lexer

  describe "tokenize/1 - basic symbols" do
    test "tokenizes simple variable" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X")
      assert tokens == [var: "X"]
    end

    test "tokenizes multiple variables" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("A B C")
      assert tokens == [var: "A", var: "B", var: "C"]
    end

    test "tokenizes lowercase atoms" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("foo bar baz")
      assert tokens == [{:atom, "foo"}, {:atom, "bar"}, {:atom, "baz"}]
    end

    test "tokenizes variable with underscores and numbers" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X_1 Y_test Z123")
      assert tokens == [var: "X_1", var: "Y_test", var: "Z123"]
    end
  end

  describe "tokenize/1 - system symbols" do
    test "tokenizes $true" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("$true")
      assert tokens == [system: "$true"]
    end

    test "tokenizes $false" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("$false")
      assert tokens == [system: "$false"]
    end

    test "tokenizes $o and $i types" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("$o $i")
      assert tokens == [system: "$o", system: "$i"]
    end

    test "tokenizes $tType" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("$tType")
      assert tokens == [system: "$tType"]
    end
  end

  describe "tokenize/1 - connectives" do
    test "tokenizes logical connectives" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("& | ~ => <=> <~>")

      assert tokens == [
               {:and, "&"},
               {:or, "|"},
               {:not, "~"},
               {:implies, "=>"},
               {:equiv, "<=>"},
               {:xor, "<~>"}
             ]
    end

    test "tokenizes NAND and NOR" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("~& ~|")
      assert tokens == [{:nand, "~&"}, {:nor, "~|"}]
    end

    test "tokenizes reverse implication" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("<=")
      assert tokens == [{:implied_by, "<="}]
    end
  end

  describe "tokenize/1 - quantifiers and binders" do
    test "tokenizes universal quantifier" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("!")
      assert tokens == [{:forall, "!"}]
    end

    test "tokenizes existential quantifier" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("?")
      assert tokens == [{:exists, "?"}]
    end

    test "tokenizes lambda abstraction" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("^")
      assert tokens == [{:lambda, "^"}]
    end

    test "tokenizes pi and sigma" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("!! ??")
      assert tokens == [{:pi, "!!"}, {:sigma, "??"}]
    end
  end

  describe "tokenize/1 - equality" do
    test "tokenizes equality" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("=")
      assert tokens == [{:eq, "="}]
    end

    test "tokenizes inequality" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("!=")
      assert tokens == [{:neq, "!="}]
    end
  end

  describe "tokenize/1 - application" do
    test "tokenizes application operator" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("@")
      assert tokens == [{:app, "@"}]
    end

    test "tokenizes function application" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("f @ X")
      assert tokens == [{:atom, "f"}, {:app, "@"}, {:var, "X"}]
    end
  end

  describe "tokenize/1 - punctuation" do
    test "tokenizes parentheses" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("( )")
      assert tokens == [{:lparen, "("}, {:rparen, ")"}]
    end

    test "tokenizes brackets" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("[ ]")
      assert tokens == [{:lbracket, "["}, {:rbracket, "]"}]
    end

    test "tokenizes colon, comma, and dot" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize(": , .")
      assert tokens == [{:colon, ":"}, {:comma, ","}, {:dot, "."}]
    end

    test "tokenizes type arrow" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize(">")
      assert tokens == [{:arrow, ">"}]
    end
  end

  describe "tokenize/1 - keywords" do
    test "tokenizes include keyword" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("include")
      assert tokens == [{:keyword, :include}]
    end

    test "tokenizes thf keyword" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("thf")
      assert tokens == [{:keyword, :thf}]
    end
  end

  describe "tokenize/1 - roles" do
    test "tokenizes role type" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("type")
      assert tokens == [{:role, :type}]
    end

    test "tokenizes role axiom" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("axiom")
      assert tokens == [{:role, :axiom}]
    end

    test "tokenizes role definition" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("definition")
      assert tokens == [{:role, :definition}]
    end

    test "tokenizes role conjecture" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("conjecture")
      assert tokens == [{:role, :conjecture}]
    end

    test "tokenizes role lemma" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("lemma")
      assert tokens == [{:role, :lemma}]
    end

    test "tokenizes role hypothesis" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("hypothesis")
      assert tokens == [{:role, :hypothesis}]
    end
  end

  describe "tokenize/1 - distinct objects" do
    test "tokenizes single quoted string" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("'hello'")
      assert tokens == [{:distinct, "hello"}]
    end

    test "tokenizes path-like string" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("'Axioms/file.ax'")
      assert tokens == [{:distinct, "Axioms/file.ax"}]
    end
  end

  describe "tokenize/1 - comments" do
    test "ignores line comments" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X % this is a comment\nY")
      assert tokens == [var: "X", var: "Y"]
    end

    test "ignores comment at end of input" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X & Y % comment")
      assert tokens == [{:var, "X"}, {:and, "&"}, {:var, "Y"}]
    end
  end

  describe "tokenize/1 - whitespace handling" do
    test "handles multiple spaces" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X     Y")
      assert tokens == [var: "X", var: "Y"]
    end

    test "handles tabs" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X\tY")
      assert tokens == [var: "X", var: "Y"]
    end

    test "handles newlines" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X\nY")
      assert tokens == [var: "X", var: "Y"]
    end

    test "handles carriage returns" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("X\r\nY")
      assert tokens == [var: "X", var: "Y"]
    end
  end

  describe "tokenize/1 - complex formulas" do
    test "tokenizes simple conjunction" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("A & B")
      assert tokens == [var: "A", and: "&", var: "B"]
    end

    test "tokenizes universal quantification" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("![X : $o]: X")

      assert tokens == [
               {:forall, "!"},
               {:lbracket, "["},
               {:var, "X"},
               {:colon, ":"},
               {:system, "$o"},
               {:rbracket, "]"},
               {:colon, ":"},
               {:var, "X"}
             ]
    end

    test "tokenizes existential quantification" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("?[X : $i]: X = X")

      assert tokens == [
               {:exists, "?"},
               {:lbracket, "["},
               {:var, "X"},
               {:colon, ":"},
               {:system, "$i"},
               {:rbracket, "]"},
               {:colon, ":"},
               {:var, "X"},
               {:eq, "="},
               {:var, "X"}
             ]
    end

    test "tokenizes lambda expression" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("^[X : $o]: X")

      assert tokens == [
               {:lambda, "^"},
               {:lbracket, "["},
               {:var, "X"},
               {:colon, ":"},
               {:system, "$o"},
               {:rbracket, "]"},
               {:colon, ":"},
               {:var, "X"}
             ]
    end

    test "tokenizes function type" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("$i > $o")

      assert tokens == [
               {:system, "$i"},
               {:arrow, ">"},
               {:system, "$o"}
             ]
    end

    test "tokenizes TPTP thf entry" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("thf(ax1, axiom, X = X).")

      assert tokens == [
               {:keyword, :thf},
               {:lparen, "("},
               {:atom, "ax1"},
               {:comma, ","},
               {:role, :axiom},
               {:comma, ","},
               {:var, "X"},
               {:eq, "="},
               {:var, "X"},
               {:rparen, ")"},
               {:dot, "."}
             ]
    end

    test "tokenizes TPTP type declaration" do
      {:ok, tokens, "", _, _, _} = Lexer.tokenize("thf(myconst_type, type, myconst : $i > $o).")

      assert tokens == [
               {:keyword, :thf},
               {:lparen, "("},
               {:atom, "myconst_type"},
               {:comma, ","},
               {:role, :type},
               {:comma, ","},
               {:atom, "myconst"},
               {:colon, ":"},
               {:system, "$i"},
               {:arrow, ">"},
               {:system, "$o"},
               {:rparen, ")"},
               {:dot, "."}
             ]
    end
  end
end
