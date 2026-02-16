defmodule BeHOLd.ClassicalHOL.Equality do
  @moduledoc """
  Provides various terms constructors as macros for different notions of
  equality. This includes variants of Leibniz equality, Andrews equality and
  extensional equality.
  """

  import HOL.Data
  import HOL.Terms
  use BeHOLd.ClassicalHOL.Definitions

  @doc """
  Constructor for Leibniz equality on the given type, which defines equality by
  stating that both arguments share the same properties. Generates an
  abstraction which can be applied to two arguments.

  # Examples

      iex> leibniz_equality(type_i(), equivalent_term()) == parse("^[X:$i, Y:$i]: ![P:$i>$o]: P @ X <=> P @ Y")
      true

      iex> leibniz_equality(type_i(), implied_by_term()) == parse("^[X:$i, Y:$i]: ![P:$i>$o]: P @ X <= P @ Y")
      true
  """
  @spec leibniz_equality(HOL.Data.type(), HOL.Data.hol_term()) :: HOL.Data.hol_term()
  def leibniz_equality(type, connective \\ equivalent_term())

  def leibniz_equality(type, hol_term(type: type_ooo()) = connective) do
    x = mk_free_var("X", type)
    y = mk_free_var("Y", type)

    p_type = type(goal: :o, args: [type])
    p = mk_free_var("P", p_type)
    p_term = mk_term(p)

    p_x = mk_appl_term(p_term, mk_term(x))
    p_y = mk_appl_term(p_term, mk_term(y))

    inner_body = connective |> mk_appl_term(p_x) |> mk_appl_term(p_y)
    inner_abs = mk_abstr_term(inner_body, p)
    outer_body = pi_term(p_type) |> mk_appl_term(inner_abs)

    outer_body |> mk_abstr_term(y) |> mk_abstr_term(x)
  end

  def leibniz_equality(_, hol_term(type: type)) do
    raise "ArgumentError: connective for Leibniz equality must be of type o⇾o⇾o, got #{inspect(type)} instead."
  end

  @doc """
  Constructor for Andrews equality on the given type, which defines equality by
  stating that both arguments share all reflexive relations. Generates an
  abstraction which can be applied to two arguments.

  # Example

      iex> andrews_equality(type_i()) == parse("^[X:$i, Y:$i]: ![Q:$i>$i>$o] ((![Z:$i]: Q @ Z @ Z) => Q @ X @ Y)")
      true
  """
  @spec andrews_equality(HOL.Data.type()) :: HOL.Data.hol_term()
  def andrews_equality(type) do
    x = mk_free_var("X", type)
    y = mk_free_var("Y", type)
    z = mk_free_var("Z", type)
    z_term = mk_term(z)

    q_type = type(goal: :o, args: [type, type])
    q = mk_free_var("Q", q_type)
    q_term = mk_term(q)

    q_x_y = q_term |> mk_appl_term(mk_term(x)) |> mk_appl_term(mk_term(y))
    q_z_z = q_term |> mk_appl_term(z_term) |> mk_appl_term(z_term)

    lhs = pi_term(type) |> mk_appl_term(mk_abstr_term(q_z_z, z))
    inner_body = implies_term() |> mk_appl_term(lhs) |> mk_appl_term(q_x_y)
    inner_abstr = mk_abstr_term(inner_body, q)
    outer_body = pi_term(q_type) |> mk_appl_term(inner_abstr)

    outer_body |> mk_abstr_term(y) |> mk_abstr_term(x)
  end

  @doc """
  Constructor for extensional equality on the given function type, which
  defines equality by equality of the extensions. Generates an abstraction
  which can be applied to two arguments.

  # Example

      iex> extensional_equality(type_ii()) == parse("^[X:$i>i, Y:$i>i]: ![Z:$i]: X @ Z = Y @ Z")
      true
  """
  @spec extensional_equality(HOL.Data.type()) :: HOL.Data.hol_term()
  def extensional_equality(type)

  def extensional_equality(type(goal: g, args: [at | ats]) = type) do
    x = mk_free_var("X", type)
    y = mk_free_var("Y", type)
    z = mk_free_var("Z", at)
    z_term = mk_term(z)

    x_z = mk_term(x) |> mk_appl_term(z_term)
    y_z = mk_term(y) |> mk_appl_term(z_term)

    inner_body = equals_term(type(goal: g, args: ats)) |> mk_appl_term(x_z) |> mk_appl_term(y_z)
    inner_abstr = mk_abstr_term(inner_body, z)
    outer_body = pi_term(at) |> mk_appl_term(inner_abstr)

    outer_body |> mk_abstr_term(y) |> mk_abstr_term(x)
  end

  def extensional_equality(type) do
    raise "ArgumentError: type for extensional equality must be a function type. Got #{inspect(type)} instead."
  end
end
