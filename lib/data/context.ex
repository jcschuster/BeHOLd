defmodule BeHOLd.Data.Context do
  @moduledoc """
  A data structure to declare and track variable types and declared constants.
  Also contains type constraints needed for parsing.

  ## Examples

      iex> Context.new()
      %BeHOLd.Data.Context{vars: %{}, consts: %{}, constraints: MapSet.new([])}

      iex> Context.new() |> Context.put_var("X", HOL.Data.mk_type(:o))
      %BeHOLd.Data.Context{
        vars: %{"X" => {:type, :o, []}},
        consts: %{},
        constraints: MapSet.new([])
      }
  """

  # The use of MapSet raises opaqueness warnings which can be ignored.
  @dialyzer {:no_opaque, new: 0}

  defstruct vars: %{}, consts: %{}, constraints: MapSet.new()

  @typedoc """
  The type of the context.

  A context contains the type of variables (`:vars`) as a `Map` from its name
  to its type. Likewise for the constants (`:consts`). The type constraints
  are represented as a `MapSet` of `HOL.Data.type()` pairs.
  """
  @type t() :: %__MODULE__{
          vars: %{String.t() => HOL.Data.type()},
          consts: %{String.t() => HOL.Data.type()},
          constraints: MapSet.t({HOL.Data.type(), HOL.Data.type()})
        }

  @doc """
  Creates an empty context.
  """
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc """
  Associates the variable with the given name with the given type in the
  context. Overwrites the old value if present.
  """
  @spec put_var(t(), String.t(), HOL.Data.type()) :: t()
  def put_var(ctx, name, type) do
    %{ctx | vars: Map.put(ctx.vars, name, type)}
  end

  @doc """
  Associates the constant with the given name with the given type in the
  context. Overwrites the old value if present.
  """
  def put_const(ctx, name, type) do
    %{ctx | consts: Map.put(ctx.consts, name, type)}
  end

  @doc """
  Adds a type constraint to the context.
  """
  @spec add_constraint(t(), HOL.Data.type(), HOL.Data.type()) :: t()
  def add_constraint(ctx, t1, t2) do
    %{ctx | constraints: MapSet.put(ctx.constraints, {t1, t2})}
  end

  @doc """
  Returns the type of the given name of a constant or variable. Returns `nil`
  if the name is not present in the context.
  """
  @spec get_type(t(), String.t()) :: HOL.Data.type() | nil
  def get_type(ctx, name) do
    Map.get(ctx.vars, name) || Map.get(ctx.consts, name)
  end
end
