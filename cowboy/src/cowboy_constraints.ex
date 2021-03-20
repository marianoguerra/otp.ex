defmodule :cowboy_constraints do
  use Bitwise
  def validate(value, constraints) when is_list(constraints) do
    apply_list(:forward, value, constraints)
  end

  def validate(value, constraint) do
    apply_list(:forward, value, [constraint])
  end

  def reverse(value, constraints) when is_list(constraints) do
    apply_list(:reverse, value, constraints)
  end

  def reverse(value, constraint) do
    apply_list(:reverse, value, [constraint])
  end

  def format_error({constraint, reason, value}) do
    apply_constraint(:format_error, {reason, value},
                       constraint)
  end

  defp apply_list(_, value, []) do
    {:ok, value}
  end

  defp apply_list(type, value0, [constraint | tail]) do
    case (apply_constraint(type, value0, constraint)) do
      {:ok, value} ->
        apply_list(type, value, tail)
      {:error, reason} ->
        {:error, {constraint, reason, value0}}
    end
  end

  defp apply_constraint(type, value, :int) do
    int(type, value)
  end

  defp apply_constraint(type, value, :nonempty) do
    nonempty(type, value)
  end

  defp apply_constraint(type, value, f) when is_function(f) do
    f.(type, value)
  end

  defp int(:forward, value) do
    try do
      {:ok, :erlang.binary_to_integer(value)}
    catch
      _, _ ->
        {:error, :not_an_integer}
    end
  end

  defp int(:reverse, value) do
    try do
      {:ok, :erlang.integer_to_binary(value)}
    catch
      _, _ ->
        {:error, :not_an_integer}
    end
  end

  defp int(:format_error, {:not_an_integer, value}) do
    :io_lib.format('The value ~p is not an integer.', [value])
  end

  defp nonempty(type, <<>>) when type !== :format_error do
    {:error, :empty}
  end

  defp nonempty(type, value) when (type !== :format_error and
                               is_binary(value)) do
    {:ok, value}
  end

  defp nonempty(:format_error, {:empty, value}) do
    :io_lib.format('The value ~p is empty.', [value])
  end

end