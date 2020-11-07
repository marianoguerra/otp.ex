defmodule :m_erl_bits do
  use Bitwise
  require Record

  Record.defrecord(:r_bittype, :bittype,
    type: :undefined,
    unit: :undefined,
    sign: :undefined,
    endian: :undefined
  )

  def system_bitdefault() do
    :no_system_bitdefault
  end

  def system_bittypes() do
    :no_system_types
  end

  def as_list(bt) do
    [
      r_bittype(bt, :type),
      {:unit, r_bittype(bt, :unit)},
      r_bittype(bt, :sign),
      r_bittype(bt, :endian)
    ]
  end

  def set_bit_type(size, :default) do
    set_bit_type(size, [])
  end

  def set_bit_type(size, typeList) do
    try do
      r_bittype(type: type, unit: unit, sign: sign, endian: endian) = set_bit(typeList)
      apply_defaults(type, size, unit, sign, endian)
    catch
      error ->
        error
    end
  end

  defp set_bit([]) do
    r_bittype()
  end

  defp set_bit([h | t]) do
    set_bit_1(t, type_to_record(h))
  end

  defp set_bit_1([t0 | ts], bt0) do
    type = type_to_record(t0)
    bt = merge_bittype(type, bt0)
    set_bit_1(ts, bt)
  end

  defp set_bit_1([], bt) do
    bt
  end

  defp type_to_record(:integer) do
    r_bittype(type: :integer)
  end

  defp type_to_record(:utf8) do
    r_bittype(type: :utf8)
  end

  defp type_to_record(:utf16) do
    r_bittype(type: :utf16)
  end

  defp type_to_record(:utf32) do
    r_bittype(type: :utf32)
  end

  defp type_to_record(:float) do
    r_bittype(type: :float)
  end

  defp type_to_record(:binary) do
    r_bittype(type: :binary)
  end

  defp type_to_record(:bytes) do
    r_bittype(type: :binary, unit: 8)
  end

  defp type_to_record(:bitstring) do
    r_bittype(type: :binary, unit: 1)
  end

  defp type_to_record(:bits) do
    r_bittype(type: :binary, unit: 1)
  end

  defp type_to_record({:unit, :undefined}) do
    r_bittype(unit: :undefined)
  end

  defp type_to_record({:unit, sz})
       when is_integer(sz) and sz > 0 and
              sz <= 256 do
    r_bittype(unit: sz)
  end

  defp type_to_record(:big) do
    r_bittype(endian: :big)
  end

  defp type_to_record(:little) do
    r_bittype(endian: :little)
  end

  defp type_to_record(:native) do
    r_bittype(endian: :native)
  end

  defp type_to_record(:signed) do
    r_bittype(sign: :signed)
  end

  defp type_to_record(:unsigned) do
    r_bittype(sign: :unsigned)
  end

  defp type_to_record(name) do
    throw({:error, {:undefined_bittype, name}})
  end

  defp merge_bittype(b1, b2) do
    endian = merge_field(r_bittype(b1, :endian), r_bittype(b2, :endian), :endianness)
    sign = merge_field(r_bittype(b1, :sign), r_bittype(b2, :sign), :sign)
    type = merge_field(r_bittype(b1, :type), r_bittype(b2, :type), :type)
    unit = merge_field(r_bittype(b1, :unit), r_bittype(b2, :unit), :unit)
    r_bittype(type: type, unit: unit, endian: endian, sign: sign)
  end

  defp merge_field(:undefined, b, _) do
    b
  end

  defp merge_field(a, :undefined, _) do
    a
  end

  defp merge_field(a, a, _) do
    a
  end

  defp merge_field(x, y, what) do
    throw({:error, {:bittype_mismatch, x, y, :erlang.atom_to_list(what)}})
  end

  defp apply_defaults(:undefined, size, unit, sign, endian) do
    apply_defaults(:integer, size, unit, sign, endian)
  end

  defp apply_defaults(:binary, :default, unit, sign, endian) do
    apply_defaults(:binary, :all, unit, sign, endian)
  end

  defp apply_defaults(:integer, :default, unit, sign, endian) do
    check_unit(unit)
    apply_defaults(:integer, 8, 1, sign, endian)
  end

  defp apply_defaults(:utf8 = type, :default, unit, sign, endian) do
    apply_defaults(type, :undefined, unit, sign, endian)
  end

  defp apply_defaults(:utf16 = type, :default, unit, sign, endian) do
    apply_defaults(type, :undefined, unit, sign, endian)
  end

  defp apply_defaults(:utf32 = type, :default, unit, sign, endian) do
    apply_defaults(type, :undefined, unit, sign, endian)
  end

  defp apply_defaults(:float, :default, unit, sign, endian) do
    check_unit(unit)
    apply_defaults(:float, 64, 1, sign, endian)
  end

  defp apply_defaults(:binary, size, :undefined, sign, endian) do
    apply_defaults(:binary, size, 8, sign, endian)
  end

  defp apply_defaults(:integer, size, :undefined, sign, endian) do
    apply_defaults(:integer, size, 1, sign, endian)
  end

  defp apply_defaults(:float, size, :undefined, sign, endian) do
    apply_defaults(:float, size, 1, sign, endian)
  end

  defp apply_defaults(type, size, unit, :undefined, endian) do
    apply_defaults(type, size, unit, :unsigned, endian)
  end

  defp apply_defaults(type, size, unit, sign, :undefined) do
    apply_defaults(type, size, unit, sign, :big)
  end

  defp apply_defaults(type, size, unit, sign, endian) do
    check_size_unit(type, size, unit)
    {:ok, size, r_bittype(type: type, unit: unit, sign: sign, endian: endian)}
  end

  defp check_size_unit(:utf8, size, unit) do
    check_size_unit_1(size, unit)
  end

  defp check_size_unit(:utf16, size, unit) do
    check_size_unit_1(size, unit)
  end

  defp check_size_unit(:utf32, size, unit) do
    check_size_unit_1(size, unit)
  end

  defp check_size_unit(_, _, _) do
    :ok
  end

  defp check_size_unit_1(size, unit) do
    case size do
      :default ->
        :ok

      :undefined ->
        :ok

      {:atom, _, :undefined} ->
        :ok

      {:value, _, :undefined} ->
        :ok

      _ ->
        throw({:error, :utf_bittype_size_or_unit})
    end

    case unit do
      :undefined ->
        :ok

      _ ->
        throw({:error, :utf_bittype_size_or_unit})
    end
  end

  defp check_unit(:undefined) do
    :ok
  end

  defp check_unit(_) do
    throw({:error, :bittype_unit})
  end
end
