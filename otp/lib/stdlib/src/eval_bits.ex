defmodule :m_eval_bits do
  use Bitwise

  def expr_grp(fields, bindings, evalFun, [], _) do
    expr_grp(fields, bindings, evalFun, <<>>)
  end

  def expr_grp(fields, bindings, evalFun, listOfBits, _) do
    bin = convert_list(listOfBits)
    expr_grp(fields, bindings, evalFun, bin)
  end

  defp convert_list(list) do
    for x <- list, into: <<>> do
      <<x::size(1)>>
    end
  end

  def expr_grp(fields, bindings, evalFun) do
    expr_grp(fields, bindings, evalFun, <<>>)
  end

  defp expr_grp([field | fS], bs0, lf, acc) do
    {bin, bs} = eval_field(field, bs0, lf)
    expr_grp(fS, bs, lf, <<acc::binary-unit(1), bin::binary-unit(1)>>)
  end

  defp expr_grp([], bs0, _Lf, acc) do
    {:value, acc, bs0}
  end

  defp eval_field(
         {:bin_element, _, {:string, _, s}, {:integer, _, 8},
          [:integer, {:unit, 1}, :unsigned, :big]},
         bs0,
         _Fun
       ) do
    latin1 =
      for c <- s do
        c &&& 255
      end

    {:erlang.list_to_binary(latin1), bs0}
  end

  defp eval_field({:bin_element, _, {:string, _, s}, :default, :default}, bs0, _Fun) do
    latin1 =
      for c <- s do
        c &&& 255
      end

    {:erlang.list_to_binary(latin1), bs0}
  end

  defp eval_field({:bin_element, line, {:string, _, s}, size0, options0}, bs0, fun) do
    {size1, [type, {:unit, unit}, sign, endian]} = make_bit_type(line, size0, options0)
    {:value, size, bs1} = fun.(size1, bs0)

    res =
      for c <- s, into: <<>> do
        <<eval_exp_field1(c, size, unit, type, endian, sign)::bitstring>>
      end

    case s do
      '' ->
        _ = eval_exp_field1(0, size, unit, type, endian, sign)
        :ok

      _ ->
        :ok
    end

    {res, bs1}
  end

  defp eval_field({:bin_element, line, e, size0, options0}, bs0, fun) do
    {:value, v, bs1} = fun.(e, bs0)
    {size1, [type, {:unit, unit}, sign, endian]} = make_bit_type(line, size0, options0)
    {:value, size, bs} = fun.(size1, bs1)
    {eval_exp_field1(v, size, unit, type, endian, sign), bs}
  end

  defp eval_exp_field1(v, size, unit, type, endian, sign) do
    try do
      eval_exp_field(v, size, unit, type, endian, sign)
    catch
      :error, :system_limit ->
        :erlang.raise(
          :error,
          :system_limit,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          )
        )

      :error, _ ->
        :erlang.raise(
          :error,
          :badarg,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          )
        )
    end
  end

  defp eval_exp_field(val, size, unit, :integer, :little, :signed) do
    <<val::size(size * unit)-little-signed>>
  end

  defp eval_exp_field(val, size, unit, :integer, :little, :unsigned) do
    <<val::size(size * unit)-little>>
  end

  defp eval_exp_field(val, size, unit, :integer, :native, :signed) do
    <<val::size(size * unit)-native-signed>>
  end

  defp eval_exp_field(val, size, unit, :integer, :native, :unsigned) do
    <<val::size(size * unit)-native>>
  end

  defp eval_exp_field(val, size, unit, :integer, :big, :signed) do
    <<val::size(size * unit)-signed>>
  end

  defp eval_exp_field(val, size, unit, :integer, :big, :unsigned) do
    <<val::size(size * unit)>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf8, _, _) do
    <<val::utf8>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf16, :big, _) do
    <<val::big-utf16>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf16, :little, _) do
    <<val::little-utf16>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf16, :native, _) do
    <<val::native-utf16>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf32, :big, _) do
    <<val::big-utf32>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf32, :little, _) do
    <<val::little-utf32>>
  end

  defp eval_exp_field(val, _Size, _Unit, :utf32, :native, _) do
    <<val::native-utf32>>
  end

  defp eval_exp_field(val, size, unit, :float, :little, _) do
    <<val::size(size * unit)-float-little>>
  end

  defp eval_exp_field(val, size, unit, :float, :native, _) do
    <<val::size(size * unit)-float-native>>
  end

  defp eval_exp_field(val, size, unit, :float, :big, _) do
    <<val::size(size * unit)-float>>
  end

  defp eval_exp_field(val, :all, unit, :binary, _, _) do
    case bit_size(val) do
      size when rem(size, unit) === 0 ->
        <<val::size(size)-binary-unit(1)>>

      _ ->
        :erlang.raise(
          :error,
          :badarg,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          )
        )
    end
  end

  defp eval_exp_field(val, size, unit, :binary, _, _) do
    <<val::size(size * unit)-binary-unit(1)>>
  end

  def bin_gen({:bin, _, fs}, bin, bs0, bBs0, mfun, efun) do
    bin_gen(fs, bin, bs0, bBs0, mfun, efun, true)
  end

  defp bin_gen([f | fs], bin, bs0, bBs0, mfun, efun, flag)
       when is_function(mfun, 2) and is_function(efun, 2) do
    case bin_gen_field(f, bin, bs0, bBs0, mfun, efun) do
      {:match, bs, bBs, rest} ->
        bin_gen(fs, rest, bs, bBs, mfun, efun, flag)

      {:nomatch, rest} ->
        bin_gen(fs, rest, bs0, bBs0, mfun, efun, false)

      :done ->
        :done
    end
  end

  defp bin_gen([], bin, bs0, _BBs0, _Mfun, _Efun, true) do
    {:match, bin, bs0}
  end

  defp bin_gen([], bin, _Bs0, _BBs0, _Mfun, _Efun, false) do
    {:nomatch, bin}
  end

  defp bin_gen_field(
         {:bin_element, _, {:string, _, s}, :default, :default},
         bin,
         bs,
         bBs,
         _Mfun,
         _Efun
       ) do
    bits =
      try do
        :erlang.list_to_binary(s)
      catch
        _, _ ->
          <<>>
      end

    size = length(s)

    case bin do
      <<^bits::size(size)-binary, rest::bitstring>> ->
        {:match, bs, bBs, rest}

      <<_::size(size)-binary, rest::bitstring>> ->
        {:nomatch, rest}

      _ ->
        :done
    end
  end

  defp bin_gen_field(
         {:bin_element, line, {:string, sLine, s}, size0, options0},
         bin0,
         bs0,
         bBs0,
         mfun,
         efun
       ) do
    {size1, [type, {:unit, unit}, sign, endian]} = make_bit_type(line, size0, options0)
    {:value, size, _BBs} = efun.(size1, bBs0)

    f = fn c, bin, bs, bBs ->
      bin_gen_field1(bin, type, size, unit, sign, endian, {:integer, sLine, c}, bs, bBs, mfun)
    end

    bin_gen_field_string(s, bin0, bs0, bBs0, f)
  end

  defp bin_gen_field({:bin_element, line, vE, size0, options0}, bin, bs0, bBs0, mfun, efun) do
    {size1, [type, {:unit, unit}, sign, endian]} = make_bit_type(line, size0, options0)
    v = :erl_eval.partial_eval(vE)
    newV = coerce_to_float(v, type)
    {:value, size, _BBs} = efun.(size1, bBs0)
    bin_gen_field1(bin, type, size, unit, sign, endian, newV, bs0, bBs0, mfun)
  end

  defp bin_gen_field_string([], rest, bs, bBs, _F) do
    {:match, bs, bBs, rest}
  end

  defp bin_gen_field_string([c | cs], bin0, bs0, bBs0, fun) do
    case fun.(c, bin0, bs0, bBs0) do
      {:match, bs, bBs, rest} ->
        bin_gen_field_string(cs, rest, bs, bBs, fun)

      {:nomatch, rest} ->
        {:nomatch, rest}

      :done ->
        :done
    end
  end

  defp bin_gen_field1(bin, type, size, unit, sign, endian, newV, bs0, bBs0, mfun) do
    case (try do
            get_value(bin, type, size, unit, sign, endian)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {val, <<_::bitstring>> = rest} ->
        case (try do
                mfun.(:match, {newV, val, bs0})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:match, bs} ->
            bBs = add_bin_binding(mfun, newV, bs, bBs0)
            {:match, bs, bBs, rest}

          _ ->
            {:nomatch, rest}
        end

      _ ->
        :done
    end
  end

  def match_bits(fs, bin, bs0, bBs, mfun, efun, _) do
    match_bits(fs, bin, bs0, bBs, mfun, efun)
  end

  def match_bits(fs, bin, bs0, bBs, mfun, efun)
      when is_function(mfun, 2) and is_function(efun, 2) do
    case (try do
            match_bits_1(fs, bin, bs0, bBs, mfun, efun)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:match, bs} ->
        {:match, bs}

      :invalid ->
        throw(:invalid)

      _Error ->
        throw(:nomatch)
    end
  end

  defp match_bits_1([], <<>>, bs, _BBs, _Mfun, _Efun) do
    {:match, bs}
  end

  defp match_bits_1([f | fs], bits0, bs0, bBs0, mfun, efun) do
    {bs, bBs, bits} = match_field_1(f, bits0, bs0, bBs0, mfun, efun)
    match_bits_1(fs, bits, bs, bBs, mfun, efun)
  end

  defp match_field_1(
         {:bin_element, _, {:string, _, s}, :default, :default},
         bin,
         bs,
         bBs,
         _Mfun,
         _Efun
       ) do
    bits = :erlang.list_to_binary(s)
    size = byte_size(bits)
    <<^bits::size(size)-binary, rest::binary-unit(1)>> = bin
    {bs, bBs, rest}
  end

  defp match_field_1(
         {:bin_element, line, {:string, sLine, s}, size0, options0},
         bin0,
         bs0,
         bBs0,
         mfun,
         efun
       ) do
    {size1, [type, {:unit, unit}, sign, endian]} = make_bit_type(line, size0, options0)
    size2 = :erl_eval.partial_eval(size1)
    {:value, size, _BBs} = efun.(size2, bBs0)

    f = fn c, bin, bs, bBs ->
      match_field(bin, type, size, unit, sign, endian, {:integer, sLine, c}, bs, bBs, mfun)
    end

    match_field_string(s, bin0, bs0, bBs0, f)
  end

  defp match_field_1({:bin_element, line, vE, size0, options0}, bin, bs0, bBs0, mfun, efun) do
    {size1, [type, {:unit, unit}, sign, endian]} = make_bit_type(line, size0, options0)
    v = :erl_eval.partial_eval(vE)
    newV = coerce_to_float(v, type)
    size2 = :erl_eval.partial_eval(size1)
    {:value, size, _BBs} = efun.(size2, bBs0)
    match_field(bin, type, size, unit, sign, endian, newV, bs0, bBs0, mfun)
  end

  defp match_field_string([], rest, bs, bBs, _Fun) do
    {bs, bBs, rest}
  end

  defp match_field_string([c | cs], bin0, bs0, bBs0, fun) do
    {bs, bBs, bin} = fun.(c, bin0, bs0, bBs0)
    match_field_string(cs, bin, bs, bBs, fun)
  end

  defp match_field(bin, type, size, unit, sign, endian, newV, bs0, bBs0, mfun) do
    {val, rest} = get_value(bin, type, size, unit, sign, endian)
    {:match, bs} = mfun.(:match, {newV, val, bs0})
    bBs = add_bin_binding(mfun, newV, bs, bBs0)
    {bs, bBs, rest}
  end

  defp coerce_to_float({:integer, l, i} = e, :float) do
    try do
      {:float, l, :erlang.float(i)}
    catch
      :error, :badarg ->
        e

      :error, :badarith ->
        e
    end
  end

  defp coerce_to_float(e, _Type) do
    e
  end

  defp add_bin_binding(_, {:var, _, :_}, _Bs, bBs) do
    bBs
  end

  defp add_bin_binding(mfun, {:var, _, name}, bs, bBs) do
    {:value, value} = mfun.(:binding, {name, bs})
    mfun.(:add_binding, {name, value, bBs})
  end

  defp add_bin_binding(_, _, _Bs, bBs) do
    bBs
  end

  defp get_value(bin, :integer, size, unit, sign, endian) do
    get_integer(bin, size * unit, sign, endian)
  end

  defp get_value(bin, :float, size, unit, _Sign, endian) do
    get_float(bin, size * unit, endian)
  end

  defp get_value(bin, :utf8, :undefined, _Unit, _Sign, _Endian) do
    <<i::utf8, rest::bits>> = bin
    {i, rest}
  end

  defp get_value(bin, :utf16, :undefined, _Unit, _Sign, :big) do
    <<i::big-utf16, rest::bits>> = bin
    {i, rest}
  end

  defp get_value(bin, :utf16, :undefined, _Unit, _Sign, :little) do
    <<i::little-utf16, rest::bits>> = bin
    {i, rest}
  end

  defp get_value(bin, :utf16, :undefined, _Unit, _Sign, :native) do
    <<i::native-utf16, rest::bits>> = bin
    {i, rest}
  end

  defp get_value(bin, :utf32, :undefined, _Unit, _Sign, :big) do
    <<val::big-utf32, rest::bits>> = bin
    {val, rest}
  end

  defp get_value(bin, :utf32, :undefined, _Unit, _Sign, :little) do
    <<val::little-utf32, rest::bits>> = bin
    {val, rest}
  end

  defp get_value(bin, :utf32, :undefined, _Unit, _Sign, :native) do
    <<val::native-utf32, rest::bits>> = bin
    {val, rest}
  end

  defp get_value(bin, :binary, :all, unit, _Sign, _Endian) do
    0 = rem(bit_size(bin), unit)
    {bin, <<>>}
  end

  defp get_value(bin, :binary, size, unit, _Sign, _Endian) do
    totSize = size * unit
    <<val::size(totSize)-bitstring, rest::bits>> = bin
    {val, rest}
  end

  defp get_integer(bin, size, :signed, :little) do
    <<val::size(size)-little-signed, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_integer(bin, size, :unsigned, :little) do
    <<val::size(size)-little, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_integer(bin, size, :signed, :native) do
    <<val::size(size)-native-signed, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_integer(bin, size, :unsigned, :native) do
    <<val::size(size)-native, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_integer(bin, size, :signed, :big) do
    <<val::size(size)-signed, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_integer(bin, size, :unsigned, :big) do
    <<val::size(size), rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_float(bin, size, :little) do
    <<val::size(size)-float-little, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_float(bin, size, :native) do
    <<val::size(size)-float-native, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp get_float(bin, size, :big) do
    <<val::size(size)-float, rest::binary-unit(1)>> = bin
    {val, rest}
  end

  defp make_bit_type(line, :default, type0) do
    case :erl_bits.set_bit_type(:default, type0) do
      {:ok, :all, bt} ->
        {{:atom, line, :all}, :erl_bits.as_list(bt)}

      {:ok, :undefined, bt} ->
        {{:atom, line, :undefined}, :erl_bits.as_list(bt)}

      {:ok, size, bt} ->
        {{:integer, line, size}, :erl_bits.as_list(bt)}

      {:error, reason} ->
        :erlang.raise(
          :error,
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          )
        )
    end
  end

  defp make_bit_type(_Line, size, type0) do
    case :erl_bits.set_bit_type(size, type0) do
      {:ok, ^size, bt} ->
        {size, :erl_bits.as_list(bt)}

      {:error, reason} ->
        :erlang.raise(
          :error,
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          )
        )
    end
  end
end
