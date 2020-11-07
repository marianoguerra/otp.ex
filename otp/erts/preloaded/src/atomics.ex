defmodule :m_atomics do
  use Bitwise

  def new(arity, opts) do
    :erts_internal.atomics_new(
      arity,
      encode_opts(opts, 1 <<< 0)
    )
  end

  defp encode_opts([{:signed, true} | t], acc) do
    encode_opts(t, acc ||| 1 <<< 0)
  end

  defp encode_opts([{:signed, false} | t], acc) do
    encode_opts(t, acc &&& ~~~(1 <<< 0))
  end

  defp encode_opts([], acc) do
    acc
  end

  defp encode_opts(_, _) do
    :erlang.error(:badarg)
  end

  def put(_Ref, _Ix, _Value) do
    :erlang.nif_error(:undef)
  end

  def get(_Ref, _Ix) do
    :erlang.nif_error(:undef)
  end

  def add(_Ref, _Ix, _Incr) do
    :erlang.nif_error(:undef)
  end

  def add_get(_Ref, _Ix, _Incr) do
    :erlang.nif_error(:undef)
  end

  def sub(ref, ix, decr) do
    :atomics.add(ref, ix, -decr)
  end

  def sub_get(ref, ix, decr) do
    :atomics.add_get(ref, ix, -decr)
  end

  def exchange(_Ref, _Ix, _Desired) do
    :erlang.nif_error(:undef)
  end

  def compare_exchange(_Ref, _Ix, _Expected, _Desired) do
    :erlang.nif_error(:undef)
  end

  def info(_Ref) do
    :erlang.nif_error(:undef)
  end
end
