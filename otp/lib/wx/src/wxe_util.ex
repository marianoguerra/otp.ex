defmodule :m_wxe_util do
  use Bitwise
  require Record
  Record.defrecord(:r_wx_ref, :wx_ref, ref: :undefined, type: :undefined, state: [])
  Record.defrecord(:r_wx_env, :wx_env, port: :undefined, sv: :undefined, debug: 0)

  Record.defrecord(:r_wx_mem, :wx_mem,
    bin: :undefined,
    size: :undefined
  )

  Record.defrecord(:r_evh, :evh,
    et: :null,
    id: -1,
    lastId: -1,
    cb: 0,
    skip: :undefined,
    userdata: [],
    handler: :undefined
  )

  def to_bool(0) do
    false
  end

  def to_bool(_) do
    true
  end

  def from_bool(true) do
    1
  end

  def from_bool(false) do
    0
  end

  def colour_bin({r, g, b}) do
    <<r::size(32)-unsigned-native, g::size(32)-unsigned-native, b::size(32)-unsigned-native,
      255::size(32)-unsigned-native>>
  end

  def colour_bin({r, g, b, a}) do
    <<r::size(32)-unsigned-native, g::size(32)-unsigned-native, b::size(32)-unsigned-native,
      a::size(32)-unsigned-native>>
  end

  def datetime_bin({{y, mo, d}, {h, mi, s}}) do
    <<d::size(32)-unsigned-native, mo - 1::size(32)-unsigned-native, y::size(32)-unsigned-native,
      h::size(32)-unsigned-native, mi::size(32)-unsigned-native, s::size(32)-unsigned-native>>
  end

  def get_const(id) do
    [{^id, data}] = :ets.lookup(:wx_non_consts, id)
    data
  end

  def cast(op, args) do
    r_wx_env(port: port, debug: dbg) = :wx.get_env()
    _ = :erlang.port_control(port, op, args)

    case dbg > 0 do
      true ->
        debug_cast(dbg &&& 15, op, args, port)

      false ->
        :ok
    end

    :ok
  end

  def call(op, args) do
    r_wx_env(port: port, debug: dbg) = :wx.get_env()

    case dbg > 0 do
      false ->
        _ = :erlang.port_control(port, op, args)
        rec(op)

      true ->
        debug_call(dbg &&& 15, op, args, port)
    end
  end

  defp rec(op) do
    receive do
      {:_wxe_result_, res} ->
        res

      {:_wxe_error_, ^op, error} ->
        [{_, mF}] = :ets.lookup(:wx_debug_info, op)
        :erlang.error({error, mF})

      {:_wxe_error_, old, error} ->
        [{_, {m, f, a}}] = :ets.lookup(:wx_debug_info, old)
        msg = :io_lib.format('~p in ~w:~w/~w', [error, m, f, a])
        send(:wxe_master, {:wxe_driver, :error, msg})
        rec(op)
    end
  end

  def construct(op, args) do
    call(op, args)
  end

  def destroy(op, r_wx_ref(ref: ref)) do
    cast(op, <<ref::size(32)-unsigned-native>>)
  end

  def register_pid(r_wx_ref(ref: ref)) do
    call(7, <<ref::size(32)-unsigned-native>>)
  end

  def send_bin(bin) when is_binary(bin) do
    r_wx_env(port: port, debug: dbg) = :wx.get_env()

    case dbg > 0 do
      false ->
        :erlang.port_command(port, bin)

      true ->
        :io.format('WX binary ~p(~p) ~n', [self(), port])
        :erlang.port_command(port, bin)
    end
  end

  def get_cbId(fun) do
    :gen_server.call(r_wx_env(:wx.get_env(), :sv), {:register_cb, fun}, :infinity)
  end

  def connect_cb(object, evData0 = r_evh(cb: callback)) do
    server = r_wx_env(:wx.get_env(), :sv)

    case callback do
      0 ->
        case :wxEvtHandler.connect_impl(object, evData0) do
          {:ok, listener} ->
            evData =
              r_evh(evData0,
                handler: listener,
                userdata: :undefined
              )

            :gen_server.call(server, {:connect_cb, object, evData}, :infinity)

          error ->
            error
        end

      _ ->
        :gen_server.call(server, {:connect_cb, object, evData0}, :infinity)
    end
  end

  def disconnect_cb(object, evData) do
    server = r_wx_env(:wx.get_env(), :sv)

    {:try_in_order, handlers} =
      :gen_server.call(
        server,
        {:disconnect_cb, object, evData},
        :infinity
      )

    disconnect(object, handlers)
  end

  defp disconnect(object, [ev | evs]) do
    try do
      :wxEvtHandler.disconnect_impl(object, ev)
    catch
      _, _ ->
        false
    else
      true ->
        true

      false ->
        disconnect(object, evs)

      error ->
        error
    end
  end

  defp disconnect(_, []) do
    false
  end

  defp debug_cast(1, op, _Args, _Port) do
    check_previous()

    case :ets.lookup(:wx_debug_info, op) do
      [{_, {m, f, _}}] ->
        :io.format('WX ~p: ~s:~s(~p) -> ok~n', [self(), m, f, op])

      [] ->
        :io.format('WX ~p: unknown(~p) -> ok~n', [self(), op])
    end
  end

  defp debug_cast(2, op, args, port) do
    check_previous()

    case :ets.lookup(:wx_debug_info, op) do
      [{_, {m, f, _}}] ->
        :io.format('WX ~p(~p): ~s:~s(~p) (~p) -> ok~n', [self(), port, m, f, op, args])

      [] ->
        :io.format('WX ~p(~p): unknown(~p) (~p) -> ok~n', [self(), port, op, args])
    end
  end

  defp debug_cast(_, _Op, _Args, _Port) do
    check_previous()
    :ok
  end

  defp debug_call(1, op, args, port) do
    check_previous()

    case :ets.lookup(:wx_debug_info, op) do
      [{_, {m, f, _}}] ->
        :io.format('WX ~p: ~s:~s(~p) -> ', [self(), m, f, op])

      [] ->
        :io.format('WX ~p: unknown(~p) -> ', [self(), op])
    end

    _ = :erlang.port_control(port, op, args)
    debug_rec(1)
  end

  defp debug_call(2, op, args, port) do
    check_previous()

    case :ets.lookup(:wx_debug_info, op) do
      [{_, {m, f, _}}] ->
        :io.format('WX ~p(~p): ~s:~s(~p) (~p) -> ', [self(), port, m, f, op, args])

      [] ->
        :io.format('WX ~p(~p): unknown(~p) (~p) -> ', [self(), port, op, args])
    end

    _ = :erlang.port_control(port, op, args)
    debug_rec(2)
  end

  defp debug_call(_, op, args, port) do
    check_previous()
    _ = :erlang.port_control(port, op, args)
    rec(op)
  end

  defp debug_rec(1) do
    receive do
      {:_wxe_result_, res} ->
        :io.format('complete ~n', [])
        res

      {:_wxe_error_, op2, error} ->
        [{_, mF2}] = :ets.lookup(:wx_debug_info, op2)
        :erlang.error({error, mF2})
    end
  end

  defp debug_rec(2) do
    receive do
      {:_wxe_result_, res} ->
        :io.format('~p ~n', [res])
        res

      {:_wxe_error_, op, error} ->
        :io.format('Error ~p ~n', [error])
        [{_, mF}] = :ets.lookup(:wx_debug_info, op)
        :erlang.error({error, mF})
    end
  end

  defp check_previous() do
    receive do
      {:_wxe_error_, op, error} ->
        [{_, mF = {m, f, _}}] = :ets.lookup(:wx_debug_info, op)
        :io.format('WX ~p: ERROR in previous command ~s:~s~n', [self(), m, f])
        :erlang.error({error, mF})
    after
      0 ->
        :ok
    end
  end

  def wxgl_dl() do
    dynLib0 = 'erl_gl'
    privDir = priv_dir(dynLib0, false)

    dynLib =
      case :os.type() do
        {:win32, _} ->
          dynLib0 ++ '.dll'

        _ ->
          dynLib0 ++ '.so'
      end

    :filename.join(privDir, dynLib)
  end

  def priv_dir(driver0, silent) do
    {:file, path} = :code.is_loaded(:wxe_util)

    priv =
      case :filelib.is_regular(path) do
        true ->
          beam = :filename.join(['ebin/', :erlang.atom_to_list(:wxe_util) ++ '.beam'])
          :filename.join(strip(path, beam), 'priv')

        false ->
          :code.priv_dir(:wx)
      end

    driver =
      case :os.type() do
        {:win32, _} ->
          driver0 ++ '.dll'

        _ ->
          driver0 ++ '.so'
      end

    case :file.read_file_info(
           :filename.join(
             priv,
             driver
           )
         ) do
      {:ok, _} ->
        priv

      {:error, _} ->
        srcPriv =
          :filename.join(
            priv,
            :erlang.system_info(:system_architecture)
          )

        case :file.read_file_info(
               :filename.join(
                 srcPriv,
                 driver
               )
             ) do
          {:ok, _} ->
            srcPriv

          {:error, _} ->
            opt_error_log(silent, 'ERROR: Could not find \'~s\' in: ~s~n', [driver, priv])
            :erlang.error({:load_driver, 'No driver found'})
        end
    end
  end

  defp strip(src, src) do
    []
  end

  defp strip([h | r], src) do
    [h | strip(r, src)]
  end

  def opt_error_log(false, format, args) do
    :error_logger.format(format, args)
  end

  def opt_error_log(true, _Format, _Args) do
    :ok
  end
end
