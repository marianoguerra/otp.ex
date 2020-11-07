defmodule :m_etop_tr do
  use Bitwise
  import :etop, only: [getopt: 2]
  @author :"siri@erix.ericsson.se"
  require Record

  Record.defrecord(:r_opts, :opts,
    node: node(),
    port: 8415,
    accum: false,
    intv: 5000,
    lines: 10,
    width: 700,
    height: 340,
    sort: :runtime,
    tracing: :on,
    out_mod: :etop_txt,
    out_proc: :undefined,
    server: :undefined,
    host: :undefined,
    tracer: :undefined,
    store: :undefined,
    accum_tab: :undefined,
    remote: :undefined
  )

  def setup_tracer(config) do
    traceNode = getopt(:node, config)
    rHost = :rpc.call(traceNode, :net_adm, :localhost, [])
    store = :ets.new(:etop_tr, [:set, :public])

    case :erlang.whereis(:dbg) do
      :undefined ->
        case :rpc.call(traceNode, :erlang, :whereis, [:dbg]) do
          :undefined ->
            :fine

          pid ->
            :erlang.exit(pid, :kill)
        end

      pid ->
        :erlang.exit(pid, :kill)
    end

    :dbg.tracer(traceNode, :port, :dbg.trace_port(:ip, {getopt(:port, config), 5000}))
    :dbg.p(:all, [:running, :timestamp])
    t = :dbg.get_tracer(traceNode)
    r_opts(config, tracer: t, host: rHost, store: store)
  end

  def stop_tracer(_Config) do
    :dbg.p(:all, :clear)
    :dbg.stop()
    :ok
  end

  def reader(config) do
    host = getopt(:host, config)
    port = getopt(:port, config)
    {:ok, sock} = :gen_tcp.connect(host, port, [{:active, false}])

    spawn_link(fn ->
      reader_init(sock, getopt(:store, config), [])
    end)
  end

  defp reader_init(sock, store, last) do
    :erlang.process_flag(:priority, :high)
    reader(sock, store, last)
  end

  defp reader(sock, store, last) do
    data = get_data(sock)
    new = handle_data(last, data, store)
    reader(sock, store, new)
  end

  defp handle_data(last, {_, pid, :in, _, time}, _) do
    [{pid, time} | last]
  end

  defp handle_data([], {_, _, :out, _, _}, _Store) do
    []
  end

  defp handle_data(last, {_, pid, :out, _, time2} = g, store) do
    case :lists.keytake(pid, 1, last) do
      {_, {_, time1}, new} ->
        elapsed = elapsed(time1, time2)

        case :ets.member(store, pid) do
          true ->
            :ets.update_counter(store, pid, elapsed)

          false ->
            :ets.insert(store, {pid, elapsed})
        end

        new

      false ->
        :io.format('Erlang top got garbage ~tp~n', [g])
        last
    end
  end

  defp handle_data(_W, {:drop, d}, _) do
    :io.format('Erlang top dropped data ~p~n', [d])
    []
  end

  defp handle_data(last, g, _) do
    :io.format('Erlang top got garbage ~tp~n', [g])
    last
  end

  defp elapsed({me1, s1, mi1}, {me2, s2, mi2}) do
    me = (me2 - me1) * 1_000_000
    s = (s2 - s1 + me) * 1_000_000
    mi2 - mi1 + s
  end

  defp get_data(sock) do
    [op | bESiz] = my_ip_read(sock, 5)
    siz = get_be(bESiz)

    case op do
      0 ->
        b = :erlang.list_to_binary(my_ip_read(sock, siz))
        :erlang.binary_to_term(b)

      1 ->
        {:drop, siz}

      else__ ->
        exit({:"bad trace tag", else__})
    end
  end

  defp get_be([a, b, c, d]) do
    a * 16_777_216 + b * 65536 + c * 256 + d
  end

  defp my_ip_read(sock, n) do
    case :gen_tcp.recv(sock, n) do
      {:ok, data} ->
        case length(data) do
          ^n ->
            data

          x ->
            data ++ my_ip_read(sock, n - x)
        end

      _Else ->
        exit(:eof)
    end
  end
end
