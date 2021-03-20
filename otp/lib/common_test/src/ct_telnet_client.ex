defmodule :m_ct_telnet_client do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    conn_name: :undefined,
    get_data: :undefined,
    keep_alive: true,
    log_pos: 1
  )

  def open(server, connName) do
    open(server, 23, 10000, true, false, connName)
  end

  def open(server, port, connName) do
    open(server, port, 10000, true, false, connName)
  end

  def open(server, port, timeout, connName) do
    open(server, port, timeout, true, false, connName)
  end

  def open(server, port, timeout, keepAlive, connName) do
    open(server, port, timeout, keepAlive, false, connName)
  end

  def open(server, port, timeout, keepAlive, noDelay, connName) do
    self = self()

    pid =
      spawn(fn ->
        init(self, server, port, timeout, keepAlive, noDelay, connName)
      end)

    receive do
      {:open, ^pid} ->
        {:ok, pid}

      {error, ^pid} ->
        error
    end
  end

  def close(pid) do
    send(pid, {:close, self()})

    receive do
      :closed ->
        :ok
    after
      5000 ->
        :ok
    end
  end

  def send_data(pid, data) do
    send_data(pid, data, '\n')
  end

  def send_data(pid, data, true) do
    send_data(pid, data, '\n')
  end

  def send_data(pid, data, newline) when is_list(newline) do
    send_data(pid, data ++ newline, false)
  end

  def send_data(pid, data, false) do
    send(pid, {:send_data, data})
    :ok
  end

  def get_data(pid) do
    send(pid, {:get_data, self()})

    receive do
      {:data, data} ->
        {:ok, data}
    end
  end

  defp init(parent, server, port, timeout, keepAlive, noDelay, connName) do
    :ct_util.mark_process()

    case :gen_tcp.connect(server, port, [:list, {:packet, 0}, {:nodelay, noDelay}], timeout) do
      {:ok, sock} ->
        dbg('~tp connected to: ~tp (port: ~w, keep_alive: ~w)\n', [
          connName,
          server,
          port,
          keepAlive
        ])

        send([255, 253, 3], sock, connName)
        send(parent, {:open, self()})
        loop(r_state(conn_name: connName, get_data: 10, keep_alive: keepAlive), sock, [])
        :gen_tcp.close(sock)

      error ->
        send(parent, {error, self()})
    end
  end

  defp loop(state, sock, acc) do
    receive do
      {:tcp_closed, _} ->
        dbg('Connection closed\n', [])
        data = :lists.reverse(:lists.append(acc))
        dbg('Printing queued messages: ~tp', [data])

        :ct_telnet.log(r_state(state, :conn_name), :general_io, '~ts', [
          :lists.sublist(data, r_state(state, :log_pos), length(data))
        ])

        receive do
          {:get_data, pid} ->
            send(pid, :closed)
        after
          100 ->
            :ok
        end

      {:tcp, _, msg0} ->
        dbg('rcv tcp msg: ~tp~n', [msg0])
        msg = check_msg(sock, msg0, [])
        loop(state, sock, [msg | acc])

      {:send_data, data} ->
        send(data, sock, r_state(state, :conn_name))
        loop(state, sock, acc)

      {:get_data, pid} ->
        newState =
          case acc do
            [] ->
              dbg('get_data nodata\n', [])
              :erlang.send_after(100, self(), {:get_data_delayed, pid})

              cond do
                r_state(state, :keep_alive) == true ->
                  r_state(state, get_data: r_state(state, :get_data) - 1)

                r_state(state, :keep_alive) == false ->
                  state
              end

            _ ->
              data = :lists.reverse(:lists.append(acc))
              len = length(data)
              dbg('get_data ~tp\n', [data])

              :ct_telnet.log(r_state(state, :conn_name), :general_io, '~ts', [
                :lists.sublist(
                  data,
                  r_state(state, :log_pos),
                  len
                )
              ])

              send(pid, {:data, data})
              r_state(state, log_pos: 1)
          end

        loop(newState, sock, [])

      {:get_data_delayed, pid} ->
        newState =
          case state do
            r_state(keep_alive: true, get_data: 0) ->
              dbg('sending NOP\n', [])

              cond do
                acc == [] ->
                  send([255, 241], sock, r_state(state, :conn_name))

                true ->
                  :ok
              end

              r_state(state, get_data: 10)

            _ ->
              state
          end

        {newAcc, pos} =
          case :erlang.is_process_alive(pid) do
            true when acc != [] ->
              data = :lists.reverse(:lists.append(acc))
              len = length(data)
              dbg('get_data_delayed ~tp\n', [data])

              :ct_telnet.log(r_state(state, :conn_name), :general_io, '~ts', [
                :lists.sublist(
                  data,
                  r_state(state, :log_pos),
                  len
                )
              ])

              send(pid, {:data, data})
              {[], 1}

            true when acc == [] ->
              dbg('get_data_delayed nodata\n', [])
              send(pid, {:data, []})
              {[], 1}

            false ->
              {acc, r_state(newState, :log_pos)}
          end

        loop(r_state(newState, log_pos: pos), sock, newAcc)

      {:close, pid} ->
        dbg('Closing connection\n', [])

        cond do
          acc == [] ->
            :ok

          true ->
            data = :lists.reverse(:lists.append(acc))
            dbg('Printing queued messages: ~tp', [data])

            :ct_telnet.log(r_state(state, :conn_name), :general_io, '~ts', [
              :lists.sublist(data, r_state(state, :log_pos), length(data))
            ])
        end

        :gen_tcp.close(sock)
        send(pid, :closed)
    after
      wait(r_state(state, :keep_alive), 8000) ->
        dbg('idle timeout\n', [])
        data = :lists.reverse(:lists.append(acc))

        case data do
          [] ->
            dbg('sending NOP\n', [])
            send([255, 241], sock, r_state(state, :conn_name))
            loop(state, sock, acc)

          _ when r_state(state, :log_pos) == length(data) + 1 ->
            loop(state, sock, acc)

          _ ->
            dbg('idle timeout, printing ~tp\n', [data])
            len = length(data)

            :ct_telnet.log(r_state(state, :conn_name), :general_io, '~ts', [
              :lists.sublist(data, r_state(state, :log_pos), len)
            ])

            loop(r_state(state, log_pos: len + 1), sock, acc)
        end
    end
  end

  defp wait(true, time) do
    time
  end

  defp wait(false, _) do
    :infinity
  end

  defp send(data, sock, connName) do
    case data do
      [255 | _] = cmd ->
        cmd_dbg('Sending', cmd)

        try do
          :io_lib.format('[~w] ~w', [:ct_telnet_client, data])
        catch
          _, _ ->
            :ok
        else
          str ->
            :ct_telnet.log(connName, :general_io, str, [])
        end

      _ ->
        dbg('Sending: ~tp\n', [data])

        try do
          :io_lib.format('[~w] ~ts', [:ct_telnet_client, data])
        catch
          _, _ ->
            :ok
        else
          str ->
            :ct_telnet.log(connName, :general_io, str, [])
        end
    end

    :ok = :gen_tcp.send(sock, data)
    :ok
  end

  defp check_msg(sock, [255, 255 | t], acc) do
    check_msg(sock, t, [255 | acc])
  end

  defp check_msg(sock, [255 | cs], acc) do
    case get_cmd(cs) do
      {cmd, cs1} ->
        cmd_dbg('Got', cmd)
        :ok = respond_cmd(cmd, sock)
        check_msg(sock, cs1, acc)

      :error ->
        acc
    end
  end

  defp check_msg(sock, [h | t], acc) do
    check_msg(sock, t, [h | acc])
  end

  defp check_msg(_Sock, [], acc) do
    acc
  end

  defp respond_cmd([251, 1], sock) do
    r = [255, 253, 1]
    cmd_dbg('Responding', r)
    :gen_tcp.send(sock, r)
  end

  defp respond_cmd([253, 1], sock) do
    r = [255, 251, 1]
    cmd_dbg('Responding', r)
    :gen_tcp.send(sock, r)
  end

  defp respond_cmd([251, 3], _Sock) do
    dbg('Server will suppress-go-ahead\n', [])
  end

  defp respond_cmd([252, 3], _Sock) do
    dbg('Warning! Server won\'t suppress-go-ahead\n', [])
  end

  defp respond_cmd([254 | _Opt], _Sock) do
    :ok
  end

  defp respond_cmd([252 | _Opt], _Sock) do
    :ok
  end

  defp respond_cmd([251, opt], sock) do
    r = [255, 254, opt]
    cmd_dbg('Responding', r)
    :gen_tcp.send(sock, r)
  end

  defp respond_cmd([253 | opt], sock) do
    r = [255, 252 | opt]
    cmd_dbg('Responding', r)
    :gen_tcp.send(sock, r)
  end

  defp respond_cmd(241, _Sock) do
    :ok
  end

  defp respond_cmd([cmd | opt], _Sock)
       when cmd >= 240 and
              cmd <= 255 do
    dbg('Received cmd: ~w. Ignored!\n', [[cmd | opt]])
  end

  defp respond_cmd([cmd | opt], _Sock) do
    dbg('WARNING: Received unknown cmd: ~w. Ignored!\n', [[cmd | opt]])
  end

  defp get_cmd([cmd | rest]) when cmd == 250 do
    get_subcmd(rest, [])
  end

  defp get_cmd([cmd | rest])
       when cmd >= 240 and
              cmd <= 249 do
    {241, rest}
  end

  defp get_cmd([cmd, opt | rest])
       when cmd >= 251 and
              cmd <= 254 do
    {[cmd, opt], rest}
  end

  defp get_cmd(_Other) do
    :error
  end

  defp get_subcmd([240 | rest], acc) do
    {[240 | :lists.reverse(acc)], rest}
  end

  defp get_subcmd([opt | rest], acc) do
    get_subcmd(rest, [opt | acc])
  end

  defp dbg(_Str, _Args) do
    :ok
  end

  defp cmd_dbg(_Prefix, _Cmd) do
    :ok
  end
end
