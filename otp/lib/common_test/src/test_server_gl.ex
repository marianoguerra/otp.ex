defmodule :m_test_server_gl do
  use Bitwise
  require Record

  Record.defrecord(:r_st, :st,
    tc_supervisor: :undefined,
    tc: :undefined,
    minor: :undefined,
    minor_monitor: :undefined,
    tsio_monitor: :undefined,
    capture: :undefined,
    reject_io: :undefined,
    permit_io: :undefined,
    auto_nl: true,
    levels: :undefined,
    escape_chars: true
  )

  def start_link(tSIO) do
    case :gen_server.start_link(:test_server_gl, [tSIO], []) do
      {:ok, pid} ->
        {:ok, pid}

      other ->
        other
    end
  end

  def stop(gL) do
    :gen_server.cast(gL, :stop)
  end

  def set_minor_fd(gL, fd, mFA) do
    req(gL, {:set_minor_fd, fd, mFA, self()})
  end

  def unset_minor_fd(gL) do
    req(gL, :unset_minor_fd)
  end

  def get_tc_supervisor(gL) do
    req(gL, :get_tc_supervisor)
  end

  def print(gL, detail, msg, printer) do
    req(gL, {:print, detail, msg, printer})
  end

  def set_props(gL, propList) do
    req(gL, {:set_props, propList})
  end

  def init([tSIO]) do
    :ct_util.mark_process(:group_leader)

    escChars =
      case :application.get_env(
             :test_server,
             :esc_chars
           ) do
        {:ok, eCBool} ->
          eCBool

        _ ->
          true
      end

    ref = :erlang.monitor(:process, tSIO)

    {:ok,
     r_st(
       tc_supervisor: :none,
       minor: :none,
       minor_monitor: :none,
       tsio_monitor: ref,
       capture: :none,
       reject_io: false,
       permit_io: :gb_sets.empty(),
       auto_nl: true,
       levels: {1, 19, 10},
       escape_chars: escChars
     )}
  end

  defp req(gL, req) do
    :gen_server.call(gL, req, :infinity)
  end

  def handle_call(:get_tc_supervisor, _From, r_st(tc_supervisor: pid) = st) do
    {:reply, pid, st}
  end

  def handle_call({:set_minor_fd, fd, mFA, supervisor}, _From, st) do
    ref = :erlang.monitor(:process, fd)
    {:reply, :ok, r_st(st, tc: mFA, minor: fd, minor_monitor: ref, tc_supervisor: supervisor)}
  end

  def handle_call(:unset_minor_fd, _From, st) do
    {:reply, :ok, r_st(st, minor: :none, tc_supervisor: :none)}
  end

  def handle_call({:set_props, propList}, _From, st) do
    {:reply, :ok, do_set_props(propList, st)}
  end

  def handle_call({:print, detail, msg, printer}, {from, _}, st) do
    output(detail, msg, printer, from, st)
    {:reply, :ok, st}
  end

  def handle_cast(:stop, st) do
    {:stop, :normal, st}
  end

  def handle_info(
        {:DOWN, ref, :process, _, reason} = d,
        r_st(minor_monitor: ref) = st
      ) do
    case reason do
      :normal ->
        :ok

      _ ->
        data =
          :io_lib.format('=== WARNING === TC: ~tw\nGot down from minor Fd ~w: ~tw\n\n', [
            r_st(st, :tc),
            r_st(st, :minor),
            d
          ])

        :test_server_io.print_unexpected(data)
    end

    {:noreply, r_st(st, minor: :none, minor_monitor: :none)}
  end

  def handle_info(
        {:DOWN, ref, :process, _, _},
        r_st(tsio_monitor: ref) = st
      ) do
    {:stop, :normal, st}
  end

  def handle_info({:permit_io, pid}, r_st(permit_io: p) = st) do
    {:noreply, r_st(st, permit_io: :gb_sets.add(pid, p))}
  end

  def handle_info({:capture, cap0}, st) do
    cap =
      case cap0 do
        false ->
          :none

        pid when is_pid(cap0) ->
          pid
      end

    {:noreply, r_st(st, capture: cap)}
  end

  def handle_info({:io_request, from, replyAs, req} = ioReq, st) do
    _ =
      try do
        io_req(req, from, st)
      catch
        _, _ ->
          send(from, {:io_reply, replyAs, {:error, :arguments}})
      else
        :passthrough ->
          send(:erlang.group_leader(), ioReq)

        {escapeHtml, data} ->
          case is_io_permitted(from, st) do
            false ->
              :ok

            true ->
              case st do
                r_st(capture: :none) ->
                  :ok

                r_st(capture: capturePid) ->
                  send(capturePid, {:captured, data})
                  :ok
              end

              case escapeHtml and r_st(st, :escape_chars) do
                true ->
                  output(:minor, :test_server_ctrl.escape_chars(data), from, from, st)

                false ->
                  output(:minor, data, from, from, st)
              end
          end

          send(from, {:io_reply, replyAs, :ok})
      end

    {:noreply, st}
  end

  def handle_info(
        {:structured_io, clientPid, {detail, str}},
        st
      ) do
    output(detail, str, clientPid, clientPid, st)
    {:noreply, st}
  end

  def handle_info({:printout, detail, ['$tc_html', format], args}, st) do
    str = :io_lib.format(format, args)
    output(detail, ['$tc_html', str], :internal, :none, st)
    {:noreply, st}
  end

  def handle_info({:printout, detail, fun}, st)
      when is_function(fun) do
    output(detail, fun, :internal, :none, st)
    {:noreply, st}
  end

  def handle_info({:printout, detail, format, args}, st) do
    str = :io_lib.format(format, args)

    cond do
      not r_st(st, :escape_chars) ->
        output(detail, ['$tc_html', str], :internal, :none, st)

      true ->
        output(detail, str, :internal, :none, st)
    end

    {:noreply, st}
  end

  def handle_info(msg, r_st(tc_supervisor: pid) = st)
      when is_pid(pid) do
    send(pid, msg)
    {:noreply, st}
  end

  def handle_info(_Msg, r_st() = st) do
    {:noreply, st}
  end

  def terminate(_, _) do
    :ok
  end

  defp do_set_props([{:levels, levels} | ps], st) do
    do_set_props(ps, r_st(st, levels: levels))
  end

  defp do_set_props([{:auto_nl, autoNL} | ps], st) do
    do_set_props(ps, r_st(st, auto_nl: autoNL))
  end

  defp do_set_props([{:reject_io_reqs, bool} | ps], st) do
    do_set_props(ps, r_st(st, reject_io: bool))
  end

  defp do_set_props([], st) do
    st
  end

  defp io_req({:put_chars, enc, str}, _, _)
       when enc === :latin1 or enc === :unicode do
    case str do
      ['$tc_html', str0] ->
        {false, :unicode.characters_to_list(str0, enc)}

      _ ->
        {true, :unicode.characters_to_list(str, enc)}
    end
  end

  defp io_req({:put_chars, encoding, mod, func, [format, args]}, _, _) do
    case format do
      ['$tc_html', format0] ->
        str = apply(mod, func, [format0, args])
        {false, :unicode.characters_to_list(str, encoding)}

      _ ->
        str = apply(mod, func, [format, args])
        {true, :unicode.characters_to_list(str, encoding)}
    end
  end

  defp io_req(_, _, _) do
    :passthrough
  end

  defp output(level, strOrFun, sender, from, st)
       when is_integer(level) do
    case selected_by_level(level, :stdout, st) do
      true when hd(strOrFun) == '$tc_html' ->
        output(:stdout, tl(strOrFun), sender, from, st)

      true when is_function(strOrFun) ->
        output(:stdout, strOrFun.(:stdout), sender, from, st)

      true ->
        output(:stdout, strOrFun, sender, from, st)

      false ->
        :ok
    end

    case selected_by_level(level, :major, st) do
      true when hd(strOrFun) == '$tc_html' ->
        output(:major, tl(strOrFun), sender, from, st)

      true when is_function(strOrFun) ->
        output(:major, strOrFun.(:major), sender, from, st)

      true ->
        output(:major, strOrFun, sender, from, st)

      false ->
        :ok
    end

    case selected_by_level(level, :minor, st) do
      true when hd(strOrFun) == '$tc_html' ->
        output(:minor, tl(strOrFun), sender, from, st)

      true when is_function(strOrFun) ->
        output(:minor, strOrFun.(:minor), sender, from, st)

      true ->
        output(:minor, :test_server_ctrl.escape_chars(strOrFun), sender, from, st)

      false ->
        :ok
    end
  end

  defp output(:stdout, str, _Sender, from, st) do
    output_to_file(:stdout, str, from, st)
  end

  defp output(:html, str, _Sender, from, st) do
    output_to_file(:html, str, from, st)
  end

  defp output(level, str, sender, from, st)
       when is_atom(level) do
    output_to_file(level, dress_output(str, sender, st), from, st)
  end

  defp output_to_file(:minor, data0, from, r_st(tc: {m, f, a}, minor: :none)) do
    data = [:io_lib.format('=== ~w:~tw/~w\n', [m, f, a]), data0]
    :test_server_io.print(from, :unexpected_io, data)
    :ok
  end

  defp output_to_file(:minor, data, from, r_st(tc: tC, minor: fd)) do
    try do
      :io.put_chars(fd, data)
    catch
      type, reason ->
        data1 = [
          :io_lib.format(
            '=== ERROR === TC: ~tw\nFailed to write to minor Fd: ~w\nType: ~w\nReason: ~tw\n',
            [tC, fd, type, reason]
          ),
          data,
          '\n'
        ]

        :test_server_io.print(from, :unexpected_io, data1)
    end
  end

  defp output_to_file(detail, data, from, _) do
    :test_server_io.print(from, detail, data)
  end

  defp is_io_permitted(from, r_st(reject_io: true, permit_io: p)) do
    :gb_sets.is_member(from, p)
  end

  defp is_io_permitted(_, r_st(reject_io: false)) do
    true
  end

  defp selected_by_level(level, :stdout, r_st(levels: {stdout, _, _})) do
    level <= stdout
  end

  defp selected_by_level(level, :major, r_st(levels: {_, major, _})) do
    level <= major
  end

  defp selected_by_level(level, :minor, r_st(levels: {_, _, minor})) do
    level >= minor
  end

  defp dress_output([?= | _] = str, :internal, _) do
    [str, ?\n]
  end

  defp dress_output(str, :internal, _) do
    ['=== ', str, ?\n]
  end

  defp dress_output(str, _, r_st(auto_nl: autoNL)) do
    case autoNL do
      true ->
        [str, ?\n]

      false ->
        str
    end
  end
end
