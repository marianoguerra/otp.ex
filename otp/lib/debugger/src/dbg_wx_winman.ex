defmodule :m_dbg_wx_winman do
  use Bitwise
  import Kernel, except: [raise: 1]
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_win, :win, owner: :undefined, title: :undefined, win: :undefined)
  Record.defrecord(:r_state, :state, wins: [])

  def start() do
    :gen_server.start({:local, :dbg_wx_winman}, :dbg_wx_winman, [], [])
  end

  def insert(title, win) do
    :gen_server.cast(
      :dbg_wx_winman,
      {:insert, self(), title, win}
    )
  end

  def is_started(title) do
    case :gen_server.call(:dbg_wx_winman, {:is_started, title}, :infinity) do
      {true, win} ->
        raise(win)
        true

      false ->
        false
    end
  end

  def clear_process(title) do
    :gen_server.cast(
      :dbg_wx_winman,
      {:clear_process, title}
    )
  end

  def raise(win) do
    case :wxTopLevelWindow.isIconized(win) do
      true ->
        :wxTopLevelWindow.iconize(win, [{:iconize, false}])

      false ->
        :ignore
    end

    :wxWindow.raise(win)
  end

  def update_windows_menu(win, [monInfo | infos]) do
    menu = :erlang.get(:Windows)
    oldItems = :wxMenu.getMenuItems(menu)

    for item <- oldItems do
      :wxMenu.delete(menu, item)
    end

    menuitem(win, menu, monInfo, 700)
    _ = :wxMenu.appendSeparator(menu)

    :wx.foldl(
      fn info, acc ->
        menuitem(win, menu, info, acc)
      end,
      701,
      infos
    )
  end

  defp menuitem(window, menu, {title, win}, id) do
    _ = :wxMenu.append(menu, id, title)

    :wxWindow.connect(window, :command_menu_selected, [
      {:id, id},
      {:userData, {:dbg_ui_winman, win}}
    ])

    id + 1
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    {:ok, r_state()}
  end

  def handle_call({:is_started, title}, _From, state) do
    reply =
      case :lists.keyfind(title, r_win(:title), r_state(state, :wins)) do
        false ->
          false

        win ->
          {true, r_win(win, :win)}
      end

    {:reply, reply, state}
  end

  def handle_cast({:insert, pid, title, win}, state) do
    :erlang.link(pid)
    wins = r_state(state, :wins) ++ [r_win(owner: pid, title: title, win: win)]
    inform_all(wins)
    {:noreply, r_state(state, wins: wins)}
  end

  def handle_cast({:clear_process, title}, state) do
    oldWins = r_state(state, :wins)

    wins =
      case :lists.keyfind(title, r_win(:title), oldWins) do
        r_win(owner: pid) ->
          msg = {:dbg_ui_winman, :destroy}
          send(pid, msg)
          :lists.keydelete(title, r_win(:title), oldWins)

        false ->
          oldWins
      end

    {:noreply, r_state(state, wins: wins)}
  end

  def handle_info({:EXIT, pid, _Reason}, state) do
    [mon | _Wins] = r_state(state, :wins)

    cond do
      pid === r_win(mon, :owner) ->
        {:stop, :normal, state}

      true ->
        wins2 = :lists.keydelete(pid, r_win(:owner), r_state(state, :wins))
        inform_all(wins2)
        {:noreply, r_state(state, wins: wins2)}
    end
  end

  def terminate(_Reason, state) do
    delete_all(r_state(state, :wins))
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp inform_all(wins) do
    infos =
      :lists.map(
        fn r_win(title: title, win: win) ->
          {title, win}
        end,
        wins
      )

    msg = {:dbg_ui_winman, :update_windows_menu, infos}

    :lists.foreach(
      fn r_win(owner: pid) ->
        send(pid, msg)
      end,
      wins
    )
  end

  defp delete_all(wins) do
    msg = {:dbg_ui_winman, :destroy}

    :lists.foreach(
      fn r_win(owner: pid) ->
        send(pid, msg)
      end,
      wins
    )
  end
end
