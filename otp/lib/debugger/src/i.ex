defmodule :m_i do
  use Bitwise
  import :io, only: [format: 1, format: 2]
  import :lists, only: [foreach: 2, sort: 1]

  def iv() do
    vsn =
      :string.slice(
        :filename.basename(:code.lib_dir(:debugger)),
        9
      )

    :erlang.list_to_atom(vsn)
  end

  def im() do
    case :debugger.start() do
      {:ok, pid} ->
        pid

      {:error, {:already_started, pid}} ->
        pid
    end
  end

  def ii(module) do
    :int.i(module)
  end

  def ii(module, _Options) do
    :int.i(module)
  end

  def iq(module) do
    :int.n(module)
  end

  def ini(module) do
    :int.ni(module)
  end

  def ini(module, _Options) do
    :int.ni(module)
  end

  def inq(module) do
    :int.nn(module)
  end

  def ib(module, line) do
    :int.break(module, line)
  end

  def ib(module, function, arity) do
    :int.break_in(module, function, arity)
  end

  def ib(module, function, arity, cond__) do
    breaks1 = :int.all_breaks(module)
    :ok = :int.break_in(module, function, arity)
    breaks2 = :int.all_breaks(module)

    :lists.foreach(
      fn {mod, line} ->
        :int.test_at_break(mod, line, cond__)
      end,
      breaks2 -- breaks1
    )
  end

  def ibd(mod, line) do
    :int.disable_break(mod, line)
  end

  def ibe(mod, line) do
    :int.enable_break(mod, line)
  end

  def iba(mod, line, action) do
    :int.action_at_break(mod, line, action)
  end

  def ibc(mod, line, fnk) do
    :int.test_at_break(mod, line, fnk)
  end

  def ir(module, line) do
    :int.delete_break(module, line)
  end

  def ir(module, function, arity) do
    :int.del_break_in(module, function, arity)
  end

  def ir(module) do
    :int.no_break(module)
  end

  def ir() do
    :int.no_break()
  end

  def il() do
    mods = sort(:int.interpreted())
    ilformat('Module', 'File')

    foreach(
      fn mod ->
        ilformat(:erlang.atom_to_list(mod), get_file(mod))
      end,
      mods
    )
  end

  defp get_file(mod) do
    case :int.file(mod) do
      {:error, :not_loaded} ->
        'not loaded'

      file ->
        file
    end
  end

  defp ilformat(a1, a2) do
    format('~-20s     ~ts\n', [a1, a2])
  end

  def ipb() do
    bps = :lists.keysort(1, :int.all_breaks())
    bhformat('Module', 'Line', 'Status', 'Action', 'Condition')
    pb_print(bps)
  end

  def ipb(module) when is_atom(module) do
    ipb1(module)
  end

  def ipb(module) when is_list(module) do
    ipb1(:erlang.list_to_atom(module))
  end

  defp ipb1(module) do
    bps = :lists.keysort(1, :int.all_breaks(module))
    bhformat('Module', 'Line', 'Status', 'Action', 'Condition')
    pb_print(bps)
  end

  defp pb_print([
         {{mod, line}, [[status, action, _, :null] | _]}
         | bps
       ]) do
    bformat(mod, line, status, action, '')
    pb_print(bps)
  end

  defp pb_print([
         {{mod, line}, [[status, action, _, cond__] | _]}
         | bps
       ]) do
    bformat(mod, line, status, action, :io_lib.format('~w', [cond__]))
    pb_print(bps)
  end

  defp pb_print(_) do
    :ok
  end

  defp bhformat(a1, a2, a3, a4, a5) do
    format('~-15s ~-9s ~-12s ~-12s ~-21s~n', [a1, a2, a3, a4, a5])
  end

  defp bformat(a1, a2, a3, a4, a5) do
    format('~-15w ~-9w ~-12w ~-12w ~-21s~n', [a1, a2, a3, a4, a5])
  end

  def ist(flag) do
    :int.stack_trace(flag)
    true
  end

  def iaa(flag) do
    iaa(flag, {:dbg_wx_trace, :start, []})
  end

  def iaa(flag, fnk) do
    :int.auto_attach(flag, fnk)
    true
  end

  def ia(pid) do
    ia(pid, {:dbg_wx_trace, :start})
  end

  def ia(x, y, z) do
    ia(:c.pid(x, y, z))
  end

  def ia(pid, fnk) do
    case :lists.keymember(pid, 1, :int.snapshot()) do
      false ->
        :no_proc

      true ->
        :int.attach(pid, fnk)
    end
  end

  def ia(x, y, z, fnk) do
    ia(:c.pid(x, y, z), fnk)
  end

  def ip() do
    stats = :int.snapshot()
    hformat('Pid', 'Initial Call', 'Status', 'Info')
    ip(stats)
  end

  defp ip([{pid, {m, f, a}, status, {}} | stats]) do
    hformat(
      :io_lib.format('~w', [pid]),
      :io_lib.format('~w:~tw/~w', [m, f, length(a)]),
      :io_lib.format('~w', [status]),
      ''
    )

    ip(stats)
  end

  defp ip([{pid, {m, f, a}, status, info} | stats]) do
    hformat(
      :io_lib.format('~w', [pid]),
      :io_lib.format('~w:~tw/~w', [m, f, length(a)]),
      :io_lib.format('~w', [status]),
      :io_lib.format('~w', [info])
    )

    ip(stats)
  end

  defp ip([]) do
    :ok
  end

  defp hformat(a1, a2, a3, a4) do
    format('~-12s ~-21ts ~-9s ~-21s~n', [a1, a2, a3, a4])
  end

  def ic() do
    :int.clear()
  end

  def help() do
    format('iv()         -- print the current version of the interpreter~n')
    format('im()         -- pop up a monitor window~n')
    format('ii(Mod)      -- interpret Mod(s) (or AbsMod(s))~n')
    format('ii(Mod,Op)   -- interpret Mod(s) (or AbsMod(s))~n')
    format('                use Op as options (same as for compile)~n')
    format('iq(Mod)      -- do not interpret Mod(s)~n')
    format('ini(Mod)     -- ii/1 at all Erlang nodes~n')
    format('ini(Mod,Op)  -- ii/2 at all Erlang nodes~n')
    format('inq(Mod)     -- iq at all Erlang nodes~n')
    format('ib(Mod,Line) -- set a break point at Line in Mod~n')
    format('ib(M,F,Arity)-- set a break point in M:F/Arity~n')
    format('ibd(Mod,Line)-- disable the break point at Line in Mod~n')
    format('ibe(Mod,Line)-- enable the break point at Line in Mod~n')
    format('iba(M,L,Action)-- set a new action at break~n')
    format('ibc(M,L,Action)-- set a new condition for break~n')
    format('ir(Mod,Line) -- remove the break point at Line in Mod~n')
    format('ir(M,F,Arity)-- remove the break point in M:F/Arity~n')
    format('ir(Mod)      -- remove all break points in Mod~n')
    format('ir()         -- remove all existing break points~n')
    format('il()         -- list all interpreted modules~n')
    format('ip()         -- print status of all interpreted processes~n')
    format('ic()         -- remove all terminated interpreted processes~n')
    format('ipb()        -- list all break points~n')
    format('ipb(Mod)     -- list all break points in Mod~n')
    format('ia(Pid)      -- attach to Pid~n')
    format('ia(X,Y,Z)    -- attach to pid(X,Y,Z)~n')
    format('ia(Pid,Fun)  -- use own Fun = {M,F} as attach application~n')
    format('ia(X,Y,Z,Fun)-- use own Fun = {M,F} as attach application~n')
    format('iaa([Flag])  -- set automatic attach to process~n')
    format('                Flag is init,break and exit~n')
    format('iaa([Fl],Fun)-- use own Fun = {M,F} as attach application~n')
    format('ist(Flag)    -- set stack trace flag~n')
    format('                Flag is all (true),no_tail or false~n')
    :ok
  end
end
