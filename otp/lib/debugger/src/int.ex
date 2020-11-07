defmodule :m_int do
  use Bitwise
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def i(absMods) do
    i2(absMods, :local, :ok)
  end

  def i(absMods, _Options) do
    i2(absMods, :local, :ok)
  end

  def ni(absMods) do
    i2(absMods, :distributed, :ok)
  end

  def ni(absMods, _Options) do
    i2(absMods, :distributed, :ok)
  end

  defp i2([absMod | absMods], dist, acc)
       when is_atom(absMod) or is_list(absMod) or
              is_tuple(absMod) do
    res = int_mod(absMod, dist)

    case acc do
      :error ->
        i2(absMods, dist, acc)

      _ ->
        i2(absMods, dist, res)
    end
  end

  defp i2([], _Dist, acc) do
    acc
  end

  defp i2(absMod, dist, _Acc)
       when is_atom(absMod) or
              is_list(absMod) or is_tuple(absMod) do
    int_mod(absMod, dist)
  end

  def n(absMods) do
    n2(absMods, :local)
  end

  def nn(absMods) do
    n2(absMods, :distributed)
  end

  defp n2([absMod | absMods], dist)
       when is_atom(absMod) or is_list(absMod) do
    del_mod(absMod, dist)
    n2(absMods, dist)
  end

  defp n2([absMod], dist)
       when is_atom(absMod) or
              is_list(absMod) do
    del_mod(absMod, dist)
  end

  defp n2([], _Dist) do
    :ok
  end

  defp n2(absMod, dist)
       when is_atom(absMod) or
              is_list(absMod) do
    del_mod(absMod, dist)
  end

  def interpreted() do
    :dbg_iserver.safe_call(:all_interpreted)
  end

  def file(mod) when is_atom(mod) do
    :dbg_iserver.safe_call({:file, mod})
  end

  def interpretable(absMod) do
    case check(absMod) do
      {:ok, _Res} ->
        true

      error ->
        error
    end
  end

  def auto_attach() do
    :dbg_iserver.safe_call(:get_auto_attach)
  end

  def auto_attach(false) do
    :dbg_iserver.safe_cast({:set_auto_attach, false})
  end

  def auto_attach([], _Function) do
    auto_attach(false)
  end

  def auto_attach(flags, {mod, func}) do
    auto_attach(flags, {mod, func, []})
  end

  def auto_attach(flags, {mod, func, args})
      when is_atom(mod) and
             is_atom(func) and is_list(args) do
    check_flags(flags)
    :dbg_iserver.safe_cast({:set_auto_attach, flags, {mod, func, args}})
  end

  defp check_flags([:init | flags]) do
    check_flags(flags)
  end

  defp check_flags([:break | flags]) do
    check_flags(flags)
  end

  defp check_flags([:exit | flags]) do
    check_flags(flags)
  end

  defp check_flags([]) do
    true
  end

  def stack_trace() do
    :dbg_iserver.safe_call(:get_stack_trace)
  end

  def stack_trace(true) do
    stack_trace(:all)
  end

  def stack_trace(flag) do
    check_flag(flag)
    :dbg_iserver.safe_cast({:set_stack_trace, flag})
  end

  defp check_flag(:all) do
    true
  end

  defp check_flag(:no_tail) do
    true
  end

  defp check_flag(false) do
    true
  end

  def break(mod, line)
      when is_atom(mod) and
             is_integer(line) do
    :dbg_iserver.safe_call({:new_break, {mod, line}, [:active, :enable, :null, :null]})
  end

  def delete_break(mod, line)
      when is_atom(mod) and
             is_integer(line) do
    :dbg_iserver.safe_cast({:delete_break, {mod, line}})
  end

  def break_in(mod, func, arity)
      when is_atom(mod) and
             is_atom(func) and is_integer(arity) do
    case :dbg_iserver.safe_call({:is_interpreted, mod, func, arity}) do
      {true, clauses} ->
        lines = first_lines(clauses)

        :lists.foreach(
          fn line ->
            break(mod, line)
          end,
          lines
        )

      false ->
        {:error, :function_not_found}
    end
  end

  def del_break_in(mod, func, arity)
      when is_atom(mod) and
             is_atom(func) and is_integer(arity) do
    case :dbg_iserver.safe_call({:is_interpreted, mod, func, arity}) do
      {true, clauses} ->
        lines = first_lines(clauses)

        :lists.foreach(
          fn line ->
            delete_break(mod, line)
          end,
          lines
        )

      false ->
        {:error, :function_not_found}
    end
  end

  defp first_lines(clauses) do
    for clause <- clauses do
      first_line(clause)
    end
  end

  defp first_line({:clause, _L, _Vars, _, exprs}) do
    first_line(exprs)
  end

  defp first_line([expr | _Exprs]) do
    :erlang.element(2, expr)
  end

  def no_break() do
    :dbg_iserver.safe_cast(:no_break)
  end

  def no_break(mod) when is_atom(mod) do
    :dbg_iserver.safe_cast({:no_break, mod})
  end

  def disable_break(mod, line)
      when is_atom(mod) and
             is_integer(line) do
    :dbg_iserver.safe_cast({:break_option, {mod, line}, :status, :inactive})
  end

  def enable_break(mod, line)
      when is_atom(mod) and
             is_integer(line) do
    :dbg_iserver.safe_cast({:break_option, {mod, line}, :status, :active})
  end

  def action_at_break(mod, line, action)
      when is_atom(mod) and
             is_integer(line) do
    check_action(action)
    :dbg_iserver.safe_cast({:break_option, {mod, line}, :action, action})
  end

  defp check_action(:enable) do
    true
  end

  defp check_action(:disable) do
    true
  end

  defp check_action(:delete) do
    true
  end

  def test_at_break(mod, line, function)
      when is_atom(mod) and
             is_integer(line) do
    check_function(function)
    :dbg_iserver.safe_cast({:break_option, {mod, line}, :condition, function})
  end

  defp check_function({mod, func})
       when is_atom(mod) and
              is_atom(func) do
    true
  end

  def get_binding(var, bs) do
    :dbg_icmd.get_binding(var, bs)
  end

  def all_breaks() do
    :dbg_iserver.safe_call(:all_breaks)
  end

  def all_breaks(mod) when is_atom(mod) do
    :dbg_iserver.safe_call({:all_breaks, mod})
  end

  def snapshot() do
    :dbg_iserver.safe_call(:snapshot)
  end

  def clear() do
    :dbg_iserver.safe_cast(:clear)
  end

  def continue(pid) when is_pid(pid) do
    case :dbg_iserver.safe_call({:get_meta, pid}) do
      {:ok, meta} when is_pid(meta) ->
        :dbg_icmd.continue(meta)
        :ok

      error ->
        error
    end
  end

  def continue(x, y, z)
      when is_integer(x) and
             is_integer(y) and is_integer(z) do
    continue(:c.pid(x, y, z))
  end

  def start() do
    :dbg_iserver.start()
  end

  def stop() do
    :lists.foreach(
      fn mod ->
        _ =
          everywhere(
            :distributed,
            fn ->
              :erts_debug.breakpoint(
                {mod, :_, :_},
                false
              )
            end
          )
      end,
      interpreted()
    )

    :dbg_iserver.stop()
  end

  def subscribe() do
    :dbg_iserver.cast({:subscribe, self()})
  end

  def attach(pid, {mod, func}) do
    attach(pid, {mod, func, []})
  end

  def attach(pid, function) do
    :dbg_iserver.cast({:attach, pid, function})
  end

  def step(pid) do
    {:ok, meta} = :dbg_iserver.call({:get_meta, pid})
    :dbg_icmd.step(meta)
  end

  def next(pid) do
    {:ok, meta} = :dbg_iserver.call({:get_meta, pid})
    :dbg_icmd.next(meta)
  end

  def finish(pid) do
    {:ok, meta} = :dbg_iserver.call({:get_meta, pid})
    :dbg_icmd.finish(meta)
  end

  def attached(pid) do
    :dbg_iserver.call({:attached, self(), pid})
  end

  def meta(meta, :step) do
    :dbg_icmd.step(meta)
  end

  def meta(meta, :next) do
    :dbg_icmd.next(meta)
  end

  def meta(meta, :continue) do
    :dbg_icmd.continue(meta)
  end

  def meta(meta, :finish) do
    :dbg_icmd.finish(meta)
  end

  def meta(meta, :skip) do
    :dbg_icmd.skip(meta)
  end

  def meta(meta, :timeout) do
    :dbg_icmd.timeout(meta)
  end

  def meta(meta, :stop) do
    :dbg_icmd.stop(meta)
  end

  def meta(meta, :messages) do
    :dbg_icmd.get(meta, :messages, :null)
  end

  def meta(meta, :trace, trace) do
    :dbg_icmd.set(meta, :trace, trace)
  end

  def meta(meta, :stack_trace, flag) do
    :dbg_icmd.set(meta, :stack_trace, flag)
  end

  def meta(meta, :bindings, stack) do
    :dbg_icmd.get(meta, :bindings, stack)
  end

  def meta(meta, :stack_frame, arg) do
    :dbg_icmd.get(meta, :stack_frame, arg)
  end

  def meta(meta, :backtrace, n) do
    :dbg_icmd.get(meta, :backtrace, n)
  end

  def meta(meta, :eval, arg) do
    :dbg_icmd.eval(meta, arg)
  end

  def contents(mod, pid) do
    {:ok, bin} = :dbg_iserver.call({:contents, mod, pid})
    :erlang.binary_to_list(bin)
  end

  def functions(mod) do
    for f <- :dbg_iserver.call({:functions, mod}),
        functions_1(f) do
      f
    end
  end

  defp functions_1([:module_info, _Arity]) do
    false
  end

  defp functions_1(_Func) do
    true
  end

  def eval(mod, func, args) do
    :dbg_debugged.eval(mod, func, args)
  end

  defp int_mod({mod, src, beam, beamBin}, dist)
       when is_atom(mod) and is_list(src) and
              is_list(beam) and is_binary(beamBin) do
    try do
      case is_file(src) do
        true ->
          check_application(src)

          case check_beam(beamBin) do
            {:ok, exp, abst, _BeamBin} ->
              load({mod, src, beam, beamBin, exp, abst}, dist)

            :error ->
              :error
          end

        false ->
          :error
      end
    catch
      reason ->
        reason
    end
  end

  defp int_mod(absMod, dist)
       when is_atom(absMod) or
              is_list(absMod) do
    case check(absMod) do
      {:ok, res} ->
        load(res, dist)

      {:error, {:app, app}} ->
        :io.format('** Cannot interpret ~p module: ~p~n', [app, absMod])
        :error

      _Error ->
        :io.format('** Invalid beam file or no abstract code: ~tp\n', [absMod])
        :error
    end
  end

  defp check(mod) when is_atom(mod) do
    try do
      check_module(mod)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp check(file) when is_list(file) do
    try do
      check_file(file)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp load({mod, src, beam, beamBin, exp, abst}, dist) do
    _ =
      everywhere(
        dist,
        fn ->
          :code.purge(mod)
          :erts_debug.breakpoint({mod, :_, :_}, false)
          {:module, ^mod} = :code.load_binary(mod, beam, beamBin)
        end
      )

    case :erl_prim_loader.get_file(:filename.absname(src)) do
      {:ok, srcBin, _} ->
        mD5 = :code.module_md5(beamBin)

        srcBin1 =
          :unicode.characters_to_binary(
            srcBin,
            enc(srcBin)
          )

        true = is_binary(srcBin1)
        bin = :erlang.term_to_binary({:interpreter_module, exp, abst, srcBin1, mD5})
        {:module, ^mod} = :dbg_iserver.safe_call({:load, mod, src, bin})

        _ =
          everywhere(
            dist,
            fn ->
              true =
                :erts_debug.breakpoint(
                  {mod, :_, :_},
                  true
                ) > 0
            end
          )

        {:module, mod}

      :error ->
        :error
    end
  end

  defp check_module(mod) do
    case :code.which(mod) do
      beam when is_list(beam) ->
        case find_src(mod, beam) do
          src when is_list(src) ->
            check_application(src)

            case check_beam(beam) do
              {:ok, exp, abst, beamBin} ->
                {:ok, {mod, src, beam, beamBin, exp, abst}}

              :error ->
                {:error, :no_debug_info}
            end

          :error ->
            {:error, :no_src}
        end

      _ ->
        {:error, :badarg}
    end
  end

  defp check_file(name0) do
    src =
      case is_file(name0) do
        true ->
          name0

        false ->
          name = name0 ++ '.erl'

          case is_file(name) do
            true ->
              name

            false ->
              :error
          end
      end

    cond do
      is_list(src) ->
        check_application(src)
        mod = scan_module_name(src)

        case find_beam(mod, src) do
          beam when is_list(beam) ->
            case check_beam(beam) do
              {:ok, exp, abst, beamBin} ->
                {:ok, {mod, src, beam, beamBin, exp, abst}}

              :error ->
                {:error, :no_debug_info}
            end

          :error ->
            {:error, :no_beam}
        end

      true ->
        {:error, :badarg}
    end
  end

  defp check_application(src) do
    case :lists.reverse(:filename.split(:filename.absname(src))) do
      [[_Mod, 'src', appS] | _] ->
        check_application2(appS)

      _ ->
        :ok
    end
  end

  defp check_application2('kernel-' ++ _) do
    throw({:error, {:app, :kernel}})
  end

  defp check_application2('stdlib-' ++ _) do
    throw({:error, {:app, :stdlib}})
  end

  defp check_application2('erts-' ++ _) do
    throw({:error, {:app, :erts}})
  end

  defp check_application2('gs-' ++ _) do
    throw({:error, {:app, :gs}})
  end

  defp check_application2('debugger-' ++ _) do
    throw({:error, {:app, :debugger}})
  end

  defp check_application2(_) do
    :ok
  end

  defp find_src(mod, beam) do
    src0 = :filename.rootname(beam) ++ '.erl'

    case is_file(src0) do
      true ->
        src0

      false ->
        ebinDir = :filename.dirname(beam)
        src = :filename.join([:filename.dirname(ebinDir), 'src', :filename.basename(src0)])

        case is_file(src) do
          true ->
            src

          false ->
            find_src_from_module(mod)
        end
    end
  end

  defp find_src_from_module(mod) do
    compile = mod.module_info(:compile)

    case :lists.keyfind(:source, 1, compile) do
      {:source, src} ->
        case is_file(src) do
          true ->
            src

          false ->
            :error
        end

      false ->
        :error
    end
  end

  defp find_beam(mod, src) do
    srcDir = :filename.dirname(src)
    beamFile = :erlang.atom_to_list(mod) ++ :code.objfile_extension()
    file = :filename.join(srcDir, beamFile)

    case is_file(file) do
      true ->
        file

      false ->
        find_beam_1(beamFile, srcDir)
    end
  end

  defp find_beam_1(beamFile, srcDir) do
    rootDir = :filename.dirname(srcDir)
    ebinDir = :filename.join(rootDir, 'ebin')
    codePath = [ebinDir | :code.get_path()]

    :lists.foldl(
      fn
        _, beam when is_list(beam) ->
          beam

        dir, :error ->
          file = :filename.join(dir, beamFile)

          case is_file(file) do
            true ->
              file

            false ->
              :error
          end
      end,
      :error,
      codePath
    )
  end

  defp check_beam(beamBin) when is_binary(beamBin) do
    case :beam_lib.chunks(
           beamBin,
           [:abstract_code, :exports]
         ) do
      {:ok, {_Mod, [{:abstract_code, :no_abstract_code} | _]}} ->
        :error

      {:ok, {_Mod, [{:abstract_code, abst}, {:exports, exp}]}} ->
        {:ok, exp, abst, beamBin}

      _ ->
        :error
    end
  end

  defp check_beam(beam) when is_list(beam) do
    {:ok, bin, _FullPath} = :erl_prim_loader.get_file(:filename.absname(beam))
    check_beam(bin)
  end

  defp is_file(name) do
    :filelib.is_regular(
      :filename.absname(name),
      :erl_prim_loader
    )
  end

  defp everywhere(:distributed, fun) do
    case :erlang.is_alive() do
      true ->
        :rpc.multicall(:erlang, :apply, [fun, []])

      false ->
        fun.()
    end
  end

  defp everywhere(:local, fun) do
    fun.()
  end

  defp scan_module_name(file) do
    try do
      {:ok, bin, _FullPath} = :erl_prim_loader.get_file(:filename.absname(file))
      scan_module_name_1([], <<>>, bin, enc(bin))
    catch
      _, _ ->
        throw({:error, :no_beam})
    end
  end

  defp scan_module_name_1(cont0, b0, bin0, enc) do
    n = min(100, byte_size(bin0))
    {bin1, bin} = :erlang.split_binary(bin0, n)

    {chars, b1} =
      case :unicode.characters_to_list(
             :erlang.list_to_binary([b0, bin1]),
             enc
           ) do
        {:incomplete, list, binary} ->
          {list, binary}

        list when is_list(list) and list !== [] ->
          {list, <<>>}
      end

    scan_module_name_2(cont0, chars, b1, bin, enc)
  end

  defp scan_module_name_2(cont0, chars, b1, bin, enc) do
    case :erl_scan.tokens(cont0, chars, _AnyLine = 1) do
      {:done, {:ok, ts, _}, rest} ->
        scan_module_name_3(ts, rest, b1, bin, enc)

      {:more, cont} ->
        scan_module_name_1(cont, b1, bin, enc)
    end
  end

  defp scan_module_name_3(
         [
           [{:-, _}, {:atom, _, :module}, {:"(", _}]
           | _
         ] = ts,
         _Chars,
         _B1,
         _Bin,
         _Enc
       ) do
    scan_module_name_4(ts)
  end

  defp scan_module_name_3([[{:-, _}, {:atom, _, _}] | _], chars, b1, bin, enc) do
    scan_module_name_2('', chars, b1, bin, enc)
  end

  defp scan_module_name_4(ts) do
    {:ok, {:attribute, _, :module, m}} = :erl_parse.parse_form(ts)
    true = is_atom(m)
    m
  end

  defp enc(bin) do
    case :epp.read_encoding_from_binary(bin) do
      :none ->
        :epp.default_encoding()

      encoding ->
        encoding
    end
  end

  defp del_mod(absMod, dist) do
    mod =
      cond do
        is_atom(absMod) ->
          absMod

        is_list(absMod) ->
          :erlang.list_to_atom(:filename.basename(absMod, '.erl'))
      end

    :dbg_iserver.safe_cast({:delete, mod})

    _ =
      everywhere(
        dist,
        fn ->
          :erts_debug.breakpoint({mod, :_, :_}, false)
          :erlang.yield()
        end
      )

    :ok
  end
end
