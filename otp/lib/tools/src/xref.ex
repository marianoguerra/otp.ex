defmodule :m_xref do
  use Bitwise
  import :lists, only: [keydelete: 3, keysearch: 3]
  import :sofs, only: [is_sofs_set: 1, to_external: 1]
  @behaviour :gen_server
  def m(module) when is_atom(module) do
    case :xref_utils.find_beam(module) do
      {:ok, file} ->
        fun = fn s ->
          :xref_base.add_module(s, file, {:builtins, true})
        end

        case (try do
                do_functions_analysis(fun)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, _, {:no_debug_info, _}} ->
            try do
              do_modules_analysis(fun)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

          result ->
            result
        end

      error ->
        error
    end
  end

  def m(file) do
    case :xref_utils.split_filename(file, '.beam') do
      false ->
        {:error, :xref_base, {:invalid_filename, file}}

      {dir, baseName} ->
        beamFile = :filename.join(dir, baseName)

        fun = fn s ->
          :xref_base.add_module(s, beamFile, {:builtins, true})
        end

        case (try do
                do_functions_analysis(fun)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, _, {:no_debug_info, _}} ->
            try do
              do_modules_analysis(fun)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

          result ->
            result
        end
    end
  end

  def d(directory) do
    fun = fn s ->
      :xref_base.add_directory(s, directory, {:builtins, true})
    end

    fun1 = fn s ->
      case fun.(s) do
        {:ok, [], _S} ->
          :no_modules

        reply ->
          reply
      end
    end

    case (try do
            do_functions_analysis(fun1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :no_modules ->
        try do
          do_modules_analysis(fun)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      result ->
        result
    end
  end

  def start(name) when is_atom(name) do
    start(name, [])
  end

  def start(opts0) when is_list(opts0) do
    {args, opts} = split_args(opts0)
    :gen_server.start(:xref, args, opts)
  end

  def start(name, opts0) when is_list(opts0) do
    {args, opts} = split_args(opts0)
    :gen_server.start({:local, name}, :xref, args, opts)
  end

  def start(name, opt) do
    start(name, [opt])
  end

  defp split_args(opts) do
    case keysearch(:xref_mode, 1, opts) do
      {:value, mode} ->
        {[mode], keydelete(:xref_mode, 1, opts)}

      false ->
        {[], opts}
    end
  end

  def stop(name) do
    try do
      :gen_server.call(name, :stop, :infinity)
    after
      try do
        :erlang.unregister(name)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end
    end
  end

  def add_release(name, dir) do
    :gen_server.call(name, {:add_release, dir}, :infinity)
  end

  def add_release(name, dir, options) do
    :gen_server.call(name, {:add_release, dir, options}, :infinity)
  end

  def add_application(name, dir) do
    :gen_server.call(name, {:add_application, dir}, :infinity)
  end

  def add_application(name, dir, options) do
    :gen_server.call(name, {:add_application, dir, options}, :infinity)
  end

  def add_module(name, file) do
    :gen_server.call(name, {:add_module, file}, :infinity)
  end

  def add_module(name, file, options) do
    :gen_server.call(name, {:add_module, file, options}, :infinity)
  end

  def add_directory(name, dir) do
    :gen_server.call(name, {:add_directory, dir}, :infinity)
  end

  def add_directory(name, dir, options) do
    :gen_server.call(name, {:add_directory, dir, options}, :infinity)
  end

  def replace_module(name, module, file) do
    :gen_server.call(name, {:replace_module, module, file}, :infinity)
  end

  def replace_module(name, module, file, options) do
    :gen_server.call(name, {:replace_module, module, file, options}, :infinity)
  end

  def replace_application(name, app, dir) do
    :gen_server.call(name, {:replace_application, app, dir}, :infinity)
  end

  def replace_application(name, app, dir, options) do
    :gen_server.call(name, {:replace_application, app, dir, options}, :infinity)
  end

  def remove_module(name, mod) do
    :gen_server.call(name, {:remove_module, mod}, :infinity)
  end

  def remove_application(name, app) do
    :gen_server.call(name, {:remove_application, app}, :infinity)
  end

  def remove_release(name, rel) do
    :gen_server.call(name, {:remove_release, rel}, :infinity)
  end

  def get_library_path(name) do
    :gen_server.call(name, :get_library_path, :infinity)
  end

  def set_library_path(name, path) do
    :gen_server.call(name, {:set_library_path, path}, :infinity)
  end

  def set_library_path(name, path, options) do
    :gen_server.call(name, {:set_library_path, path, options}, :infinity)
  end

  def info(name) do
    :gen_server.call(name, :info, :infinity)
  end

  def info(name, what) do
    :gen_server.call(name, {:info, what}, :infinity)
  end

  def info(name, what, qual) do
    :gen_server.call(name, {:info, what, qual}, :infinity)
  end

  def update(name) do
    :gen_server.call(name, :update, :infinity)
  end

  def update(name, options) do
    :gen_server.call(name, {:update, options}, :infinity)
  end

  def forget(name) do
    :gen_server.call(name, :forget, :infinity)
  end

  def forget(name, variable) do
    :gen_server.call(name, {:forget, variable}, :infinity)
  end

  def variables(name) do
    :gen_server.call(name, :variables, :infinity)
  end

  def variables(name, options) do
    :gen_server.call(name, {:variables, options}, :infinity)
  end

  def analyse(name, what) do
    :gen_server.call(name, {:analyze, what}, :infinity)
  end

  def analyse(name, what, options) do
    :gen_server.call(name, {:analyze, what, options}, :infinity)
  end

  def analyze(name, what) do
    :gen_server.call(name, {:analyze, what}, :infinity)
  end

  def analyze(name, what, options) do
    :gen_server.call(name, {:analyze, what, options}, :infinity)
  end

  def q(name, q) do
    :gen_server.call(name, {:qry, q}, :infinity)
  end

  def q(name, q, options) do
    :gen_server.call(name, {:qry, q, options}, :infinity)
  end

  def get_default(name) do
    :gen_server.call(name, :get_default, :infinity)
  end

  def get_default(name, option) do
    :gen_server.call(name, {:get_default, option}, :infinity)
  end

  def set_default(name, optionValues) do
    :gen_server.call(name, {:set_default, optionValues}, :infinity)
  end

  def set_default(name, option, value) do
    :gen_server.call(name, {:set_default, option, value}, :infinity)
  end

  def format_error({:error, module, error}) do
    module.format_error(error)
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  def init(args) do
    case :xref_base.new(args) do
      {:ok, s} ->
        {:ok, s}

      {:error, _Module, reason} ->
        {:stop, reason}
    end
  end

  def handle_call(:stop, _From, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_call({:add_release, dir}, _From, state) do
    case :xref_base.add_release(state, dir) do
      {:ok, releaseName, newState} ->
        {:reply, {:ok, releaseName}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_release, dir, options}, _From, state) do
    case :xref_base.add_release(state, dir, options) do
      {:ok, releaseName, newState} ->
        {:reply, {:ok, releaseName}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_application, dir}, _From, state) do
    case :xref_base.add_application(state, dir) do
      {:ok, appName, newState} ->
        {:reply, {:ok, appName}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_application, dir, options}, _From, state) do
    case :xref_base.add_application(state, dir, options) do
      {:ok, appName, newState} ->
        {:reply, {:ok, appName}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_module, file}, _From, state) do
    case :xref_base.add_module(state, file) do
      {:ok, module, newState} ->
        {:reply, {:ok, module}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_module, file, options}, _From, state) do
    case :xref_base.add_module(state, file, options) do
      {:ok, module, newState} ->
        {:reply, {:ok, module}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:replace_application, appl, dir}, _From, state) do
    case :xref_base.replace_application(state, appl, dir) do
      {:ok, appName, newState} ->
        {:reply, {:ok, appName}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:replace_application, appl, dir, opts}, _From, state) do
    case :xref_base.replace_application(state, appl, dir, opts) do
      {:ok, appName, newState} ->
        {:reply, {:ok, appName}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:remove_module, mod}, _From, state) do
    case :xref_base.remove_module(state, mod) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:remove_application, appl}, _From, state) do
    case :xref_base.remove_application(state, appl) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:remove_release, rel}, _From, state) do
    case :xref_base.remove_release(state, rel) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_directory, dir}, _From, state) do
    case :xref_base.add_directory(state, dir) do
      {:ok, modules, newState} ->
        {:reply, {:ok, modules}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_directory, dir, options}, _From, state) do
    case :xref_base.add_directory(state, dir, options) do
      {:ok, modules, newState} ->
        {:reply, {:ok, modules}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call(:get_library_path, _From, state) do
    path = :xref_base.get_library_path(state)
    {:reply, path, state}
  end

  def handle_call({:set_library_path, path}, _From, state) do
    case :xref_base.set_library_path(state, path) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:set_library_path, path, options}, _From, state) do
    case :xref_base.set_library_path(state, path, options) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:replace_module, module, file}, _From, state) do
    case :xref_base.replace_module(state, module, file) do
      {:ok, ^module, newState} ->
        {:reply, {:ok, module}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:replace_module, module, file, options}, _From, state) do
    case :xref_base.replace_module(state, module, file, options) do
      {:ok, ^module, newState} ->
        {:reply, {:ok, module}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call(:info, _From, state) do
    {:reply, :xref_base.info(state), state}
  end

  def handle_call({:info, what}, _From, state) do
    {:reply, :xref_base.info(state, what), state}
  end

  def handle_call({:info, what, qual}, _From, state) do
    {:reply, :xref_base.info(state, what, qual), state}
  end

  def handle_call(:update, _From, state) do
    case :xref_base.update(state) do
      {:ok, newState, modules} ->
        {:reply, {:ok, modules}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:update, options}, _From, state) do
    case :xref_base.update(state, options) do
      {:ok, newState, modules} ->
        {:reply, {:ok, modules}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call(:forget, _From, state) do
    {:ok, newState} = :xref_base.forget(state)
    {:reply, :ok, newState}
  end

  def handle_call({:forget, variable}, _From, state) do
    case :xref_base.forget(state, variable) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call(:variables, _From, state) do
    {reply, newState} = :xref_base.variables(state)
    {:reply, reply, newState}
  end

  def handle_call({:variables, options}, _From, state) do
    {reply, newState} = :xref_base.variables(state, options)
    {:reply, reply, newState}
  end

  def handle_call({:analyze, what}, _From, state) do
    {reply, newState} = :xref_base.analyze(state, what)
    {:reply, unsetify(reply), newState}
  end

  def handle_call({:analyze, what, options}, _From, state) do
    {reply, newState} = :xref_base.analyze(state, what, options)
    {:reply, unsetify(reply), newState}
  end

  def handle_call({:qry, q}, _From, state) do
    {reply, newState} = :xref_base.q(state, q)
    {:reply, unsetify(reply), newState}
  end

  def handle_call({:qry, q, options}, _From, state) do
    {reply, newState} = :xref_base.q(state, q, options)
    {:reply, unsetify(reply), newState}
  end

  def handle_call(:get_default, _From, state) do
    reply = :xref_base.get_default(state)
    {:reply, reply, state}
  end

  def handle_call({:get_default, option}, _From, state) do
    reply = :xref_base.get_default(state, option)
    {:reply, reply, state}
  end

  def handle_call({:set_default, optionValues}, _From, state) do
    case :xref_base.set_default(state, optionValues) do
      {:ok, newState} ->
        {:reply, :ok, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:set_default, option, value}, _From, state) do
    case :xref_base.set_default(state, option, value) do
      {:ok, oldValue, newState} ->
        {:reply, {:ok, oldValue}, newState}

      error ->
        {:reply, error, state}
    end
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp do_functions_analysis(fFun) do
    {:ok, state} = :xref_base.new()

    {:ok, state1} =
      :xref_base.set_library_path(
        state,
        :code_path
      )

    {:ok, state2} =
      :xref_base.set_default(
        state1,
        [{:verbose, false}, {:warnings, false}]
      )

    state3 =
      case fFun.(state2) do
        {:ok, _, s} ->
          s

        error2 ->
          throw(error2)
      end

    {undef, state4} =
      do_analysis(
        state3,
        :undefined_function_calls
      )

    {unused, state5} = do_analysis(state4, :locals_not_used)

    {deprecated, _} =
      do_analysis(
        state5,
        :deprecated_function_calls
      )

    [
      {:deprecated, to_external(deprecated)},
      {:undefined, to_external(undef)},
      {:unused, to_external(unused)}
    ]
  end

  defp do_modules_analysis(fFun) do
    {:ok, state} = :xref_base.new({:xref_mode, :modules})

    {:ok, state1} =
      :xref_base.set_library_path(
        state,
        :code_path
      )

    {:ok, state2} =
      :xref_base.set_default(
        state1,
        [{:verbose, false}, {:warnings, false}]
      )

    state3 =
      case fFun.(state2) do
        {:ok, _, s} ->
          s

        error2 ->
          throw(error2)
      end

    {undef, state4} =
      do_analysis(
        state3,
        :undefined_functions
      )

    {deprecated, _} =
      do_analysis(
        state4,
        :deprecated_functions
      )

    [{:deprecated, to_external(deprecated)}, {:undefined, to_external(undef)}]
  end

  defp do_analysis(state, analysis) do
    case :xref_base.analyze(state, analysis) do
      {{:ok, reply}, newState} ->
        {reply, newState}

      {error, _} ->
        throw(error)
    end
  end

  defp unsetify(reply = {:ok, x}) do
    case is_sofs_set(x) do
      true ->
        {:ok, to_external(x)}

      false ->
        reply
    end
  end

  defp unsetify(reply) do
    reply
  end
end
