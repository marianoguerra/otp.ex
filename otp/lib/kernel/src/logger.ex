defmodule :m_logger do
  use Bitwise

  def emergency(x) do
    log(:emergency, x)
  end

  def emergency(x, y) do
    log(:emergency, x, y)
  end

  def emergency(x, y, z) do
    log(:emergency, x, y, z)
  end

  def alert(x) do
    log(:alert, x)
  end

  def alert(x, y) do
    log(:alert, x, y)
  end

  def alert(x, y, z) do
    log(:alert, x, y, z)
  end

  def critical(x) do
    log(:critical, x)
  end

  def critical(x, y) do
    log(:critical, x, y)
  end

  def critical(x, y, z) do
    log(:critical, x, y, z)
  end

  def error(x) do
    log(:error, x)
  end

  def error(x, y) do
    log(:error, x, y)
  end

  def error(x, y, z) do
    log(:error, x, y, z)
  end

  def warning(x) do
    log(:warning, x)
  end

  def warning(x, y) do
    log(:warning, x, y)
  end

  def warning(x, y, z) do
    log(:warning, x, y, z)
  end

  def notice(x) do
    log(:notice, x)
  end

  def notice(x, y) do
    log(:notice, x, y)
  end

  def notice(x, y, z) do
    log(:notice, x, y, z)
  end

  def info(x) do
    log(:info, x)
  end

  def info(x, y) do
    log(:info, x, y)
  end

  def info(x, y, z) do
    log(:info, x, y, z)
  end

  def debug(x) do
    log(:debug, x)
  end

  def debug(x, y) do
    log(:debug, x, y)
  end

  def debug(x, y, z) do
    log(:debug, x, y, z)
  end

  def log(level, stringOrReport) do
    do_log(level, stringOrReport, %{})
  end

  def log(level, stringOrReport, metadata)
      when is_map(metadata) and
             not is_function(stringOrReport) do
    do_log(level, stringOrReport, metadata)
  end

  def log(level, funOrFormat, args) do
    do_log(level, {funOrFormat, args}, %{})
  end

  def log(level, funOrFormat, args, metadata) do
    do_log(level, {funOrFormat, args}, metadata)
  end

  def allow(level, module) when is_atom(module) do
    :logger_config.allow(level, module)
  end

  def macro_log(location, level, stringOrReport) do
    log_allowed(location, level, stringOrReport, %{})
  end

  def macro_log(location, level, stringOrReport, meta)
      when is_map(meta) and
             not is_function(stringOrReport) do
    log_allowed(location, level, stringOrReport, meta)
  end

  def macro_log(location, level, funOrFormat, args) do
    log_allowed(location, level, {funOrFormat, args}, %{})
  end

  def macro_log(location, level, funOrFormat, args, meta) do
    log_allowed(location, level, {funOrFormat, args}, meta)
  end

  def format_otp_report(%{:label => _, :report => report}) do
    format_report(report)
  end

  def format_otp_report(report) do
    format_report(report)
  end

  def format_report(report) when is_map(report) do
    format_report(:maps.to_list(report))
  end

  def format_report(report) when is_list(report) do
    case :lists.flatten(report) do
      [] ->
        {'~tp', [[]]}

      flatList ->
        case string_p1(flatList) do
          true ->
            {'~ts', [flatList]}

          false ->
            format_term_list(report, [], [])
        end
    end
  end

  def format_report(report) do
    {'~tp', [report]}
  end

  defp format_term_list([{tag, data} | t], format, args) do
    porS =
      case string_p(data) do
        true ->
          's'

        false ->
          'p'
      end

    format_term_list(t, ['    ~tp: ~t' ++ porS | format], [[data, tag] | args])
  end

  defp format_term_list([data | t], format, args) do
    format_term_list(t, ['    ~tp' | format], [data | args])
  end

  defp format_term_list([], format, args) do
    {:lists.flatten(
       :lists.join(
         ?\n,
         :lists.reverse(format)
       )
     ), :lists.reverse(args)}
  end

  defp string_p(list) when is_list(list) do
    string_p1(:lists.flatten(list))
  end

  defp string_p(_) do
    false
  end

  defp string_p1([]) do
    false
  end

  defp string_p1(flatList) do
    :io_lib.printable_unicode_list(flatList)
  end

  def internal_log(level, term) when is_atom(level) do
    :erlang.display_string('Logger - ' ++ :erlang.atom_to_list(level) ++ ': ')
    :erlang.display(term)
  end

  def timestamp() do
    :os.system_time(:microsecond)
  end

  def add_primary_filter(filterId, filter) do
    :logger_server.add_filter(:primary, {filterId, filter})
  end

  def add_handler_filter(handlerId, filterId, filter) do
    :logger_server.add_filter(handlerId, {filterId, filter})
  end

  def remove_primary_filter(filterId) do
    :logger_server.remove_filter(:primary, filterId)
  end

  def remove_handler_filter(handlerId, filterId) do
    :logger_server.remove_filter(handlerId, filterId)
  end

  def add_handler(handlerId, module, config) do
    :logger_server.add_handler(handlerId, module, config)
  end

  def remove_handler(handlerId) do
    :logger_server.remove_handler(handlerId)
  end

  def set_primary_config(key, value) do
    :logger_server.set_config(:primary, key, value)
  end

  def set_primary_config(config) do
    :logger_server.set_config(:primary, config)
  end

  def set_handler_config(handlerId, key, value) do
    :logger_server.set_config(handlerId, key, value)
  end

  def set_handler_config(handlerId, config) do
    :logger_server.set_config(handlerId, config)
  end

  def set_proxy_config(config) do
    :logger_server.set_config(:proxy, config)
  end

  def update_primary_config(config) do
    :logger_server.update_config(:primary, config)
  end

  def update_handler_config(handlerId, key, value) do
    :logger_server.update_config(handlerId, key, value)
  end

  def update_handler_config(handlerId, config) do
    :logger_server.update_config(handlerId, config)
  end

  def update_proxy_config(config) do
    :logger_server.update_config(:proxy, config)
  end

  def get_primary_config() do
    {:ok, config} = :logger_config.get(:logger, :primary)
    :maps.remove(:handlers, config)
  end

  def get_handler_config(handlerId) do
    case :logger_config.get(:logger, handlerId) do
      {:ok, %{:module => module} = config} ->
        {:ok,
         try do
           module.filter_config(config)
         catch
           _, _ ->
             config
         end}

      error ->
        error
    end
  end

  def get_handler_config() do
    for handlerId <- get_handler_ids() do
      {:ok, config} = get_handler_config(handlerId)
      config
    end
  end

  def get_handler_ids() do
    {:ok, %{:handlers => handlerIds}} = :logger_config.get(:logger, :primary)
    handlerIds
  end

  def get_proxy_config() do
    {:ok, config} = :logger_config.get(:logger, :proxy)
    config
  end

  def update_formatter_config(handlerId, formatterConfig) do
    :logger_server.update_formatter_config(
      handlerId,
      formatterConfig
    )
  end

  def update_formatter_config(handlerId, key, value) do
    :logger_server.update_formatter_config(
      handlerId,
      %{key => value}
    )
  end

  def set_module_level(module, level) when is_atom(module) do
    set_module_level([module], level)
  end

  def set_module_level(modules, level) do
    :logger_server.set_module_level(modules, level)
  end

  def unset_module_level(module) when is_atom(module) do
    unset_module_level([module])
  end

  def unset_module_level(modules) do
    :logger_server.unset_module_level(modules)
  end

  def unset_module_level() do
    :logger_server.unset_module_level()
  end

  def set_application_level(app, level) do
    case :application.get_key(app, :modules) do
      {:ok, modules} ->
        set_module_level(modules, level)

      :undefined ->
        {:error, {:not_loaded, app}}
    end
  end

  def unset_application_level(app) do
    case :application.get_key(app, :modules) do
      {:ok, modules} ->
        unset_module_level(modules)

      :undefined ->
        {:error, {:not_loaded, app}}
    end
  end

  def get_module_level(module) when is_atom(module) do
    get_module_level([module])
  end

  def get_module_level(modules) when is_list(modules) do
    for {m, l} <- get_module_level(),
        :lists.member(m, modules) do
      {m, l}
    end
  end

  def get_module_level() do
    :logger_config.get_module_level()
  end

  def compare_levels(level, level)
      when level === :emergency or level === :alert or level === :critical or level === :error or
             level === :warning or level === :notice or level === :info or level === :debug or
             level === :all or level === :none do
    :eq
  end

  def compare_levels(level1, level2)
      when level1 === :emergency or level1 === :alert or level1 === :critical or level1 === :error or
             level1 === :warning or level1 === :notice or level1 === :info or level1 === :debug or
             level1 === :all or
             (level1 === :none and
                (level2 === :emergency or level2 === :alert or level2 === :critical or
                   level2 === :error or level2 === :warning or level2 === :notice or
                   level2 === :info or level2 === :debug)) or level2 === :all or level2 === :none do
    int1 = :logger_config.level_to_int(level1)
    int2 = :logger_config.level_to_int(level2)

    cond do
      int1 < int2 ->
        :gt

      true ->
        :lt
    end
  end

  def compare_levels(level1, level2) do
    :erlang.error(:badarg, [level1, level2])
  end

  def set_process_metadata(meta) when is_map(meta) do
    _ = :erlang.put(:"$logger_metadata$", meta)
    :ok
  end

  def set_process_metadata(meta) do
    :erlang.error(:badarg, [meta])
  end

  def update_process_metadata(meta) when is_map(meta) do
    case get_process_metadata() do
      :undefined ->
        set_process_metadata(meta)

      meta0 when is_map(meta0) ->
        set_process_metadata(:maps.merge(meta0, meta))
        :ok
    end
  end

  def update_process_metadata(meta) do
    :erlang.error(:badarg, [meta])
  end

  def get_process_metadata() do
    :erlang.get(:"$logger_metadata$")
  end

  def unset_process_metadata() do
    _ = :erlang.erase(:"$logger_metadata$")
    :ok
  end

  def get_config() do
    %{
      :primary => get_primary_config(),
      :handlers => get_handler_config(),
      :proxy => get_proxy_config(),
      :module_levels => :lists.keysort(1, get_module_level())
    }
  end

  def i() do
    %{
      :primary => primary,
      :handlers => handlerConfigs,
      :proxy => proxy,
      :module_levels => modules
    } = get_config()

    m = modifier()
    i_primary(primary, m)
    i_handlers(handlerConfigs, m)
    i_proxy(proxy, m)
    i_modules(modules, m)
  end

  def i(:primary) do
    i_primary(get_primary_config(), modifier())
  end

  def i(:handlers) do
    i_handlers(get_handler_config(), modifier())
  end

  def i(:proxy) do
    i_proxy(get_proxy_config(), modifier())
  end

  def i(:modules) do
    i_modules(get_module_level(), modifier())
  end

  def i(handlerId) when is_atom(handlerId) do
    case get_handler_config(handlerId) do
      {:ok, handlerConfig} ->
        i_handlers([handlerConfig], modifier())

      error ->
        error
    end
  end

  def i(what) do
    :erlang.error(:badarg, [what])
  end

  defp i_primary(
         %{:level => level, :filters => filters, :filter_default => filterDefault},
         m
       ) do
    :io.format('Primary configuration: ~n', [])
    :io.format('    Level: ~p~n', [level])
    :io.format('    Filter Default: ~p~n', [filterDefault])
    :io.format('    Filters: ~n', [])
    print_filters('        ', filters, m)
  end

  defp i_handlers(handlerConfigs, m) do
    :io.format('Handler configuration: ~n', [])
    print_handlers(handlerConfigs, m)
  end

  defp i_proxy(proxy, m) do
    :io.format('Proxy configuration: ~n', [])
    print_custom('    ', proxy, m)
  end

  defp i_modules(modules, m) do
    :io.format('Level set per module: ~n', [])
    print_module_levels(modules, m)
  end

  defp encoding() do
    case :lists.keyfind(:encoding, 1, :io.getopts()) do
      false ->
        :latin1

      {:encoding, enc} ->
        enc
    end
  end

  defp modifier() do
    modifier(encoding())
  end

  defp modifier(:latin1) do
    ''
  end

  defp modifier(_) do
    't'
  end

  defp print_filters(indent, {id, {fun, arg}}, m) do
    :io.format(
      '~sId: ~' ++ m ++ 'p~n~s    Fun: ~' ++ m ++ 'p~n~s    Arg: ~' ++ m ++ 'p~n',
      [indent, id, indent, fun, indent, arg]
    )
  end

  defp print_filters(indent, [], _M) do
    :io.format('~s(none)~n', [indent])
  end

  defp print_filters(indent, filters, m) do
    for filter <- filters do
      print_filters(indent, filter, m)
    end

    :ok
  end

  defp print_handlers(
         %{
           :id => id,
           :module => module,
           :level => level,
           :filters => filters,
           :filter_default => filterDefault,
           :formatter => {formatterModule, formatterConfig}
         } = config,
         m
       ) do
    :io.format(
      '    Id: ~' ++
        m ++
        'p~n        Module: ~p~n        Level:  ~p~n        Formatter:~n            Module: ~p~n            Config:~n',
      [id, module, level, formatterModule]
    )

    print_custom('                ', formatterConfig, m)
    :io.format('        Filter Default: ~p~n        Filters:~n', [filterDefault])
    print_filters('            ', filters, m)

    case :maps.find(:config, config) do
      {:ok, handlerConfig} ->
        :io.format('        Handler Config:~n')
        print_custom('            ', handlerConfig, m)

      :error ->
        :ok
    end

    myKeys = [:filter_default, :filters, :formatter, :level, :module, :id, :config]

    case :maps.without(myKeys, config) do
      empty when empty == %{} ->
        :ok

      unhandled ->
        :io.format('        Custom Config:~n')
        print_custom('            ', unhandled, m)
    end
  end

  defp print_handlers([], _M) do
    :io.format('    (none)~n')
  end

  defp print_handlers(handlerConfigs, m) do
    for handlerConfig <- handlerConfigs do
      print_handlers(handlerConfig, m)
    end

    :ok
  end

  defp print_custom(indent, {key, value}, m) do
    :io.format('~s~' ++ m ++ 'p: ~' ++ m ++ 'p~n', [indent, key, value])
  end

  defp print_custom(indent, map, m) when is_map(map) do
    print_custom(indent, :lists.keysort(1, :maps.to_list(map)), m)
  end

  defp print_custom(indent, list, m)
       when is_list(list) and
              is_tuple(hd(list)) do
    for x <- list do
      print_custom(indent, x, m)
    end

    :ok
  end

  defp print_custom(indent, value, m) do
    :io.format('~s~' ++ m ++ 'p~n', [indent, value])
  end

  defp print_module_levels({module, level}, m) do
    :io.format('    Module: ~' ++ m ++ 'p~n        Level: ~p~n', [module, level])
  end

  defp print_module_levels([], _M) do
    :io.format('    (none)~n')
  end

  defp print_module_levels(modules, m) do
    for module <- modules do
      print_module_levels(module, m)
    end

    :ok
  end

  def internal_init_logger() do
    try do
      env = get_logger_env(:kernel)
      check_logger_config(:kernel, env)

      :ok =
        :logger.set_primary_config(
          :level,
          get_logger_level()
        )

      :ok =
        :logger.set_primary_config(
          :filter_default,
          get_primary_filter_default(env)
        )

      for {id, filter} <- get_primary_filters(env) do
        case :logger.add_primary_filter(id, filter) do
          :ok ->
            :ok

          {:error, reason} ->
            throw(reason)
        end
      end

      for {:module_level, level, modules} <- env do
        case :logger.set_module_level(modules, level) do
          :ok ->
            :ok

          {:error, reason} ->
            throw(reason)
        end
      end

      case :logger.set_handler_config(:simple, :filters, get_default_handler_filters()) do
        :ok ->
          :ok

        {:error, {:not_found, :simple}} ->
          :ok
      end

      init_kernel_handlers(env)
    catch
      reason ->
        case :logger.allow(:error, :logger) do
          true ->
            apply(:logger, :macro_log, [
              %{
                :mfa => {:logger, :internal_init_logger, 0},
                :line => 823,
                :file => 'otp/lib/kernel/src/logger.erl'
              },
              :error,
              'Invalid logger config: ~p',
              [reason]
            ])

          false ->
            :ok
        end

        {:error, {:bad_config, {:kernel, reason}}}
    end
  end

  defp init_kernel_handlers(env) do
    try do
      case get_logger_type(env) do
        {:ok, :silent} ->
          :ok = :logger.remove_handler(:simple)

        {:ok, false} ->
          :ok

        {:ok, type} ->
          init_default_config(type, env)
      end
    catch
      reason ->
        case :logger.allow(:error, :logger) do
          true ->
            apply(:logger, :macro_log, [
              %{
                :mfa => {:logger, :init_kernel_handlers, 1},
                :line => 841,
                :file => 'otp/lib/kernel/src/logger.erl'
              },
              :error,
              'Invalid default handler config: ~p',
              [reason]
            ])

          false ->
            :ok
        end

        {:error, {:bad_config, {:kernel, reason}}}
    end
  end

  def add_handlers(:kernel) do
    env = get_logger_env(:kernel)

    case get_proxy_opts(env) do
      :undefined ->
        add_handlers(:kernel, env)

      opts ->
        case set_proxy_config(opts) do
          :ok ->
            add_handlers(:kernel, env)

          {:error, reason} ->
            {:error, {:bad_proxy_config, reason}}
        end
    end
  end

  def add_handlers(app) when is_atom(app) do
    add_handlers(app, get_logger_env(app))
  end

  def add_handlers(handlerConfig) do
    add_handlers(
      :application.get_application(),
      handlerConfig
    )
  end

  defp add_handlers(app, handlerConfig) do
    try do
      check_logger_config(app, handlerConfig)

      defaultAdded =
        :lists.foldl(
          fn
            {:handler, :default = id, module, config}, _
            when not :erlang.is_map_key(
                   :filters,
                   config
                 ) ->
              defConfig = %{:filter_default => :stop, :filters => get_default_handler_filters()}

              setup_handler(
                id,
                module,
                :maps.merge(
                  defConfig,
                  config
                )
              )

              true

            {:handler, id, module, config}, default ->
              setup_handler(id, module, config)
              default or id == :default

            _, default ->
              default
          end,
          false,
          handlerConfig
        )

      for _ <- [:EFE_DUMMY_GEN], defaultAdded do
        case :logger.remove_handler(:simple) do
          :ok ->
            :ok

          {:error, {:not_found, :simple}} ->
            :ok
        end
      end

      :ok
    catch
      reason0 ->
        reason =
          case app do
            :undefined ->
              reason0

            _ ->
              {app, reason0}
          end

        case :logger.allow(:error, :logger) do
          true ->
            apply(:logger, :macro_log, [
              %{
                :mfa => {:logger, :add_handlers, 2},
                :line => 900,
                :file => 'otp/lib/kernel/src/logger.erl'
              },
              :error,
              'Invalid logger handler config: ~p',
              [reason]
            ])

          false ->
            :ok
        end

        {:error, {:bad_config, {:handler, reason}}}
    end
  end

  defp setup_handler(id, module, config) do
    case :logger.add_handler(id, module, config) do
      :ok ->
        :ok

      {:error, reason} ->
        throw(reason)
    end
  end

  defp check_logger_config(_, []) do
    :ok
  end

  defp check_logger_config(app, [{:handler, _, _, _} | env]) do
    check_logger_config(app, env)
  end

  defp check_logger_config(
         :kernel,
         [{:handler, :default, :undefined} | env]
       ) do
    check_logger_config(:kernel, env)
  end

  defp check_logger_config(:kernel, [{:filters, _, _} | env]) do
    check_logger_config(:kernel, env)
  end

  defp check_logger_config(:kernel, [{:module_level, _, _} | env]) do
    check_logger_config(:kernel, env)
  end

  defp check_logger_config(:kernel, [{:proxy, _} | env]) do
    check_logger_config(:kernel, env)
  end

  defp check_logger_config(_, bad) do
    throw(bad)
  end

  defp get_logger_type(env) do
    case :application.get_env(:kernel, :error_logger) do
      {:ok, :tty} ->
        {:ok, :standard_io}

      {:ok, {:file, file}} when is_list(file) ->
        {:ok, {:file, file}}

      {:ok, false} ->
        {:ok, false}

      {:ok, :silent} ->
        {:ok, :silent}

      :undefined ->
        case :lists.member(
               {:handler, :default, :undefined},
               env
             ) do
          true ->
            {:ok, false}

          false ->
            {:ok, :standard_io}
        end

      {:ok, bad} ->
        throw({:error_logger, bad})
    end
  end

  defp get_logger_level() do
    case :application.get_env(:kernel, :logger_level, :info) do
      level
      when level === :emergency or level === :alert or level === :critical or level === :error or
             level === :warning or level === :notice or level === :info or level === :debug or
             level === :all or level === :none ->
        level

      level ->
        throw({:logger_level, level})
    end
  end

  defp get_primary_filter_default(env) do
    case :lists.keyfind(:filters, 1, env) do
      {:filters, default, _} ->
        default

      false ->
        :log
    end
  end

  defp get_primary_filters(env) do
    case (for f = {:filters, _, _} <- env do
            f
          end) do
      [{:filters, _, filters}] ->
        case :lists.all(
               fn
                 {_, _} ->
                   true

                 _ ->
                   false
               end,
               filters
             ) do
          true ->
            filters

          false ->
            throw({:invalid_filters, filters})
        end

      [] ->
        []

      _ ->
        throw({:multiple_filters, env})
    end
  end

  defp get_proxy_opts(env) do
    case (for p = {:proxy, _} <- env do
            p
          end) do
      [{:proxy, opts}] ->
        opts

      [] ->
        :undefined

      _ ->
        throw({:multiple_proxies, env})
    end
  end

  defp init_default_config(type, env)
       when type == :standard_io or
              type == :standard_error or
              :erlang.element(1, type) == :file do
    defaultFormatter = %{
      :formatter => {:logger_formatter, %{:legacy_header => true, :single_line => false}}
    }

    defaultConfig = %{defaultFormatter | :config => %{:type => type}}

    newLoggerEnv =
      case :lists.keyfind(:default, 2, env) do
        {:handler, :default, :logger_std_h, config} ->
          :lists.keyreplace(
            :default,
            2,
            env,
            {:handler, :default, :logger_std_h,
             :maps.merge(
               defaultConfig,
               config
             )}
          )

        {:handler, :default, module, config} ->
          :lists.keyreplace(
            :default,
            2,
            env,
            {:handler, :default, module,
             :maps.merge(
               defaultFormatter,
               config
             )}
          )

        _ ->
          [
            {:handler, :default, :logger_std_h, defaultConfig}
            | env
          ]
      end

    :application.set_env(:kernel, :logger, newLoggerEnv, [{:timeout, :infinity}])
  end

  defp get_default_handler_filters() do
    case :application.get_env(:kernel, :logger_sasl_compatible, false) do
      true ->
        [
          {:remote_gl, {&:logger_filters.remote_gl/2, :stop}},
          {:domain, {&:logger_filters.domain/2, {:log, :super, [:otp]}}},
          {:no_domain, {&:logger_filters.domain/2, {:log, :undefined, []}}}
        ]

      false ->
        [
          {:remote_gl, {&:logger_filters.remote_gl/2, :stop}},
          {:domain, {&:logger_filters.domain/2, {:log, :super, [:otp, :sasl]}}},
          {:no_domain, {&:logger_filters.domain/2, {:log, :undefined, []}}}
        ]
    end
  end

  defp get_logger_env(app) do
    :application.get_env(app, :logger, [])
  end

  defp do_log(level, msg, %{:mfa => {module, _, _}} = meta) do
    case :logger_config.allow(level, module) do
      true ->
        log_allowed(%{}, level, msg, meta)

      false ->
        :ok
    end
  end

  defp do_log(level, msg, meta) do
    case :logger_config.allow(level) do
      true ->
        log_allowed(%{}, level, msg, meta)

      false ->
        :ok
    end
  end

  defp log_allowed(location, level, {fun, funArgs}, meta)
       when is_function(fun, 1) do
    try do
      fun.(funArgs)
    catch
      c, r ->
        log_allowed(
          location,
          level,
          {'LAZY_FUN CRASH: ~tp; Reason: ~tp', [{fun, funArgs}, {c, r}]},
          meta
        )
    else
      msg = {format, args}
      when is_list(format) or is_binary(format) or
             (is_atom(format) and
                is_list(args)) ->
        log_allowed(location, level, msg, meta)

      report
      when is_map(report) or (is_list(report) and is_tuple(hd(report))) ->
        log_allowed(location, level, report, meta)

      string when is_list(string) or is_binary(string) ->
        log_allowed(location, level, string, meta)

      other ->
        log_allowed(
          location,
          level,
          {'LAZY_FUN ERROR: ~tp; Returned: ~tp', [{fun, funArgs}, other]},
          meta
        )
    end
  end

  defp log_allowed(location, level, msg, meta0)
       when is_map(meta0) do
    meta =
      add_default_metadata(
        :maps.merge(
          location,
          :maps.merge(proc_meta(), meta0)
        )
      )

    case node(:maps.get(:gl, meta)) do
      node when node !== node() ->
        log_remote(node, level, msg, meta)

      _ ->
        :ok
    end

    do_log_allowed(level, msg, meta, tid())
  end

  defp do_log_allowed(level, {format, args} = msg, meta, tid)
       when level === :emergency or level === :alert or level === :critical or level === :error or
              level === :warning or level === :notice or level === :info or
              (level === :debug and
                 (is_list(format) or is_binary(format))) or
              (is_atom(format) and
                 is_list(args) and is_map(meta)) do
    :logger_backend.log_allowed(
      %{:level => level, :msg => msg, :meta => meta},
      tid
    )
  end

  defp do_log_allowed(level, report, meta, tid)
       when level === :emergency or level === :alert or level === :critical or level === :error or
              level === :warning or level === :notice or level === :info or
              (level === :debug and
                 is_map(report)) or
              (is_list(report) and is_tuple(hd(report)) and
                 is_map(meta)) do
    :logger_backend.log_allowed(
      %{:level => level, :msg => {:report, report}, :meta => meta},
      tid
    )
  end

  defp do_log_allowed(level, string, meta, tid)
       when level === :emergency or level === :alert or level === :critical or level === :error or
              level === :warning or level === :notice or level === :info or
              (level === :debug and
                 is_list(string)) or
              (is_binary(string) and
                 is_map(meta)) do
    :logger_backend.log_allowed(
      %{:level => level, :msg => {:string, string}, :meta => meta},
      tid
    )
  end

  defp tid() do
    :ets.whereis(:logger)
  end

  defp log_remote(node, level, {format, args}, meta) do
    log_remote(node, {:log, level, format, args, meta})
  end

  defp log_remote(node, level, msg, meta) do
    log_remote(node, {:log, level, msg, meta})
  end

  defp log_remote(node, request) do
    :logger_proxy.log({:remote, node, request})
    :ok
  end

  def add_default_metadata(meta) do
    add_default_metadata([:pid, :gl, :time], meta)
  end

  defp add_default_metadata([key | keys], meta) do
    case :maps.is_key(key, meta) do
      true ->
        add_default_metadata(keys, meta)

      false ->
        add_default_metadata(
          keys,
          %{meta | key => default(key)}
        )
    end
  end

  defp add_default_metadata([], meta) do
    meta
  end

  defp proc_meta() do
    case get_process_metadata() do
      procMeta when is_map(procMeta) ->
        procMeta

      _ ->
        %{}
    end
  end

  defp default(:pid) do
    self()
  end

  defp default(:gl) do
    :erlang.group_leader()
  end

  defp default(:time) do
    timestamp()
  end

  def filter_stacktrace(module, [{module, _, _, _} | _]) do
    []
  end

  def filter_stacktrace(module, [h | t]) do
    [h | filter_stacktrace(module, t)]
  end

  def filter_stacktrace(_, []) do
    []
  end
end
