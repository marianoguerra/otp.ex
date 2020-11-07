defmodule :m_error_handler do
  use Bitwise

  def undefined_function(module, func, args) do
    case ensure_loaded(module) do
      {:module, ^module} ->
        case :erlang.function_exported(module, func, length(args)) do
          true ->
            apply(module, func, args)

          false ->
            call_undefined_function_handler(module, func, args)
        end

      {:module, _} ->
        crash(module, func, args)

      _Other ->
        crash(module, func, args)
    end
  end

  def undefined_lambda(module, fun, args) do
    case ensure_loaded(module) do
      {:module, ^module} ->
        apply(fun, args)

      {:module, _} ->
        crash(fun, args)

      _Other ->
        crash(fun, args)
    end
  end

  def breakpoint(module, func, args) do
    int().eval(module, func, args)
  end

  def raise_undef_exception(module, func, args) do
    crash({module, func, args, []})
  end

  defp int() do
    :int
  end

  defp crash(fun, args) do
    crash({fun, args, []})
  end

  defp crash(m, f, a) do
    crash({m, f, a, []})
  end

  defp crash(tuple) do
    try do
      :erlang.error(:undef)
    catch
      :error, :undef ->
        stk = [tuple | tl(__STACKTRACE__)]
        :erlang.raise(:error, :undef, stk)
    end
  end

  defp ensure_loaded(module) do
    self = self()

    case :erlang.whereis(:code_server) do
      ^self ->
        error =
          'The code server called the unloaded module `' ++ :erlang.atom_to_list(module) ++ '\''

        :erlang.halt(error)

      pid when is_pid(pid) ->
        :code.ensure_loaded(module)

      _ ->
        :init.ensure_loaded(module)
    end
  end

  def stub_function(mod, func, args) do
    exit({:undef, [{mod, func, args, []}]})
  end

  defp call_undefined_function_handler(module, func, args) do
    handler = :"$handle_undefined_function"

    case :erlang.function_exported(module, handler, 2) do
      false ->
        crash(module, func, args)

      true ->
        apply(module, handler, [func, args])
    end
  end
end
