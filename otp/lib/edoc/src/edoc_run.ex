defmodule :m_edoc_run do
  use Bitwise
  import :edoc_report, only: [error: 1, report: 2]

  def application(args) do
    f = fn ->
      case parse_args(args) do
        [app] ->
          :edoc.application(app)

        [app, opts] ->
          :edoc.application(app, opts)

        [app, dir, opts] ->
          :edoc.application(app, dir, opts)

        _ ->
          invalid_args('edoc_run:application/1', args)
      end
    end

    run(f)
  end

  def files(args) do
    f = fn ->
      case parse_args(args) do
        [files] ->
          :edoc.files(files)

        [files, opts] ->
          :edoc.files(files, opts)

        _ ->
          invalid_args('edoc_run:files/1', args)
      end
    end

    run(f)
  end

  def toc(args) do
    f = fn ->
      case parse_args(args) do
        [dir, paths] ->
          :edoc.toc(dir, paths)

        [dir, paths, opts] ->
          :edoc.toc(dir, paths, opts)

        _ ->
          invalid_args('edoc_run:toc/1', args)
      end
    end

    run(f)
  end

  def file(args) do
    f = fn ->
      case parse_args(args) do
        [file] ->
          :edoc.file(file, [])

        [file, opts] ->
          :edoc.file(file, opts)

        _ ->
          invalid_args('edoc_run:file/1', args)
      end
    end

    run(f)
  end

  defp invalid_args(where, args) do
    report('invalid arguments to ~ts: ~tw.', [where, args])
    shutdown_error()
  end

  defp run(f) do
    wait_init()

    case (try do
            {:ok, f.()}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _} ->
        shutdown_ok()

      {:EXIT, e} ->
        report('edoc terminated abnormally: ~tP.', [e, 10])
        shutdown_error()

      thrown ->
        report('internal error: throw without catch in edoc: ~tP.', [thrown, 15])
        shutdown_error()
    end
  end

  defp wait_init() do
    case :erlang.whereis(:code_server) do
      :undefined ->
        :erlang.yield()
        wait_init()

      _ ->
        :ok
    end
  end

  defp shutdown_ok() do
    :init.stop()
  end

  defp shutdown_error() do
    receive do
    after
      1000 ->
        :ok
    end

    :erlang.halt(1)
  end

  defp parse_args([a | as]) when is_atom(a) do
    [parse_arg(:erlang.atom_to_list(a)) | parse_args(as)]
  end

  defp parse_args([a | as]) do
    [parse_arg(a) | parse_args(as)]
  end

  defp parse_args([]) do
    []
  end

  defp parse_arg(a) do
    case (try do
            {:ok, :edoc_lib.parse_expr(a, 1)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, expr} ->
        case (try do
                :erl_parse.normalise(expr)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            report('bad argument: \'~ts\':', [a])
            exit(:error)

          term ->
            term
        end

      {:error, _, d} ->
        report('error parsing argument \'~ts\'', [a])
        :erlang.error(d)
        exit(:error)
    end
  end
end
