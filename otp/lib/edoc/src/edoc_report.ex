defmodule :m_edoc_report do
  use Bitwise
  require Record
  Record.defrecord(:r_context, :context, dir: '', env: :undefined, opts: [])
  Record.defrecord(:r_doclet_gen, :doclet_gen, sources: [], app: [], modules: [])

  Record.defrecord(:r_doclet_toc, :doclet_toc,
    paths: :undefined,
    indir: :undefined
  )

  Record.defrecord(:r_module, :module,
    name: [],
    parameters: :none,
    functions: [],
    exports: [],
    attributes: [],
    records: [],
    encoding: :latin1
  )

  Record.defrecord(:r_env, :env,
    module: [],
    root: '',
    file_suffix: :undefined,
    apps: :undefined,
    modules: :undefined,
    app_default: :undefined,
    macros: [],
    includes: []
  )

  Record.defrecord(:r_comment, :comment,
    line: 0,
    text: :undefined
  )

  Record.defrecord(:r_entry, :entry,
    name: :undefined,
    args: [],
    line: 0,
    export: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tag, :tag, name: :undefined, line: 0, origin: :comment, data: :undefined)

  def error(what) do
    :erlang.error([], what)
  end

  def error(where, what) do
    error(0, where, what)
  end

  def error(line, where, s) when is_list(s) do
    report(line, where, s, [])
  end

  def error(line, where, {s, d}) when is_list(s) do
    report(line, where, s, d)
  end

  def error(line, where, {:format_error, m, d}) do
    report(line, where, m.format_error(d), [])
  end

  def warning(s) do
    warning(s, [])
  end

  def warning(s, vs) do
    warning([], s, vs)
  end

  def warning(where, s, vs) do
    warning(0, where, s, vs)
  end

  def warning(l, where, s, vs) do
    report(l, where, 'warning: ' ++ s, vs)
  end

  def report(s, vs) do
    report([], s, vs)
  end

  def report(where, s, vs) do
    report(0, where, s, vs)
  end

  def report(l, where, s, vs) do
    :io.put_chars(where(where))

    cond do
      is_integer(l) and l > 0 ->
        :io.fwrite('at line ~w: ', [l])

      true ->
        :ok
    end

    :io.fwrite(s, vs)
    :io.nl()
  end

  defp where({file, :module}) do
    :io_lib.fwrite('~ts, in module header: ', [file])
  end

  defp where({file, :footer}) do
    :io_lib.fwrite('~ts, in module footer: ', [file])
  end

  defp where({file, :header}) do
    :io_lib.fwrite('~ts, in header file: ', [file])
  end

  defp where({file, {f, a}}) do
    :io_lib.fwrite('~ts, function ~ts/~w: ', [file, f, a])
  end

  defp where([]) do
    :io_lib.fwrite('~s: ', [:edoc])
  end

  defp where(file) when is_list(file) do
    file ++ ': '
  end
end
