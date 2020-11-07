defmodule :m_edoc_refs do
  use Bitwise
  import Kernel, except: [to_string: 1]
  import :edoc_lib, only: [escape_uri: 1, join_uri: 2]
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

  def app(app) do
    {:app, app}
  end

  def app(app, ref) do
    {:app, app, ref}
  end

  def module(m) do
    {:module, m}
  end

  def module(m, ref) do
    {:module, m, ref}
  end

  def module(app, m, ref) do
    app(app, module(m, ref))
  end

  def function(f, a) do
    {:function, f, a}
  end

  def function(m, f, a) do
    module(m, function(f, a))
  end

  def function(app, m, f, a) do
    module(app, m, function(f, a))
  end

  def type(t) do
    {:type, t}
  end

  def type(m, t) do
    module(m, type(t))
  end

  def type(app, m, t) do
    module(app, m, type(t))
  end

  def to_string({:app, a}) do
    '//' ++ :erlang.atom_to_list(a)
  end

  def to_string({:app, a, ref}) do
    '//' ++ :erlang.atom_to_list(a) ++ '/' ++ to_string(ref)
  end

  def to_string({:module, m}) do
    :erlang.atom_to_list(m)
  end

  def to_string({:module, m, ref}) do
    :erlang.atom_to_list(m) ++ ':' ++ to_string(ref)
  end

  def to_string({:function, f, a}) do
    :erlang.atom_to_list(f) ++ '/' ++ :erlang.integer_to_list(a)
  end

  def to_string({:type, t}) do
    :erlang.atom_to_list(t) ++ '()'
  end

  def to_label({:function, f, a}) do
    escape_uri(:erlang.atom_to_list(f)) ++ '-' ++ :erlang.integer_to_list(a)
  end

  def to_label({:type, t}) do
    'type-' ++ escape_uri(:erlang.atom_to_list(t))
  end

  def get_uri({:app, app}, env) do
    join_uri(app_ref(app, env), 'index.html')
  end

  def get_uri({:app, app, ref}, env) do
    app_ref(app, ref, env)
  end

  def get_uri({:module, m, ref}, env) do
    module_ref(m, env) ++ '#' ++ to_label(ref)
  end

  def get_uri({:module, m}, env) do
    module_ref(m, env)
  end

  def get_uri(ref, _Env) do
    '#' ++ to_label(ref)
  end

  defp abs_uri({:module, m}, env) do
    module_absref(m, env)
  end

  defp abs_uri({:module, m, ref}, env) do
    module_absref(m, env) ++ '#' ++ to_label(ref)
  end

  defp module_ref(m, env) do
    case r_env(env, :modules).(m) do
      '' ->
        file = :erlang.atom_to_list(m) ++ r_env(env, :file_suffix)
        escape_uri(file)

      base ->
        join_uri(base, module_absref(m, env))
    end
  end

  defp module_absref(m, env) do
    escape_uri(:erlang.atom_to_list(m)) ++ escape_uri(r_env(env, :file_suffix))
  end

  defp app_ref(a, env) do
    case r_env(env, :apps).(a) do
      '' ->
        join_uri(
          r_env(env, :app_default),
          join_uri(escape_uri(:erlang.atom_to_list(a)), 'doc')
        )

      base ->
        base
    end
  end

  defp app_ref(a, ref, env) do
    join_uri(app_ref(a, env), abs_uri(ref, env))
  end

  def is_top({:app, _App}, _Env) do
    true
  end

  def is_top(_Ref, _Env) do
    false
  end
end
