defmodule :m_xmerl_eventp do
  use Bitwise
  @vsn :"0.19"
  @date :"03-09-17"
  require Record

  Record.defrecord(:r_xmlDecl, :xmlDecl,
    vsn: :undefined,
    encoding: :undefined,
    standalone: :undefined,
    attributes: :undefined
  )

  Record.defrecord(:r_xmlAttribute, :xmlAttribute,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: [],
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    normalized: :undefined
  )

  Record.defrecord(:r_xmlNamespace, :xmlNamespace,
    default: [],
    nodes: []
  )

  Record.defrecord(:r_xmlNsNode, :xmlNsNode,
    parents: [],
    pos: :undefined,
    prefix: :undefined,
    uri: []
  )

  Record.defrecord(:r_xmlElement, :xmlElement,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: :EFE_TODO_NESTED_RECORD,
    parents: [],
    pos: :undefined,
    attributes: [],
    content: [],
    language: '',
    xmlbase: '',
    elementdef: :undeclared
  )

  Record.defrecord(:r_xmlText, :xmlText,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    type: :text
  )

  Record.defrecord(:r_xmlComment, :xmlComment,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined
  )

  Record.defrecord(:r_xmlPI, :xmlPI,
    name: :undefined,
    parents: [],
    pos: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmlDocument, :xmlDocument, content: :undefined)

  Record.defrecord(:r_xmlContext, :xmlContext,
    axis_type: :forward,
    context_node: :undefined,
    context_position: 1,
    nodeset: [],
    bindings: [],
    functions: [],
    namespace: [],
    whole_document: :undefined
  )

  Record.defrecord(:r_xmlNode, :xmlNode, type: :element, node: :undefined, parents: [], pos: 1)

  Record.defrecord(:r_xmlObj, :xmlObj,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmerl_fun_states, :xmerl_fun_states,
    event: :undefined,
    hook: :undefined,
    rules: :undefined,
    fetch: :undefined,
    cont: :undefined
  )

  Record.defrecord(:r_xmerl_scanner, :xmerl_scanner,
    encoding: :undefined,
    standalone: :no,
    environment: :prolog,
    declarations: [],
    doctype_name: :undefined,
    doctype_DTD: :internal,
    comments: true,
    document: false,
    default_attrs: false,
    rules: :undefined,
    keep_rules: false,
    namespace_conformant: false,
    xmlbase: :undefined,
    xmlbase_cache: :undefined,
    fetch_path: [],
    filename: :file_name_unknown,
    validation: :off,
    schemaLocation: [],
    space: :preserve,
    event_fun: :undefined,
    hook_fun: :undefined,
    acc_fun: :undefined,
    fetch_fun: :undefined,
    close_fun: :undefined,
    continuation_fun: :undefined,
    rules_read_fun: :undefined,
    rules_write_fun: :undefined,
    rules_delete_fun: :undefined,
    user_state: :undefined,
    fun_states: :EFE_TODO_NESTED_RECORD,
    entity_references: [],
    text_decl: false,
    quiet: false,
    col: 1,
    line: 1,
    common_data: []
  )

  Record.defrecord(:r_xmerl_event, :xmerl_event,
    event: :undefined,
    line: :undefined,
    col: :undefined,
    pos: :undefined,
    data: :undefined
  )

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

  def stream(fname, options) do
    accF = fn x, acc, s ->
      acc(x, acc, s)
    end

    case :file.open(fname, [:read, :raw, :binary]) do
      {:ok, fd} ->
        b0 = :erlang.list_to_binary([])
        contS = [{b0, fname, fd}]

        opts =
          scanner_options(
            options,
            [
              {:continuation_fun, &cont/3, contS},
              {:acc_fun, accF},
              {:fetch_fun, &fetch/2},
              {:rules, &rules_read/3, &rules_write/4, ''},
              {:close_fun, &close/1}
            ]
          )

        :xmerl_scan.string([], opts)

      error ->
        error
    end
  end

  def stream_sax(fname, callBack, userState, options) do
    uS = {:xmerl.callbacks(callBack), userState}

    accF = fn x, acc, s ->
      acc(x, acc, s)
    end

    hookF = fn parsedEntity, s ->
      {cBs, arg} = :xmerl_scan.user_state(s)

      case parsedEntity do
        r_xmlComment() ->
          {[], s}

        _ ->
          case :xmerl.export_element(parsedEntity, cBs, arg) do
            {:error, reason} ->
              throw({:error, reason})

            resp ->
              {resp, :xmerl_scan.user_state({cBs, resp}, s)}
          end
      end
    end

    case :file.open(fname, [:read, :raw, :binary]) do
      {:ok, fd} ->
        b0 = :erlang.list_to_binary([])
        contS = [{b0, fname, fd}]

        opts =
          scanner_options(
            options,
            [
              {:acc_fun, accF},
              {:close_fun, &close/1},
              {:continuation_fun, &cont/3, contS},
              {:fetch_fun, &fetch/2},
              {:hook_fun, hookF},
              {:rules, &rules_read/3, &rules_write/4, ''},
              {:user_state, uS}
            ]
          )

        :xmerl_scan.string([], opts)

      error ->
        error
    end
  end

  def file_sax(fname, callBack, userState, options) do
    uS = {:xmerl.callbacks(callBack), userState}

    accF = fn x, acc, s ->
      {[x | acc], s}
    end

    hookF = fn parsedEntity, s ->
      {cBs, arg} = :xmerl_scan.user_state(s)

      case parsedEntity do
        r_xmlComment() ->
          {[], s}

        _ ->
          case :xmerl.export_element(parsedEntity, cBs, arg) do
            {:error, reason} ->
              throw({:error, reason})

            resp ->
              {resp, :xmerl_scan.user_state({cBs, resp}, s)}
          end
      end
    end

    opts =
      scanner_options(
        options,
        [{:acc_fun, accF}, {:hook_fun, hookF}, {:user_state, uS}]
      )

    :xmerl_scan.file(fname, opts)
  end

  def string_sax(string, callBack, userState, options) do
    uS = {:xmerl.callbacks(callBack), userState}

    accF = fn x, acc, s ->
      {[x | acc], s}
    end

    hookF = fn parsedEntity, s ->
      {cBs, arg} = :xmerl_scan.user_state(s)

      case parsedEntity do
        r_xmlComment() ->
          {[], s}

        _ ->
          case :xmerl.export_element(parsedEntity, cBs, arg) do
            {:error, reason} ->
              throw({:error, reason})

            resp ->
              {resp, :xmerl_scan.user_state({cBs, resp}, s)}
          end
      end
    end

    opts =
      scanner_options(
        options,
        [{:acc_fun, accF}, {:hook_fun, hookF}, {:user_state, uS}]
      )

    :xmerl_scan.string(string, opts)
  end

  defp cont(f, exception, s) do
    case :xmerl_scan.cont_state(s) do
      [{_Fname, :eof} | _] ->
        exception.(s)

      [{sofar, fname, fd} | t] ->
        cont2(f, exception, sofar, fd, fname, t, s)
    end
  end

  defp cont2(f, exception, sofar, fd, fname, t, s) do
    case (try do
            read_chunk(fd, fname, sofar)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, bin} ->
        find_good_split(:erlang.list_to_binary([sofar, bin]), f, exception, fd, fname, t, s)

      :eof ->
        :ok = :file.close(fd)
        newS = :xmerl_scan.cont_state([{fname, :eof} | t], s)
        f.(:erlang.binary_to_list(sofar), newS)

      error ->
        exit(error)
    end
  end

  defp read_chunk(fd, _Fname, _Sofar) do
    :file.read(fd, 8192)
  end

  defp find_good_split(bin, f, exception, fd, fname, t, s) do
    find_good_split(:erlang.size(bin) - 1, bin, f, exception, fd, fname, t, s)
  end

  defp find_good_split(0, b, f, exception, fd, fname, t, s) do
    cont2(f, exception, b, fd, fname, t, s)
  end

  defp find_good_split(size, b, f, exception, fd, fname, t, s) do
    case b do
      <<_Bytes::size(size)-binary, h::integer, tail::binary>>
      when h == 32 or h == 13 or h == 10 or h == 9 ->
        {subB, _} = :erlang.split_binary(b, size + 1)

        newS =
          :xmerl_scan.cont_state(
            [{tail, fname, fd} | t],
            s
          )

        f.(:erlang.binary_to_list(subB), newS)

      _ ->
        find_good_split(size - 1, b, f, exception, fd, fname, t, s)
    end
  end

  defp acc(x = r_xmlText(value: text), acc, s) do
    case detect_nul_text(text) do
      :ok ->
        {[r_xmlText(x, value: :lists.flatten(text)) | acc], s}

      :nok ->
        {acc, s}
    end
  end

  defp acc(x, acc, s) do
    {[x | acc], s}
  end

  defp detect_nul_text([h | t]) when h == 10 or h == 32 or h == 9 do
    detect_nul_text(t)
  end

  defp detect_nul_text([]) do
    :nok
  end

  defp detect_nul_text(_) do
    :ok
  end

  defp fetch({:system, uRI}, s) do
    fetch_URI(uRI, s)
  end

  defp fetch({:public, _PublicID, uRI}, s) do
    fetch_URI(uRI, s)
  end

  defp fetch_URI(uRI, s) do
    split = :filename.split(uRI)
    filename = :lists.last(split)

    fullname =
      case split do
        [['/', _] | _] ->
          uRI

        ['file:', name] ->
          :filename.join(r_xmerl_scanner(s, :xmlbase), name)

        _ ->
          :filename.join(r_xmerl_scanner(s, :xmlbase), uRI)
      end

    file = path_locate(r_xmerl_scanner(s, :fetch_path), filename, fullname)
    :no_debug

    case :file.open(file, [:read, :raw, :binary]) do
      {:ok, fd} ->
        contS = :xmerl_scan.cont_state(s)

        newS =
          :xmerl_scan.cont_state(
            [
              {:erlang.list_to_binary([]), file, fd}
              | contS
            ],
            s
          )

        {:ok, {:string, []}, newS}

      _Error ->
        :no_debug
        {:ok, :not_fetched, s}
    end
  end

  defp path_locate([dir | dirs], fN, fullName) do
    f = :filename.join(dir, fN)

    case :file.read_file_info(f) do
      {:ok, r_file_info(type: :regular)} ->
        f

      _ ->
        path_locate(dirs, fN, fullName)
    end
  end

  defp path_locate([], _FN, fullName) do
    fullName
  end

  defp close(s) do
    contS = :xmerl_scan.cont_state(s)

    case contS do
      [{_Fname, :eof} | t] ->
        :xmerl_scan.cont_state(t, s)

      [{_Sofar, _Fname, fd} | t] ->
        :ok = :file.close(fd)
        :xmerl_scan.cont_state(t, s)
    end
  end

  defp rules_write(context, name, value, r_xmerl_scanner(rules: :undefined) = s) do
    tab = :ets.new(:rules, [:set, :public])
    rules_write(context, name, value, r_xmerl_scanner(s, rules: tab))
  end

  defp rules_write(context, name, value, r_xmerl_scanner(rules: t) = s) do
    :ets.insert(t, {{context, name}, value})
    s
  end

  defp rules_read(_Context, _Name, r_xmerl_scanner(rules: :undefined)) do
    :undefined
  end

  defp rules_read(context, name, r_xmerl_scanner(rules: t)) do
    case :ets.lookup(t, {context, name}) do
      [] ->
        :undefined

      [{_K, v}] ->
        v
    end
  end

  defp scanner_options([h | t], opts) do
    case (try do
            keyreplace(h, 1, opts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      false ->
        scanner_options(t, [h | opts])

      newOpts ->
        scanner_options(t, newOpts)
    end
  end

  defp scanner_options([], opts) do
    opts
  end

  defp keyreplace(x, pos, [h | t])
       when :erlang.element(
              pos,
              x
            ) ==
              :erlang.element(
                pos,
                h
              ) do
    [x | t]
  end

  defp keyreplace(x, pos, [h | t]) do
    [h | keyreplace(x, pos, t)]
  end

  defp keyreplace(_, _Pos, []) do
    throw(false)
  end
end
