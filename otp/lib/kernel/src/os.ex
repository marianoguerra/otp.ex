defmodule :m_os do
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

  def env() do
    :erlang.nif_error(:undef)
  end

  def get_env_var(_VarName) do
    :erlang.nif_error(:undef)
  end

  def getpid() do
    :erlang.nif_error(:undef)
  end

  def perf_counter() do
    :erlang.nif_error(:undef)
  end

  def perf_counter(unit) do
    :erlang.convert_time_unit(:os.perf_counter(), :perf_counter, unit)
  end

  def set_env_var(_, _) do
    :erlang.nif_error(:undef)
  end

  def system_time() do
    :erlang.nif_error(:undef)
  end

  def system_time(_Unit) do
    :erlang.nif_error(:undef)
  end

  def timestamp() do
    :erlang.nif_error(:undef)
  end

  def unset_env_var(_) do
    :erlang.nif_error(:undef)
  end

  def set_signal(_Signal, _Option) do
    :erlang.nif_error(:undef)
  end

  def getenv() do
    for {key, value} <- :os.env() do
      :lists.flatten([key, ?=, value])
    end
  end

  def getenv(varName) do
    :os.get_env_var(varName)
  end

  def getenv(varName, defaultValue) do
    case :os.getenv(varName) do
      false ->
        defaultValue

      value ->
        value
    end
  end

  def putenv(varName, value) do
    :os.set_env_var(varName, value)
  end

  def unsetenv(varName) do
    :os.unset_env_var(varName)
  end

  def type() do
    :erlang.system_info(:os_type)
  end

  def version() do
    :erlang.system_info(:os_version)
  end

  def find_executable(name) do
    find_executable(name, :os.getenv('PATH', ''))
  end

  def find_executable(name, path) do
    extensions = extensions()

    case :filename.pathtype(name) do
      :relative ->
        find_executable1(name, split_path(path), extensions)

      _ ->
        case verify_executable(name, extensions, extensions) do
          {:ok, complete} ->
            complete

          :error ->
            false
        end
    end
  end

  defp find_executable1(name, [base | rest], extensions) do
    complete0 = :filename.join(base, name)

    case verify_executable(complete0, extensions, extensions) do
      {:ok, complete} ->
        complete

      :error ->
        find_executable1(name, rest, extensions)
    end
  end

  defp find_executable1(_Name, [], _Extensions) do
    false
  end

  defp verify_executable(name0, [ext | rest], origExtensions) do
    name1 = name0 ++ ext

    case :file.read_file_info(name1) do
      {:ok, r_file_info(type: :regular, mode: mode)}
      when mode &&& 73 !== 0 ->
        {:ok, name1}

      _ ->
        verify_executable(name0, rest, origExtensions)
    end
  end

  defp verify_executable(name, [], origExtensions)
       when origExtensions !== [''] do
    case can_be_full_name(
           :string.lowercase(name),
           origExtensions
         ) do
      true ->
        verify_executable(name, [''], [''])

      _ ->
        :error
    end
  end

  defp verify_executable(_, [], _) do
    :error
  end

  defp can_be_full_name(_Name, []) do
    false
  end

  defp can_be_full_name(name, [h | t]) do
    case :lists.suffix(h, name) do
      true ->
        true

      _ ->
        can_be_full_name(name, t)
    end
  end

  defp split_path(path) do
    case type() do
      {:win32, _} ->
        {:ok, curr} = :file.get_cwd()
        split_path(path, ?;, [], [curr])

      _ ->
        split_path(path, ?:, [], [])
    end
  end

  defp split_path([sep | rest], sep, current, path) do
    split_path(rest, sep, [], [reverse_element(current) | path])
  end

  defp split_path([c | rest], sep, current, path) do
    split_path(rest, sep, [c | current], path)
  end

  defp split_path([], _, current, path) do
    :lists.reverse(path, [reverse_element(current)])
  end

  defp reverse_element([]) do
    '.'
  end

  defp reverse_element([?" | t]) do
    case :lists.reverse(t) do
      [?" | list] ->
        list

      list ->
        list ++ [?"]
    end
  end

  defp reverse_element(list) do
    :lists.reverse(list)
  end

  defp extensions() do
    case type() do
      {:win32, _} ->
        ['.exe', '.com', '.cmd', '.bat']

      {:unix, _} ->
        ['']
    end
  end

  def cmd(cmd) do
    cmd(cmd, %{})
  end

  def cmd(cmd, opts) do
    {spawnCmd, spawnOpts, spawnInput, eot} = mk_cmd(:os.type(), validate(cmd))

    port =
      :erlang.open_port(
        {:spawn, spawnCmd},
        [
          :binary,
          :stderr_to_stdout,
          :stream,
          :in,
          :hide
          | spawnOpts
        ]
      )

    monRef = :erlang.monitor(:port, port)
    true = :erlang.port_command(port, spawnInput)
    bytes = get_data(port, monRef, eot, [], 0, :maps.get(:max_size, opts, :infinity))
    :erlang.demonitor(monRef, [:flush])
    string = :unicode.characters_to_list(bytes)

    cond do
      is_list(string) ->
        string

      true ->
        :erlang.binary_to_list(bytes)
    end
  end

  defp mk_cmd({:win32, wtype}, cmd) do
    command =
      case {:os.getenv('COMSPEC'), wtype} do
        {false, :windows} ->
          :lists.concat(['command.com /c', cmd])

        {false, _} ->
          :lists.concat(['cmd /c', cmd])

        {cspec, _} ->
          :lists.concat([cspec, ' /c', cmd])
      end

    {command, [], [], <<>>}
  end

  defp mk_cmd(_, cmd) do
    {'/bin/sh -s unix:cmd', [:out],
     ['(', :unicode.characters_to_binary(cmd), '\n) </dev/null; echo "\004"\n'], <<4>>}
  end

  defp validate(atom) when is_atom(atom) do
    validate(:erlang.atom_to_list(atom))
  end

  defp validate(list) when is_list(list) do
    case validate1(list) do
      false ->
        list

      true ->
        :string.trim(list, :trailing, [0])
    end
  end

  defp validate1([0 | rest]) do
    validate2(rest)
  end

  defp validate1([c | rest]) when is_integer(c) and c > 0 do
    validate1(rest)
  end

  defp validate1([list | rest]) when is_list(list) do
    :erlang.or(validate1(list), validate1(rest))
  end

  defp validate1([]) do
    false
  end

  defp validate2([]) do
    true
  end

  defp validate2([0 | rest]) do
    validate2(rest)
  end

  defp validate2([list | rest]) when is_list(list) do
    validate2(list)
    validate2(rest)
  end

  defp get_data(port, monRef, eot, sofar, size, max) do
    receive do
      {^port, {:data, bytes}} ->
        case eot(bytes, eot, size, max) do
          :more ->
            get_data(port, monRef, eot, [sofar, bytes], size + byte_size(bytes), max)

          last ->
            try do
              :erlang.port_close(port)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

            flush_until_down(port, monRef)
            :erlang.iolist_to_binary([sofar, last])
        end

      {:DOWN, ^monRef, _, _, _} ->
        flush_exit(port)
        :erlang.iolist_to_binary(sofar)
    end
  end

  defp eot(bs, <<>>, size, max)
       when size + byte_size(bs) < max do
    :more
  end

  defp eot(bs, <<>>, size, max) do
    :binary.part(bs, {0, max - size})
  end

  defp eot(bs, eot, size, max) do
    case :binary.match(bs, eot) do
      {pos, _} when size + pos < max ->
        :binary.part(bs, {0, pos})

      _ ->
        eot(bs, <<>>, size, max)
    end
  end

  defp flush_until_down(port, monRef) do
    receive do
      {^port, {:data, _Bytes}} ->
        flush_until_down(port, monRef)

      {:DOWN, ^monRef, _, _, _} ->
        flush_exit(port)
    end
  end

  defp flush_exit(port) do
    receive do
      {:EXIT, ^port, _} ->
        :ok
    after
      0 ->
        :ok
    end
  end
end
