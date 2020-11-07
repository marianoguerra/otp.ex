defmodule :m_log_mf_h do
  use Bitwise
  @behaviour :gen_event
  require Record

  Record.defrecord(:r_state, :state,
    dir: :undefined,
    maxB: :undefined,
    maxF: :undefined,
    curB: :undefined,
    curF: :undefined,
    cur_fd: :undefined,
    index: [],
    pred: :undefined
  )

  def init(dir, maxB, maxF) do
    init(dir, maxB, maxF, fn _ ->
      true
    end)
  end

  def init(dir, maxB, maxF, pred) do
    {dir, maxB, maxF, pred}
  end

  def init({dir, maxB, maxF, pred})
      when is_integer(maxF) and maxF > 0 and maxF < 256 do
    first =
      case read_index_file(dir) do
        {:ok, lastWritten} ->
          inc(lastWritten, maxF)

        _ ->
          1
      end

    case (try do
            file_open(dir, first)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, fd} ->
        {:ok,
         r_state(dir: dir, maxB: maxB, maxF: maxF, pred: pred, curF: first, cur_fd: fd, curB: 0)}

      error ->
        error
    end
  end

  def handle_event(event, state) do
    r_state(curB: curB, maxB: maxB, curF: curF, maxF: maxF, dir: dir, cur_fd: curFd, pred: pred) =
      state

    case (try do
            pred.(event)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      true ->
        bin = :erlang.term_to_binary(tag_event(event))
        size = byte_size(bin)

        newState =
          cond do
            curB + size < maxB ->
              state

            true ->
              :ok = :file.close(curFd)
              newF = inc(curF, maxF)
              {:ok, newFd} = file_open(dir, newF)
              r_state(state, cur_fd: newFd, curF: newF, curB: 0)
          end

        [hi, lo] = put_int16(size)

        case :file.write(
               r_state(newState, :cur_fd),
               [hi, lo, bin]
             ) do
          :ok ->
            :ok

          {:error, reason} ->
            exit({:file_exit, reason})
        end

        {:ok, r_state(newState, curB: r_state(newState, :curB) + size + 2)}

      _ ->
        {:ok, state}
    end
  end

  def handle_info({:emulator, gL, chars}, state) do
    handle_event({:emulator, gL, chars}, state)
  end

  def handle_info(_, state) do
    {:ok, state}
  end

  def terminate(_, state) do
    :ok = :file.close(r_state(state, :cur_fd))
    state
  end

  def handle_call(:null, state) do
    {:ok, :null, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp file_open(dir, fileNo) do
    case :file.open(
           dir ++
             [
               ?/
               | :erlang.integer_to_list(fileNo)
             ],
           [:raw, :write]
         ) do
      {:ok, fd} ->
        write_index_file(dir, fileNo)
        {:ok, fd}

      _ ->
        exit(:file_open)
    end
  end

  defp put_int16(i) do
    [i &&& 65280 >>> 8, i &&& 255]
  end

  defp tag_event(event) do
    {:erlang.localtime(), event}
  end

  defp read_index_file(dir) do
    case :file.open(dir ++ '/index', [:raw, :read]) do
      {:ok, fd} ->
        res =
          case (try do
                  :file.read(fd, 1)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:ok, [index]} ->
              {:ok, index}

            _ ->
              :error
          end

        :ok = :file.close(fd)
        res

      _ ->
        :error
    end
  end

  defp write_index_file(dir, index) do
    file = dir ++ '/index'
    tmpFile = file ++ '.tmp'

    case :file.open(tmpFile, [:raw, :write]) do
      {:ok, fd} ->
        :ok = :file.write(fd, [index])
        :ok = :file.close(fd)
        :ok = :file.rename(tmpFile, file)
        :ok

      _ ->
        exit(:write_index_file)
    end
  end

  defp inc(n, max) do
    cond do
      n < max ->
        n + 1

      true ->
        1
    end
  end
end
