defmodule :m_tags do
  use Bitwise

  def root() do
    root([])
  end

  def root(options) do
    subdir(:code.root_dir(), options)
  end

  def dir(dir) do
    dir(dir, [])
  end

  def dir(dir, options) do
    dirs([dir], options)
  end

  def dirs(dirs) do
    dirs(dirs, [])
  end

  def dirs(dirs, options) do
    files(collect_dirs(dirs, false), options)
  end

  def subdir(dir) do
    subdir(dir, [])
  end

  def subdir(dir, options) do
    subdirs([dir], options)
  end

  def subdirs(dirs) do
    subdirs(dirs, [])
  end

  def subdirs(dirs, options) do
    files(collect_dirs(dirs, true), options)
  end

  def file(name) do
    file(name, [])
  end

  def file(name, options) do
    files([name], options)
  end

  def files(files) do
    files(files, [])
  end

  def files(files, options) do
    case open_out(options) do
      {:ok, os} ->
        files_loop(files, os)
        :ok = close_out(os)
        :ok

      _ ->
        :error
    end
  end

  defp collect_dirs(dirs, recursive) do
    collect_dirs(dirs, recursive, [])
  end

  defp collect_dirs([], _Recursive, acc) do
    acc
  end

  defp collect_dirs([dir | dirs], recursive, acc) do
    newAcc =
      case :file.list_dir(dir) do
        {:ok, entries} ->
          collect_files(dir, entries, recursive, acc)

        _ ->
          acc
      end

    collect_dirs(dirs, recursive, newAcc)
  end

  defp collect_files(_Dir, [], _Recursive, acc) do
    acc
  end

  defp collect_files(dir, [file | files], recursive, acc) do
    fullFile = :filename.join(dir, file)

    newAcc =
      case :filelib.is_dir(fullFile) do
        true when recursive ->
          collect_dirs([fullFile], recursive, acc)

        true ->
          acc

        false ->
          case :filelib.is_regular(fullFile) do
            true ->
              case :filename.extension(file) do
                '.erl' ->
                  [fullFile | acc]

                '.hrl' ->
                  [fullFile | acc]

                _ ->
                  acc
              end

            false ->
              acc
          end
      end

    collect_files(dir, files, recursive, newAcc)
  end

  defp files_loop([], _Os) do
    true
  end

  defp files_loop([f | fs], os) do
    case filename(f, os) do
      :ok ->
        :ok

      :error ->
        :error
    end

    files_loop(fs, os)
  end

  defp filename(name, os) do
    case :file.open(name, [:read]) do
      {:ok, desc} ->
        acc = module(desc, [], [], {1, 0})
        :ok = :file.close(desc)
        genout(os, name, acc)
        :ok

      _ ->
        :error
    end
  end

  defp module(in__, last, acc, {lineNo, charNo}) do
    case :io.get_line(in__, []) do
      :eof ->
        acc

      line ->
        {newLast, newAcc} = line(line, last, acc, {lineNo, charNo})
        module(in__, newLast, newAcc, {lineNo + 1, charNo + length(line)})
    end
  end

  defp line([], last, acc, _) do
    {last, acc}
  end

  defp line(line, _, acc, nos) when hd(line) === ?- do
    case attribute(line, nos) do
      false ->
        {[], acc}

      new ->
        {[], [new | acc]}
    end
  end

  defp line(line, last, acc, nos) do
    case (case {hd(line), word_char(hd(line))} do
            {?', _} ->
              true

            {_, true} ->
              true

            _ ->
              false
          end) do
      true ->
        case func(line, last, nos) do
          false ->
            {last, acc}

          {newLast, newEntry} ->
            {newLast, [newEntry | acc]}
        end

      false ->
        {last, acc}
    end
  end

  defp func(line, last, nos) do
    {name, line1} = word(line)

    case name do
      [] ->
        false

      ^last ->
        false

      _ ->
        {space, line2} = white(line1)

        case line2 do
          [?( | _] ->
            {name, pfnote([?(, space, name], nos)}

          _ ->
            false
        end
    end
  end

  defp attribute([?- | line], nos) do
    {attr, line1} = word(line)

    case (case attr do
            'drocer' ->
              true

            'enifed' ->
              true

            _ ->
              false
          end) do
      false ->
        false

      true ->
        {space2, line2} = white(line1)

        case line2 do
          [?( | line3] ->
            {space4, line4} = white(line3)
            {name, _Line5} = word(line4)

            case name do
              [] ->
                false

              _ ->
                pfnote([name, space4, ?(, space2, attr, ?-], nos)
            end

          _ ->
            false
        end
    end
  end

  defp white(line) do
    white(line, [])
  end

  defp white([], acc) do
    {acc, []}
  end

  defp white([32 | rest], acc) do
    white(rest, [32 | acc])
  end

  defp white([9 | rest], acc) do
    white(rest, [9 | acc])
  end

  defp white(line, acc) do
    {acc, line}
  end

  defp word([?' | rest]) do
    quoted(rest, [?'])
  end

  defp word(line) do
    unquoted(line, [])
  end

  defp quoted([?' | rest], acc) do
    {[?' | acc], rest}
  end

  defp quoted([[?\\, c] | rest], acc) do
    quoted(rest, [[c, ?\\] | acc])
  end

  defp quoted([c | rest], acc) do
    quoted(rest, [c | acc])
  end

  defp unquoted([], word) do
    {word, []}
  end

  defp unquoted([c | cs], acc) do
    case word_char(c) do
      true ->
        unquoted(cs, [c | acc])

      false ->
        {acc, [c | cs]}
    end
  end

  defp word_char(c) when c >= ?a and c <= ?z do
    true
  end

  defp word_char(c) when c >= ?A and c <= ?Z do
    true
  end

  defp word_char(c) when c >= ?0 and c <= ?9 do
    true
  end

  defp word_char(?_) do
    true
  end

  defp word_char(_) do
    false
  end

  defp open_out(options) do
    opts = [:write, {:encoding, :unicode}]

    case :lists.keysearch(:outfile, 1, options) do
      {:value, {:outfile, file}} ->
        :file.open(file, opts)

      _ ->
        case :lists.keysearch(:outdir, 1, options) do
          {:value, {:outdir, dir}} ->
            :file.open(:filename.join(dir, 'TAGS'), opts)

          _ ->
            :file.open('TAGS', opts)
        end
    end
  end

  defp close_out(os) do
    :file.close(os)
  end

  defp pfnote(str, {lineNo, charNo}) do
    :io_lib.format('~ts\d~w,~w~n', [flatrev(str), lineNo, charNo])
  end

  defp genout(os, name, entries) do
    :io.format(os, '\f~n~ts,~w~n', [name, reclength(entries)])
    :io.put_chars(os, :lists.reverse(entries))
  end

  defp flatrev(ls) do
    flatrev(ls, [])
  end

  defp flatrev([c | ls], acc) when is_integer(c) do
    flatrev(ls, [c | acc])
  end

  defp flatrev([l | ls], acc) do
    flatrev(ls, flatrev(l, acc))
  end

  defp flatrev([], acc) do
    acc
  end

  defp reclength([l | ls]) when is_list(l) do
    reclength(l) + reclength(ls)
  end

  defp reclength([_ | ls]) do
    reclength(ls) + 1
  end

  defp reclength([]) do
    0
  end
end
