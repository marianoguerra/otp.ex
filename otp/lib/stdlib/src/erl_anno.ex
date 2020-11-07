defmodule :m_erl_anno do
  use Bitwise

  def to_term(anno) do
    anno
  end

  def from_term(line) when is_integer(line) and line < 0 do
    set_generated(true, new(-line))
  end

  def from_term(term) do
    term
  end

  def new(line) when is_integer(line) and line >= 0 do
    new_location(line)
  end

  def new({line, column} = loc)
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    new_location(loc)
  end

  def new(term) do
    :erlang.error(:badarg, [term])
  end

  defp new_location(location) do
    location
  end

  def is_anno(line) when is_integer(line) and line >= 0 do
    true
  end

  def is_anno({line, column})
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    true
  end

  def is_anno(anno) do
    anno !== [] and is_anno1(anno) and :lists.keymember(:location, 1, anno)
  end

  defp is_anno1([{item, value} | anno]) do
    is_anno2(item, value) and is_anno1(anno)
  end

  defp is_anno1(a) do
    a === []
  end

  defp is_anno2(:location, line)
       when is_integer(line) and
              line >= 0 do
    true
  end

  defp is_anno2(:location, {line, column})
       when is_integer(line) and line >= 0 and
              is_integer(column) and column >= 1 do
    true
  end

  defp is_anno2(:generated, true) do
    true
  end

  defp is_anno2(:file, filename) do
    is_filename(filename)
  end

  defp is_anno2(:record, true) do
    true
  end

  defp is_anno2(:text, text) do
    is_string(text)
  end

  defp is_anno2(_, _) do
    false
  end

  defp is_filename(t) do
    is_list(t) or is_binary(t)
  end

  defp is_string(t) do
    is_list(t)
  end

  def column({line, column})
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    column
  end

  def column(line) when is_integer(line) and line >= 0 do
    :undefined
  end

  def column(anno) do
    case location(anno) do
      {_Line, column} ->
        column

      _Line ->
        :undefined
    end
  end

  def end_location(anno) do
    case text(anno) do
      :undefined ->
        :undefined

      text ->
        case location(anno) do
          {line, column} ->
            end_location(text, line, column)

          line ->
            end_location(text, line)
        end
    end
  end

  def file(line) when is_integer(line) and line >= 0 do
    :undefined
  end

  def file({line, column})
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    :undefined
  end

  def file(anno) do
    anno_info(anno, :file)
  end

  def generated(line) when is_integer(line) and line >= 0 do
    false
  end

  def generated({line, column})
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    false
  end

  def generated(anno) do
    anno_info(anno, :generated, false)
  end

  def line(anno) do
    case location(anno) do
      {line, _Column} ->
        line

      line ->
        line
    end
  end

  def location(line) when is_integer(line) and line >= 0 do
    line
  end

  def location({line, column} = location)
      when is_integer(line) and line >= 0 and
             is_integer(column) and column >= 1 do
    location
  end

  def location(anno) do
    anno_info(anno, :location)
  end

  def record(line) when is_integer(line) and line >= 0 do
    false
  end

  def record({line, column})
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    false
  end

  def record(anno) do
    anno_info(anno, :record, false)
  end

  def text(line) when is_integer(line) and line >= 0 do
    :undefined
  end

  def text({line, column})
      when is_integer(line) and
             line >= 0 and
             is_integer(column) and column >= 1 do
    :undefined
  end

  def text(anno) do
    anno_info(anno, :text)
  end

  def set_file(file, anno) do
    set(:file, file, anno)
  end

  def set_generated(generated, anno) do
    set(:generated, generated, anno)
  end

  def set_line(line, anno) do
    case location(anno) do
      {_Line, column} ->
        set_location({line, column}, anno)

      _Line ->
        set_location(line, anno)
    end
  end

  def set_location(line, l)
      when is_integer(l) and l >= 0 and
             is_integer(line) and line >= 0 do
    new_location(line)
  end

  def set_location(line, {l, column})
      when is_integer(l) and
             l >= 0 and
             is_integer(column) and column >= 1 and
             is_integer(line) and line >= 0 do
    new_location(line)
  end

  def set_location({l, c} = loc, line)
      when is_integer(line) and
             line >= 0 and is_integer(l) and l >= 0 and
             is_integer(c) and c >= 1 do
    new_location(loc)
  end

  def set_location({l, c} = loc, {line, column})
      when is_integer(line) and line >= 0 and
             is_integer(column) and column >= 1 and is_integer(l) and
             l >= 0 and is_integer(c) and c >= 1 do
    new_location(loc)
  end

  def set_location(location, anno) do
    set(:location, location, anno)
  end

  def set_record(record, anno) do
    set(:record, record, anno)
  end

  def set_text(text, anno) do
    set(:text, text, anno)
  end

  defp set(item, value, anno) do
    case {is_settable(item, value), anno} do
      {true, line} when is_integer(line) and line >= 0 ->
        set_anno(item, value, [{:location, line}])

      {true, {l, c} = location}
      when is_integer(l) and
             l >= 0 and is_integer(c) and c >= 1 ->
        set_anno(item, value, [{:location, location}])

      {true, a} when is_list(a) and a !== [] ->
        set_anno(item, value, anno)

      _ ->
        :erlang.error(:badarg, [item, value, anno])
    end
  end

  defp set_anno(item, value, anno) do
    case default(item, value) do
      true ->
        reset(anno, item)

      false ->
        r =
          case anno_info(anno, item) do
            :undefined ->
              [{item, value} | anno]

            _ ->
              :lists.keyreplace(item, 1, anno, {item, value})
          end

        reset_simplify(r)
    end
  end

  defp reset(anno, item) do
    a = :lists.keydelete(item, 1, anno)
    reset_simplify(a)
  end

  defp reset_simplify(a) do
    simplify(a)
  end

  defp simplify([{:location, location}]) do
    location
  end

  defp simplify(anno) do
    anno
  end

  defp anno_info(anno, item, default) do
    try do
      :lists.keyfind(item, 1, anno)
    catch
      _, _ ->
        :erlang.error(:badarg, [anno])
    else
      false ->
        default

      {^item, value} ->
        value
    end
  end

  defp anno_info(anno, item) do
    try do
      :lists.keyfind(item, 1, anno)
    catch
      _, _ ->
        :erlang.error(:badarg, [anno])
    else
      {^item, value} ->
        value

      false ->
        :undefined
    end
  end

  defp end_location('', line, column) do
    {line, column}
  end

  defp end_location([?\n | string], line, _Column) do
    end_location(string, line + 1, 1)
  end

  defp end_location([_ | string], line, column) do
    end_location(string, line, column + 1)
  end

  defp end_location('', line) do
    line
  end

  defp end_location([?\n | string], line) do
    end_location(string, line + 1)
  end

  defp end_location([_ | string], line) do
    end_location(string, line)
  end

  defp is_settable(:file, file) do
    is_filename(file)
  end

  defp is_settable(:generated, boolean)
       when boolean or
              not boolean do
    true
  end

  defp is_settable(:location, line)
       when is_integer(line) and
              line >= 0 do
    true
  end

  defp is_settable(:location, {line, column})
       when is_integer(line) and line >= 0 and
              is_integer(column) and column >= 1 do
    true
  end

  defp is_settable(:record, boolean) when boolean or not boolean do
    true
  end

  defp is_settable(:text, text) do
    is_string(text)
  end

  defp is_settable(_, _) do
    false
  end

  defp default(:generated, false) do
    true
  end

  defp default(:record, false) do
    true
  end

  defp default(_, _) do
    false
  end
end
