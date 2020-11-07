defmodule :m_sys_pre_attributes do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    forms: :undefined,
    pre_ops: [],
    post_ops: [],
    options: :undefined
  )

  def parse_transform(forms, options) do
    s = r_state(forms: forms, options: options)
    s2 = init_transform(options, s)
    report_verbose('Pre  options: ~p~n', [r_state(s2, :pre_ops)], s2)
    report_verbose('Post options: ~p~n', [r_state(s2, :post_ops)], s2)
    s3 = pre_transform(s2)
    s4 = post_transform(s3)
    r_state(s4, :forms)
  end

  defp init_transform([{:attribute, :insert, name, val} | tail], s) do
    op = {:insert, name, val}
    postOps = [op | r_state(s, :post_ops)]
    init_transform(tail, r_state(s, post_ops: postOps))
  end

  defp init_transform(
         [{:attribute, :replace, name, val} | tail],
         s
       ) do
    op = {:replace, name, val}
    preOps = [op | r_state(s, :pre_ops)]
    postOps = [op | r_state(s, :post_ops)]

    init_transform(
      tail,
      r_state(s, pre_ops: preOps, post_ops: postOps)
    )
  end

  defp init_transform([{:attribute, :delete, name} | tail], s) do
    op = {:delete, name}
    preOps = [op | r_state(s, :pre_ops)]
    init_transform(tail, r_state(s, pre_ops: preOps))
  end

  defp init_transform([_ | t], s) do
    init_transform(t, s)
  end

  defp init_transform([], s) do
    s
  end

  defp pre_transform(r_state(pre_ops: []) = s) do
    s
  end

  defp pre_transform(s) do
    pre_transform(r_state(s, :forms), [], s)
  end

  defp pre_transform([h | t], acc, s) do
    case h do
      {:attribute, line, name, val} ->
        case :lists.keyfind(name, 2, r_state(s, :pre_ops)) do
          false ->
            pre_transform(t, [h | acc], s)

          {:replace, ^name, newVal} ->
            report_warning('Replace attribute ~p: ~p -> ~p~n', [name, val, newVal], s)
            new = {:attribute, line, name, newVal}
            pre = :lists.keydelete(name, 2, r_state(s, :pre_ops))
            post = :lists.keydelete(name, 2, r_state(s, :post_ops))
            s2 = r_state(s, pre_ops: pre, post_ops: post)

            cond do
              pre == [] ->
                forms = :lists.reverse(acc, [new | t])
                r_state(s2, forms: forms)

              true ->
                pre_transform(t, [new | acc], s2)
            end

          {:delete, ^name} ->
            report_warning('Delete attribute ~p: ~p~n', [name, val], s)
            pre_transform(t, acc, s)
        end

      _Any ->
        pre_transform(t, [h | acc], s)
    end
  end

  defp pre_transform([], acc, s) do
    r_state(s, forms: :lists.reverse(acc))
  end

  defp post_transform(r_state(post_ops: []) = s) do
    s
  end

  defp post_transform(s) do
    post_transform(r_state(s, :forms), [], s)
  end

  defp post_transform([h | t], acc, s) do
    case h do
      {:attribute, line, :module, _Val} = attribute ->
        acc2 = :lists.reverse([attribute | acc])
        forms = acc2 ++ attrs(r_state(s, :post_ops), line, s) ++ t
        r_state(s, forms: forms, post_ops: [])

      _Any ->
        post_transform(t, [h | acc], s)
    end
  end

  defp post_transform([], acc, s) do
    r_state(s, forms: :lists.reverse(acc))
  end

  defp attrs([{:replace, name, newVal} | t], line, s) do
    report_verbose('Insert attribute ~p: ~p~n', [name, newVal], s)
    [{:attribute, line, name, newVal} | attrs(t, line, s)]
  end

  defp attrs([{:insert, name, newVal} | t], line, s) do
    report_verbose('Insert attribute ~p: ~p~n', [name, newVal], s)
    [{:attribute, line, name, newVal} | attrs(t, line, s)]
  end

  defp attrs([], _, _) do
    []
  end

  defp report_warning(format, args, s) do
    case is_warning(s) do
      true ->
        :io.format('~p: * WARNING * ' ++ format, [:sys_pre_attributes | args])

      false ->
        :ok
    end
  end

  defp report_verbose(format, args, s) do
    case is_verbose(s) do
      true ->
        :io.format('~p: ' ++ format, [:sys_pre_attributes | args])

      false ->
        :ok
    end
  end

  defp is_warning(s) do
    :erlang.or(
      :lists.member(
        :report_warnings,
        r_state(s, :options)
      ),
      is_verbose(s)
    )
  end

  defp is_verbose(s) do
    :lists.member(:verbose, r_state(s, :options))
  end
end
