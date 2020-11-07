defmodule :m_dbg_istk do
  use Bitwise
  require Record

  Record.defrecord(:r_ieval, :ieval,
    level: 1,
    line: -1,
    module: :undefined,
    function: :undefined,
    arguments: :undefined,
    top: false
  )

  Record.defrecord(:r_e, :e,
    level: :undefined,
    mfa: :undefined,
    line: :undefined,
    bindings: :undefined,
    lc: :undefined
  )

  def init() do
    init([])
  end

  def delayed_to_external() do
    stack = :erlang.get(:dbg_istk)

    fn ->
      {:stack, :erlang.term_to_binary(stack)}
    end
  end

  def from_external({:stack, stk}) do
    :erlang.put(:dbg_istk, :erlang.binary_to_term(stk))
  end

  defp init(stack) do
    :erlang.put(:dbg_istk, stack)
  end

  def push(
        bs,
        r_ieval(level: le, module: mod, function: name, arguments: as, line: li) = ieval,
        lc
      ) do
    entry = r_e(level: le, mfa: {mod, name, as}, line: li, bindings: bs, lc: lc)

    case :erlang.get(:trace_stack) do
      false ->
        r_ieval(ieval, level: le + 1)

      :no_tail when lc ->
        ieval

      _ ->
        :erlang.put(:dbg_istk, [entry | :erlang.get(:dbg_istk)])
        r_ieval(ieval, level: le + 1)
    end
  end

  def pop() do
    case :erlang.get(:trace_stack) do
      false ->
        :ignore

      _ ->
        case :erlang.get(:dbg_istk) do
          [_Entry | entries] ->
            :erlang.put(:dbg_istk, entries)

          [] ->
            :ignore
        end
    end
  end

  def pop(le) do
    case :erlang.get(:trace_stack) do
      false ->
        :ignore

      _ ->
        :erlang.put(:dbg_istk, pop(le, :erlang.get(:dbg_istk)))
    end
  end

  defp pop(level, [r_e(level: le) | stack])
       when level <= le do
    pop(level, stack)
  end

  defp pop(_Level, stack) do
    stack
  end

  def stack_level() do
    stack_level(:erlang.get(:dbg_istk))
  end

  defp stack_level([]) do
    1
  end

  defp stack_level([r_e(level: le) | _]) do
    le
  end

  def delayed_stacktrace() do
    stack0 = :erlang.get(:dbg_istk)

    fn numEntries ->
      stack = stacktrace(numEntries, stack0, [])

      for {arityOnly, _} <- stack do
        finalize(arityOnly)
      end
    end
  end

  def delayed_stacktrace(:include_args, ieval) do
    r_ieval(module: mod, function: name, arguments: as, line: li) = ieval

    stack0 = [
      r_e(mfa: {mod, name, as}, line: li)
      | :erlang.get(:dbg_istk)
    ]

    fn numEntries ->
      case stacktrace(numEntries, stack0, []) do
        [] ->
          []

        [{_, withArgs} | stack] ->
          [
            finalize(withArgs)
            | for {arityOnly, _} <- stack do
                finalize(arityOnly)
              end
          ]
      end
    end
  end

  def delayed_stacktrace(:no_args, ieval) do
    r_ieval(module: mod, function: name, arguments: as, line: li) = ieval

    stack0 = [
      r_e(mfa: {mod, name, as}, line: li)
      | :erlang.get(:dbg_istk)
    ]

    fn numEntries ->
      stack = stacktrace(numEntries, stack0, [])

      for {arityOnly, _} <- stack do
        finalize(arityOnly)
      end
    end
  end

  defp stacktrace(n, [r_e(lc: true) | t], acc) do
    stacktrace(n, t, acc)
  end

  defp stacktrace(n, [e | t], []) do
    stacktrace(n - 1, t, [normalize(e)])
  end

  defp stacktrace(n, [e | t], [{p, _} | _] = acc) when n > 0 do
    case normalize(e) do
      {^p, _} ->
        stacktrace(n, t, acc)

      new ->
        stacktrace(n - 1, t, [new | acc])
    end
  end

  defp stacktrace(_, _, acc) do
    :lists.reverse(acc)
  end

  defp normalize(r_e(mfa: {m, fun, as}, line: li))
       when is_function(fun) do
    loc = {m, li}
    {{fun, length(as), loc}, {fun, as, loc}}
  end

  defp normalize(r_e(mfa: {m, f, as}, line: li)) do
    loc = {m, li}
    {{m, f, length(as), loc}, {m, f, as, loc}}
  end

  defp finalize({m, f, a, loc}) do
    {m, f, a, line(loc)}
  end

  defp finalize({fun, a, loc}) do
    {fun, a, line(loc)}
  end

  defp line({mod, line}) when line > 0 do
    [{:file, :erlang.atom_to_list(mod) ++ '.erl'}, {:line, line}]
  end

  defp line(_) do
    []
  end

  def bindings(sP) do
    bindings(sP, :erlang.get(:dbg_istk))
  end

  defp bindings(sP, [r_e(level: sP, bindings: bs) | _]) do
    bs
  end

  defp bindings(sP, [_Entry | entries]) do
    bindings(sP, entries)
  end

  defp bindings(_SP, []) do
    :erl_eval.new_bindings()
  end

  def stack_frame(:up, sP) do
    stack_frame(sP, :up, :erlang.get(:dbg_istk))
  end

  def stack_frame(:down, sP) do
    stack_frame(sP, :down, :lists.reverse(:erlang.get(:dbg_istk)))
  end

  defp stack_frame(sP, :up, [
         r_e(level: le, mfa: {cm, _, _}, line: li, bindings: bs)
         | _
       ])
       when le < sP do
    {le, {cm, li}, bs}
  end

  defp stack_frame(sP, :down, [
         r_e(level: le, mfa: {cm, _, _}, line: li, bindings: bs)
         | _
       ])
       when le > sP do
    {le, {cm, li}, bs}
  end

  defp stack_frame(sP, dir, [r_e(level: sP) | stack]) do
    case stack do
      [
        r_e(level: le, mfa: {cm, _, _}, line: li, bindings: bs)
        | _
      ] ->
        {le, {cm, li}, bs}

      [] when dir === :up ->
        :top

      [] when dir === :down ->
        :bottom
    end
  end

  defp stack_frame(sP, dir, [_Entry | stack]) do
    stack_frame(sP, dir, stack)
  end

  def backtrace(howMany, ieval) do
    r_ieval(level: level, module: mod, function: name, arguments: as) = ieval

    stack0 = [
      r_e(level: level, mfa: {mod, name, as})
      | :erlang.get(:dbg_istk)
    ]

    stack =
      case howMany do
        :all ->
          stack0

        n ->
          :lists.sublist(stack0, n)
      end

    for r_e(level: le, mfa: mFA) <- stack do
      {le, mFA}
    end
  end

  def in_use_p(mod, mod) do
    true
  end

  def in_use_p(mod, _Cm) do
    case :erlang.get(:trace_stack) do
      false ->
        true

      _ ->
        :lists.any(
          fn
            r_e(mfa: {m, _, _}) when m === mod ->
              true

            _ ->
              false
          end,
          :erlang.get(:dbg_istk)
        )
    end
  end
end
