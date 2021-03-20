defmodule :m_prettypr do
  use Bitwise
  require Record
  Record.defrecord(:r_text, :text, s: :undefined)

  Record.defrecord(:r_nest, :nest,
    n: :undefined,
    d: :undefined
  )

  Record.defrecord(:r_beside, :beside,
    d1: :undefined,
    d2: :undefined
  )

  Record.defrecord(:r_above, :above,
    d1: :undefined,
    d2: :undefined
  )

  Record.defrecord(:r_sep, :sep, ds: :undefined, i: 0, p: false)
  Record.defrecord(:r_float, :float, d: :undefined, h: :undefined, v: :undefined)

  Record.defrecord(:r_union, :union,
    d1: :undefined,
    d2: :undefined
  )

  Record.defrecord(:r_fit, :fit, d: :undefined)

  def text(s) do
    mktext(string(s))
  end

  defp mktext(s) do
    r_text(s: s)
  end

  def null_text(s) do
    mktext(null_string(s))
  end

  def text_par(s) do
    text_par(s, 0)
  end

  def text_par(s, 0) do
    par(words(s))
  end

  def text_par(s, n) when n > 0 do
    nest(n, par(words(s), -n))
  end

  def text_par(s, n) when n < 0 do
    par(words(s), -n)
  end

  defp words(s) do
    words(s, [], [])
  end

  defp words([?\s | cs], as, ws) do
    words_1(cs, as, ws)
  end

  defp words([?\t | cs], as, ws) do
    words_1(cs, as, ws)
  end

  defp words([?\n | cs], as, ws) do
    words_1(cs, as, ws)
  end

  defp words([c | cs], as, ws) do
    words(cs, [c | as], ws)
  end

  defp words([], [], ws) do
    :lists.reverse(ws)
  end

  defp words([], as, ws) do
    words_1([], as, ws)
  end

  defp words_1(cs, [], ws) do
    words(cs, [], ws)
  end

  defp words_1(cs, as, ws) do
    words(cs, [], [text(:lists.reverse(as)) | ws])
  end

  def empty() do
    :null
  end

  def break(d) do
    above(d, empty())
  end

  def nest(n, d) do
    cond do
      n === 0 ->
        d

      true ->
        r_nest(n: n, d: d)
    end
  end

  def beside(d1, d2) do
    r_beside(d1: d1, d2: d2)
  end

  def above(d1, d2) do
    r_above(d1: d1, d2: d2)
  end

  def sep(ds) do
    r_sep(ds: ds)
  end

  def par(ds) do
    par(ds, 0)
  end

  def par(ds, n) do
    mksep(ds, n, true)
  end

  defp mksep(ds, n, p) when is_integer(n) do
    r_sep(ds: ds, i: n, p: p)
  end

  def follow(d1, d2) do
    follow(d1, d2, 0)
  end

  def follow(d1, d2, n) when is_integer(n) do
    beside(par([d1, __MODULE__.nil()], n), d2)
  end

  def floating(d) do
    floating(d, 0, 0)
  end

  def floating(d, h, v)
      when is_integer(h) and
             is_integer(v) do
    r_float(d: d, h: h, v: v)
  end

  def format(d) do
    format(d, 80)
  end

  def format(d, w) do
    format(d, w, 65)
  end

  def format(d, w, r) do
    case best(d, w, r) do
      :empty ->
        throw(:no_layout)

      l ->
        layout(l)
    end
  end

  defp layout(l) do
    :lists.reverse(layout(0, l, []))
  end

  defp layout(n, r_above(d1: r_text(s: [_ | '']), d2: l), cs) do
    layout(n, l, [?\n | cs])
  end

  defp layout(n, r_above(d1: r_text(s: s), d2: l), cs) do
    layout(n, l, [?\n | flatrev(string_chars(s), indent(n, cs))])
  end

  defp layout(n, r_nest(n: n1, d: l), cs) do
    layout(n + n1, l, cs)
  end

  defp layout(n, r_text(s: s), cs) do
    flatrev(string_chars(s), indent(n, cs))
  end

  defp layout(_N, :null, cs) do
    cs
  end

  defp indent(n, cs) when n > 0 do
    indent(n - 1, [?\s | cs])
  end

  defp indent(_N, cs) do
    cs
  end

  defp flatrev(cs, as) do
    flatrev(cs, as, [])
  end

  defp flatrev([c = [_ | _] | cs], as, ss) do
    flatrev(c, as, [cs | ss])
  end

  defp flatrev([[] | cs], as, ss) do
    flatrev(cs, as, ss)
  end

  defp flatrev([c | cs], as, ss) do
    flatrev(cs, [c | as], ss)
  end

  defp flatrev([], as, [s | ss]) do
    flatrev(s, as, ss)
  end

  defp flatrev([], as, []) do
    as
  end

  Record.defrecord(:r_c_best_nest, :c_best_nest, w: :undefined, r: :undefined, i: :undefined)
  Record.defrecord(:r_c_above_nest, :c_above_nest, d: :undefined, i: 0, c: :undefined)

  Record.defrecord(:r_c_beside, :c_beside,
    d: :undefined,
    c: :undefined
  )

  Record.defrecord(:r_c_text_beside, :c_text_beside,
    s: :undefined,
    c: :undefined
  )

  Record.defrecord(:r_c_sep_nest, :c_sep_nest,
    ds: :undefined,
    i: :undefined,
    p: :undefined,
    c: :undefined
  )

  Record.defrecord(:r_c_best_nest_or, :c_best_nest_or,
    w: :undefined,
    r: :undefined,
    i: :undefined,
    d: :undefined
  )

  Record.defrecord(:r_c_fit, :c_fit, c: :undefined)

  Record.defrecord(:r_c_float_beside, :c_float_beside,
    d: :undefined,
    h: :undefined,
    v: :undefined,
    c: :undefined
  )

  Record.defrecord(:r_c_float_above_nest, :c_float_above_nest,
    d: :undefined,
    h: :undefined,
    v: :undefined,
    i: :undefined,
    c: :undefined
  )

  def best(d, w, r) do
    rewrite(d, r_c_best_nest(w: w, r: r, i: 0))
  end

  defp rewrite(r_text(s: s), c) do
    case c do
      r_c_best_nest(i: n) ->
        nest(n, mktext(s))

      r_c_above_nest(d: d1, i: n1, c: c1) ->
        case c1 do
          r_c_best_nest(w: w, r: r, i: n) ->
            nest(
              n,
              above(mktext(s), rewrite(d1, r_c_best_nest(w: w - n, r: r, i: n1)))
            )

          r_c_beside(d: d2, c: c2) ->
            rewrite(above(mktext(s), nest(n1, beside(d1, d2))), c2)

          r_c_text_beside(s: s1, c: c2) ->
            rewrite(
              above(
                mktext(concat(s1, s)),
                nest(n1 + width(s1), d1)
              ),
              c2
            )

          r_c_sep_nest(ds: ds, i: n, c: c2) ->
            case is_empty_string(s) do
              false ->
                w = width(s)

                rewrite(
                  beside(
                    mktext(s),
                    mksep(
                      [
                        above(
                          __MODULE__.nil(),
                          nest(n1 - w, d1)
                        )
                        | ds
                      ],
                      n - w,
                      r_c_sep_nest(c1, :p)
                    )
                  ),
                  c2
                )

              true ->
                case r_c_sep_nest(c1, :p) do
                  false ->
                    rewrite(
                      expand_sep(
                        above(
                          __MODULE__.nil(),
                          nest(n1, d1)
                        ),
                        ds,
                        n
                      ),
                      c2
                    )

                  true ->
                    rewrite(
                      expand_par(
                        above(
                          __MODULE__.nil(),
                          nest(n1, d1)
                        ),
                        ds,
                        n
                      ),
                      c2
                    )
                end
            end

          r_c_best_nest_or(w: w, r: r, i: n, d: d) ->
            l = width(s)

            case :erlang.or(l + n > w, l > r) do
              true ->
                rewrite(d, r_c_best_nest(w: w, r: r, i: n))

              false ->
                rewrite(
                  above(mktext(s), nest(n1, d1)),
                  r_c_best_nest(w: w, r: r, i: n)
                )
            end

          r_c_float_beside(d: d2, c: c2) ->
            rewrite(beside(d2, above(mktext(s), nest(n1, d1))), c2)

          r_c_float_above_nest(d: d2, i: n2, c: c2) ->
            rewrite(
              above(
                d2,
                nest(n2, above(mktext(s), nest(n1, d1)))
              ),
              c2
            )

          r_c_above_nest() ->
            exit(:badarg)

          r_c_fit() ->
            exit(:badarg)
        end

      r_c_beside(d: d1, c: c1) ->
        case c1 do
          r_c_above_nest(d: d2, i: n, c: c2) ->
            case is_empty_string(s) do
              false ->
                w = width(s)

                rewrite(
                  beside(
                    mktext(s),
                    above(
                      beside(__MODULE__.nil(), d1),
                      nest(n - w, d2)
                    )
                  ),
                  c2
                )

              true ->
                rewrite(d1, r_c_text_beside(s: s, c: c1))
            end

          r_c_text_beside(s: s1, c: c2) ->
            rewrite(beside(mktext(concat(s1, s)), d1), c2)

          r_c_sep_nest(ds: ds, i: n, c: c2) ->
            case is_empty_string(s) do
              false ->
                w = width(s)

                rewrite(
                  beside(
                    mktext(s),
                    mksep([beside(__MODULE__.nil(), d1) | ds], n - w, r_c_sep_nest(c1, :p))
                  ),
                  c2
                )

              true ->
                rewrite(d1, r_c_text_beside(s: s, c: c1))
            end

          r_c_best_nest_or(w: w, r: r, i: n, d: d) ->
            l = width(s)

            case :erlang.or(l + n > w, l > r) do
              true ->
                rewrite(d, r_c_best_nest(w: w, r: r, i: n))

              false ->
                rewrite(d1, r_c_text_beside(s: s, c: c1))
            end

          r_c_float_beside(d: d2, c: c2) ->
            rewrite(beside(d2, beside(mktext(s), d1)), c2)

          r_c_float_above_nest(d: d2, i: n, c: c2) ->
            rewrite(above(d2, nest(n, beside(mktext(s), d1))), c2)

          _ ->
            rewrite(d1, r_c_text_beside(s: s, c: c1))
        end

      r_c_text_beside(s: s1, c: c1) ->
        rewrite(mktext(concat(s1, s)), c1)

      r_c_sep_nest(ds: ds, i: n, c: c1) ->
        case is_empty_string(s) do
          false ->
            rewrite(
              beside(
                mktext(s),
                mksep([__MODULE__.nil() | ds], n - width(s), r_c_sep_nest(c, :p))
              ),
              c1
            )

          true ->
            case r_c_sep_nest(c, :p) do
              false ->
                rewrite(expand_sep(__MODULE__.nil(), ds, n), c1)

              true ->
                rewrite(expand_par(__MODULE__.nil(), ds, n), c1)
            end
        end

      r_c_best_nest_or(w: w, r: r, i: n, d: d) ->
        l = width(s)

        case :erlang.or(l + n > w, l > r) do
          true ->
            rewrite(d, r_c_best_nest(w: w, r: r, i: n))

          false ->
            nest(n, mktext(s))
        end

      r_c_fit(c: c1) ->
        rewrite(mktext(s), c1)

      r_c_float_beside(d: d1, c: c1) ->
        rewrite(beside(d1, mktext(s)), c1)

      r_c_float_above_nest(d: d1, i: n, c: c1) ->
        rewrite(above(d1, nest(n, mktext(s))), c1)
    end
  end

  defp rewrite(r_nest(n: n, d: d), c) do
    case c do
      r_c_best_nest(w: w, r: r, i: n1) ->
        rewrite(d, r_c_best_nest(w: w, r: r, i: n + n1))

      r_c_above_nest(d: d1, i: n1, c: c1) ->
        rewrite(nest(n, above(d, nest(n1 - n, d1))), c1)

      r_c_beside(d: d1, c: c1) ->
        rewrite(nest(n, beside(d, d1)), c1)

      r_c_text_beside() ->
        rewrite(d, c)

      r_c_sep_nest(ds: ds, i: n1, c: c1) ->
        rewrite(nest(n, mksep([d | ds], n1 - n, r_c_sep_nest(c, :p))), c1)

      r_c_fit(c: c1) ->
        rewrite(nest(n, fit(d)), c1)

      r_c_float_beside() ->
        rewrite(d, c)

      r_c_float_above_nest(d: d1, h: h, v: v, i: n1, c: c1) ->
        rewrite(d, r_c_float_above_nest(d: d1, h: h, v: v, i: n + n1, c: c1))

      r_c_best_nest_or() ->
        exit(:badarg)
    end
  end

  defp rewrite(r_above(d1: d1, d2: d2), c) do
    case c do
      r_c_above_nest(d: d3, i: n, c: c1) ->
        rewrite(d1, r_c_above_nest(d: above(d2, nest(n, d3)), c: c1))

      r_c_beside(d: d3, c: c1) ->
        rewrite(above(d1, beside(d2, d3)), c1)

      r_c_fit(c: c1) ->
        rewrite(:empty, c1)

      _ ->
        rewrite(d1, r_c_above_nest(d: d2, c: c))
    end
  end

  defp rewrite(r_beside(d1: d1, d2: d2), c) do
    case c do
      r_c_beside(d: d3, c: c1) ->
        rewrite(d1, r_c_beside(d: beside(d2, d3), c: c1))

      r_c_fit(c: c1) ->
        rewrite(beside(fit(d1), fit(d2)), c1)

      _ ->
        rewrite(d1, r_c_beside(d: d2, c: c))
    end
  end

  defp rewrite(r_sep(ds: ds, i: n, p: p), c) do
    case c do
      r_c_fit(c: c1) ->
        rewrite(fit(horizontal(ds)), c1)

      r_c_float_beside(d: d1, c: c1) ->
        rewrite(beside(d1, mksep(ds, n, p)), c1)

      r_c_float_above_nest(d: d1, i: n1, c: c1) ->
        rewrite(above(d1, nest(n1, mksep(ds, n, p))), c1)

      _ ->
        enter_sep(ds, n, p, c)
    end
  end

  defp rewrite(r_union(d1: d1, d2: d2), c) do
    case c do
      r_c_best_nest(w: w, r: r, i: n) ->
        rewrite(d1, r_c_best_nest_or(w: w, r: r, i: n, d: d2))

      r_c_above_nest(d: d3, i: n, c: c1) ->
        rewrite(
          union(
            above(d1, nest(n, d3)),
            above(d2, nest(n, d3))
          ),
          c1
        )

      r_c_beside(d: d3, c: c1) ->
        rewrite(union(beside(d1, d3), beside(d2, d3)), c1)

      r_c_text_beside(s: s, c: c1) ->
        rewrite(
          union(
            beside(mktext(s), d1),
            beside(mktext(s), d2)
          ),
          c1
        )

      r_c_sep_nest(ds: ds, i: n, c: c1) ->
        rewrite(
          union(
            mksep([d1 | ds], n, r_c_sep_nest(c, :p)),
            mksep([d2 | ds], n, r_c_sep_nest(c, :p))
          ),
          c1
        )

      r_c_best_nest_or(w: w, r: r, i: n, d: d3) ->
        rewrite(d1, r_c_best_nest_or(w: w, r: r, i: n, d: union(d2, d3)))

      r_c_fit(c: c1) ->
        rewrite(union(fit(d1), fit(d2)), c1)

      r_c_float_beside(d: d3, h: h, v: v, c: c1) ->
        rewrite(
          union(
            beside(floating(d3, h, v), d1),
            beside(floating(d3, h, v), d2)
          ),
          c1
        )

      r_c_float_above_nest(d: d3, h: h, v: v, i: n, c: c1) ->
        rewrite(
          union(
            above(floating(d3, h, v), nest(n, d1)),
            above(floating(d3, h, v), nest(n, d2))
          ),
          c1
        )
    end
  end

  defp rewrite(:empty, c) do
    case c do
      r_c_best_nest() ->
        :empty

      r_c_above_nest(c: c1) ->
        rewrite(:empty, c1)

      r_c_beside(c: c1) ->
        rewrite(:empty, c1)

      r_c_text_beside(c: c1) ->
        rewrite(:empty, c1)

      r_c_sep_nest(c: c1) ->
        rewrite(:empty, c1)

      r_c_best_nest_or(w: w, r: r, i: n, d: d) ->
        rewrite(d, r_c_best_nest(w: w, r: r, i: n))

      r_c_fit(c: c1) ->
        rewrite(:empty, c1)

      r_c_float_beside(c: c1) ->
        rewrite(:empty, c1)

      r_c_float_above_nest(c: c1) ->
        rewrite(:empty, c1)
    end
  end

  defp rewrite(r_fit(d: d), c) do
    case c do
      r_c_fit() ->
        rewrite(d, c)

      _ ->
        rewrite(d, r_c_fit(c: c))
    end
  end

  defp rewrite(r_float(d: d, h: h, v: v), c) do
    case c do
      r_c_beside(d: d1, c: c1) ->
        case c1 do
          r_c_float_beside(d: d2, h: h1, v: v1, c: c2) when h1 > h ->
            rewrite(
              beside(
                floating(d, h, v),
                beside(floating(d2, h1, v1), d1)
              ),
              c2
            )

          r_c_float_beside(d: d2, h: h1, v: v1, c: c2) when v1 != v ->
            rewrite(
              above(
                floating(d2, h1, v1),
                beside(floating(d, h, v), d1)
              ),
              c2
            )

          r_c_float_above_nest(d: d2, h: h1, v: v1, i: n1, c: c2) when v1 > v ->
            rewrite(
              above(
                nest(n1, floating(d, h, v)),
                above(floating(d2, h1, v1), d1)
              ),
              c2
            )

          r_c_float_above_nest(d: d2, h: h1, v: v1, i: _N1, c: c2)
          when v1 == v and
                 h1 != h ->
            rewrite(
              beside(
                floating(d2, h1, v1),
                beside(floating(d, h, v), d1)
              ),
              c2
            )

          _ ->
            rewrite(d1, r_c_float_beside(d: d, h: h, v: v, c: c1))
        end

      r_c_above_nest(d: d1, i: n, c: c1) ->
        case c1 do
          r_c_float_beside(d: d2, h: h1, v: v1, c: c2) when h1 > h ->
            rewrite(
              beside(
                floating(d, h, v),
                beside(floating(d2, h1, v1), d1)
              ),
              c2
            )

          r_c_float_beside(d: d2, h: h1, v: v1, c: c2) when v1 != v ->
            rewrite(
              above(
                floating(d2, h1, v1),
                above(floating(d, h, v), nest(n, d1))
              ),
              c2
            )

          r_c_float_above_nest(d: d2, h: h1, v: v1, i: n1, c: c2) when v1 > v ->
            rewrite(
              above(
                nest(n1, floating(d, h, v)),
                above(floating(d2, h1, v1), nest(n + n1, d1))
              ),
              c2
            )

          r_c_float_above_nest(d: d2, h: h1, v: v1, i: _N1, c: c2)
          when v1 == v and
                 h1 != h ->
            rewrite(
              beside(
                floating(d2, h1, v1),
                above(floating(d, h, v), nest(n, d1))
              ),
              c2
            )

          _ ->
            rewrite(d1, r_c_float_above_nest(d: d, h: h, v: v, i: n, c: c1))
        end

      r_c_fit(c: c1) ->
        rewrite(floating(fit(d), h, v), c1)

      r_c_float_beside(d: d1, h: h1, v: v1, c: c1) ->
        cond do
          h1 > h ->
            rewrite(
              beside(floating(d, h, v), floating(d1, h1, v1)),
              c1
            )

          v1 != v ->
            rewrite(
              above(floating(d, h, v), floating(d1, h1, v1)),
              c1
            )

          true ->
            rewrite(beside(floating(d1, h1, v1), d), c1)
        end

      r_c_float_above_nest(d: d1, h: h1, v: v1, i: n, c: c1) ->
        cond do
          v1 > v ->
            rewrite(
              above(
                nest(n, floating(d, h, v)),
                floating(d1, h1, v1)
              ),
              c1
            )

          v1 == v and h1 != h ->
            rewrite(
              beside(floating(d, h, v), floating(d1, h1, v1)),
              c1
            )

          true ->
            rewrite(above(floating(d1, h1, v1), nest(n, d)), c1)
        end

      _ ->
        rewrite(d, c)
    end
  end

  defp rewrite(:null, c) do
    case c do
      r_c_best_nest() ->
        :null

      r_c_above_nest(d: d, i: n, c: c1) ->
        rewrite(nest(n, d), c1)

      r_c_beside(d: d, c: c1) ->
        rewrite(d, c1)

      r_c_text_beside(s: s, c: c1) ->
        rewrite(mktext(s), c1)

      r_c_sep_nest() ->
        rewrite(__MODULE__.nil(), c)

      r_c_best_nest_or(w: w, r: r, i: n) ->
        rewrite(:null, r_c_best_nest(w: w, r: r, i: n))

      r_c_fit(c: c1) ->
        rewrite(:null, c1)

      r_c_float_beside(d: d, h: _H, v: _V, c: c1) ->
        rewrite(beside(d, :null), c1)

      r_c_float_above_nest(d: d, h: _H, v: _V, i: n, c: c1) ->
        rewrite(above(d, nest(n, :null)), c1)
    end
  end

  defp unquote(nil)() do
    text('')
  end

  defp hspace() do
    text([?\s])
  end

  defp union(d1, d2) do
    r_union(d1: d1, d2: d2)
  end

  defp fit(d) do
    r_fit(d: d)
  end

  defp enter_sep(ds, n, p, c) do
    case ds do
      [d] ->
        rewrite(d, c)

      [d | ds1] ->
        rewrite(d, r_c_sep_nest(ds: ds1, i: n, p: p, c: c))
    end
  end

  defp expand_sep(d, ds, n) do
    union(
      fit(horizontal([d | ds])),
      vertical([
        d
        | for d1 <- ds do
            nest(n, d1)
          end
      ])
    )
  end

  defp expand_par(d, [d1 | ds] = dL, n) do
    union(
      beside(
        fit(d),
        beside(hspace(), mksep([fit(d1) | ds], n - 1, true))
      ),
      above(d, nest(n, par(dL)))
    )
  end

  defp horizontal(ds) do
    foldr1(
      fn d1, d2 ->
        beside(d1, beside(hspace(), d2))
      end,
      ds
    )
  end

  defp vertical(ds) do
    foldr1(&above/2, ds)
  end

  defp foldr1(_F, [h]) do
    h
  end

  defp foldr1(f, [h | t]) do
    f.(h, foldr1(f, t))
  end

  defp string(s) do
    [strwidth(s) | s]
  end

  defp null_string(s) do
    [0 | s]
  end

  defp concat([_], [_ | _] = s) do
    s
  end

  defp concat([_ | _] = s, [_]) do
    s
  end

  defp concat([l1 | s1], [l2 | s2]) do
    [l1 + l2, s1 | s2]
  end

  defp string_chars([_ | s]) do
    s
  end

  defp width(s) do
    hd(s)
  end

  defp is_empty_string([_]) do
    true
  end

  defp is_empty_string([_ | _]) do
    false
  end

  defp strwidth(s) do
    strwidth(s, 0)
  end

  defp strwidth([?\t | cs], n) do
    strwidth(cs, n - rem(n, 8) + 8)
  end

  defp strwidth([_ | cs], n) do
    strwidth(cs, n + 1)
  end

  defp strwidth([], n) do
    n
  end
end
