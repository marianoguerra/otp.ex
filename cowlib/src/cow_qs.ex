defmodule :cow_qs do
  use Bitwise
  def parse_qs(b) do
    parse_qs_name(b, [], <<>>)
  end

  defp parse_qs_name(<<?%, h, l, rest :: bits>>, acc, name) do
    c = unhex(h) <<< 4 ||| unhex(l)
    parse_qs_name(rest, acc, <<name :: bits, c>>)
  end

  defp parse_qs_name(<<?+, rest :: bits>>, acc, name) do
    parse_qs_name(rest, acc, <<name :: bits, " ">>)
  end

  defp parse_qs_name(<<?=, rest :: bits>>, acc, name)
      when name !== <<>> do
    parse_qs_value(rest, acc, name, <<>>)
  end

  defp parse_qs_name(<<?&, rest :: bits>>, acc, name) do
    case (name) do
      <<>> ->
        parse_qs_name(rest, acc, <<>>)
      _ ->
        parse_qs_name(rest, [{name, true} | acc], <<>>)
    end
  end

  defp parse_qs_name(<<c, rest :: bits>>, acc, name)
      when (c !== ?% and c !== ?=) do
    parse_qs_name(rest, acc, <<name :: bits, c>>)
  end

  defp parse_qs_name(<<>>, acc, name) do
    case (name) do
      <<>> ->
        :lists.reverse(acc)
      _ ->
        :lists.reverse([{name, true} | acc])
    end
  end

  defp parse_qs_value(<<?%, h, l, rest :: bits>>, acc, name, value) do
    c = unhex(h) <<< 4 ||| unhex(l)
    parse_qs_value(rest, acc, name, <<value :: bits, c>>)
  end

  defp parse_qs_value(<<?+, rest :: bits>>, acc, name, value) do
    parse_qs_value(rest, acc, name, <<value :: bits, " ">>)
  end

  defp parse_qs_value(<<?&, rest :: bits>>, acc, name, value) do
    parse_qs_name(rest, [{name, value} | acc], <<>>)
  end

  defp parse_qs_value(<<c, rest :: bits>>, acc, name, value)
      when c !== ?% do
    parse_qs_value(rest, acc, name, <<value :: bits, c>>)
  end

  defp parse_qs_value(<<>>, acc, name, value) do
    :lists.reverse([{name, value} | acc])
  end

  def qs([]) do
    <<>>
  end

  def qs(l) do
    qs(l, <<>>)
  end

  defp qs([], acc) do
    <<?&, qs :: bits>> = acc
    qs
  end

  defp qs([{name, true} | tail], acc) do
    acc2 = urlencode(name, <<acc :: bits, ?&>>)
    qs(tail, acc2)
  end

  defp qs([{name, value} | tail], acc) do
    acc2 = urlencode(name, <<acc :: bits, ?&>>)
    acc3 = urlencode(value, <<acc2 :: bits, ?=>>)
    qs(tail, acc3)
  end

  def urldecode(b) do
    urldecode(b, <<>>)
  end

  defp urldecode(<<?%, h, l, rest :: bits>>, acc) do
    c = unhex(h) <<< 4 ||| unhex(l)
    urldecode(rest, <<acc :: bits, c>>)
  end

  defp urldecode(<<?+, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, " ">>)
  end

  defp urldecode(<<c, rest :: bits>>, acc) when c !== ?% do
    urldecode(rest, <<acc :: bits, c>>)
  end

  defp urldecode(<<>>, acc) do
    acc
  end

  defp unhex(?0) do
    0
  end

  defp unhex(?1) do
    1
  end

  defp unhex(?2) do
    2
  end

  defp unhex(?3) do
    3
  end

  defp unhex(?4) do
    4
  end

  defp unhex(?5) do
    5
  end

  defp unhex(?6) do
    6
  end

  defp unhex(?7) do
    7
  end

  defp unhex(?8) do
    8
  end

  defp unhex(?9) do
    9
  end

  defp unhex(?A) do
    10
  end

  defp unhex(?B) do
    11
  end

  defp unhex(?C) do
    12
  end

  defp unhex(?D) do
    13
  end

  defp unhex(?E) do
    14
  end

  defp unhex(?F) do
    15
  end

  defp unhex(?a) do
    10
  end

  defp unhex(?b) do
    11
  end

  defp unhex(?c) do
    12
  end

  defp unhex(?d) do
    13
  end

  defp unhex(?e) do
    14
  end

  defp unhex(?f) do
    15
  end

  def urlencode(b) do
    urlencode(b, <<>>)
  end

  defp urlencode(<<?\s, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?+>>)
  end

  defp urlencode(<<?-, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?->>)
  end

  defp urlencode(<<?., rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?.>>)
  end

  defp urlencode(<<?0, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?0>>)
  end

  defp urlencode(<<?1, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?1>>)
  end

  defp urlencode(<<?2, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?2>>)
  end

  defp urlencode(<<?3, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?3>>)
  end

  defp urlencode(<<?4, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?4>>)
  end

  defp urlencode(<<?5, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?5>>)
  end

  defp urlencode(<<?6, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?6>>)
  end

  defp urlencode(<<?7, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?7>>)
  end

  defp urlencode(<<?8, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?8>>)
  end

  defp urlencode(<<?9, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?9>>)
  end

  defp urlencode(<<?A, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?A>>)
  end

  defp urlencode(<<?B, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?B>>)
  end

  defp urlencode(<<?C, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?C>>)
  end

  defp urlencode(<<?D, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?D>>)
  end

  defp urlencode(<<?E, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?E>>)
  end

  defp urlencode(<<?F, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?F>>)
  end

  defp urlencode(<<?G, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?G>>)
  end

  defp urlencode(<<?H, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?H>>)
  end

  defp urlencode(<<?I, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?I>>)
  end

  defp urlencode(<<?J, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?J>>)
  end

  defp urlencode(<<?K, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?K>>)
  end

  defp urlencode(<<?L, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?L>>)
  end

  defp urlencode(<<?M, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?M>>)
  end

  defp urlencode(<<?N, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?N>>)
  end

  defp urlencode(<<?O, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?O>>)
  end

  defp urlencode(<<?P, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?P>>)
  end

  defp urlencode(<<?Q, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?Q>>)
  end

  defp urlencode(<<?R, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?R>>)
  end

  defp urlencode(<<?S, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?S>>)
  end

  defp urlencode(<<?T, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?T>>)
  end

  defp urlencode(<<?U, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?U>>)
  end

  defp urlencode(<<?V, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?V>>)
  end

  defp urlencode(<<?W, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?W>>)
  end

  defp urlencode(<<?X, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?X>>)
  end

  defp urlencode(<<?Y, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?Y>>)
  end

  defp urlencode(<<?Z, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?Z>>)
  end

  defp urlencode(<<?_, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?_>>)
  end

  defp urlencode(<<?a, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?a>>)
  end

  defp urlencode(<<?b, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?b>>)
  end

  defp urlencode(<<?c, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?c>>)
  end

  defp urlencode(<<?d, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?d>>)
  end

  defp urlencode(<<?e, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?e>>)
  end

  defp urlencode(<<?f, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?f>>)
  end

  defp urlencode(<<?g, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?g>>)
  end

  defp urlencode(<<?h, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?h>>)
  end

  defp urlencode(<<?i, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?i>>)
  end

  defp urlencode(<<?j, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?j>>)
  end

  defp urlencode(<<?k, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?k>>)
  end

  defp urlencode(<<?l, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?l>>)
  end

  defp urlencode(<<?m, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?m>>)
  end

  defp urlencode(<<?n, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?n>>)
  end

  defp urlencode(<<?o, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?o>>)
  end

  defp urlencode(<<?p, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?p>>)
  end

  defp urlencode(<<?q, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?q>>)
  end

  defp urlencode(<<?r, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?r>>)
  end

  defp urlencode(<<?s, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?s>>)
  end

  defp urlencode(<<?t, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?t>>)
  end

  defp urlencode(<<?u, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?u>>)
  end

  defp urlencode(<<?v, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?v>>)
  end

  defp urlencode(<<?w, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?w>>)
  end

  defp urlencode(<<?x, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?x>>)
  end

  defp urlencode(<<?y, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?y>>)
  end

  defp urlencode(<<?z, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?z>>)
  end

  defp urlencode(<<c, rest :: bits>>, acc) do
    h = hex(c >>> 4)
    l = hex(c &&& 15)
    urlencode(rest, <<acc :: bits, ?%, h, l>>)
  end

  defp urlencode(<<>>, acc) do
    acc
  end

  defp hex(0) do
    ?0
  end

  defp hex(1) do
    ?1
  end

  defp hex(2) do
    ?2
  end

  defp hex(3) do
    ?3
  end

  defp hex(4) do
    ?4
  end

  defp hex(5) do
    ?5
  end

  defp hex(6) do
    ?6
  end

  defp hex(7) do
    ?7
  end

  defp hex(8) do
    ?8
  end

  defp hex(9) do
    ?9
  end

  defp hex(10) do
    ?A
  end

  defp hex(11) do
    ?B
  end

  defp hex(12) do
    ?C
  end

  defp hex(13) do
    ?D
  end

  defp hex(14) do
    ?E
  end

  defp hex(15) do
    ?F
  end

end