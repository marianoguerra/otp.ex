defmodule :cow_uri do
  use Bitwise
  def urldecode(b) do
    urldecode(b, <<>>)
  end

  defp urldecode(<<?%, h, l, rest :: bits>>, acc) do
    c = unhex(h) <<< 4 ||| unhex(l)
    urldecode(rest, <<acc :: bits, c>>)
  end

  defp urldecode(<<?!, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?!>>)
  end

  defp urldecode(<<?$, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?$>>)
  end

  defp urldecode(<<?&, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?&>>)
  end

  defp urldecode(<<?', rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?'>>)
  end

  defp urldecode(<<?(, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?(>>)
  end

  defp urldecode(<<?), rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?)>>)
  end

  defp urldecode(<<?*, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?*>>)
  end

  defp urldecode(<<?+, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?+>>)
  end

  defp urldecode(<<?,, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?,>>)
  end

  defp urldecode(<<?-, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?->>)
  end

  defp urldecode(<<?., rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?.>>)
  end

  defp urldecode(<<?0, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?0>>)
  end

  defp urldecode(<<?1, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?1>>)
  end

  defp urldecode(<<?2, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?2>>)
  end

  defp urldecode(<<?3, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?3>>)
  end

  defp urldecode(<<?4, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?4>>)
  end

  defp urldecode(<<?5, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?5>>)
  end

  defp urldecode(<<?6, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?6>>)
  end

  defp urldecode(<<?7, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?7>>)
  end

  defp urldecode(<<?8, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?8>>)
  end

  defp urldecode(<<?9, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?9>>)
  end

  defp urldecode(<<?:, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?:>>)
  end

  defp urldecode(<<?;, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?;>>)
  end

  defp urldecode(<<?=, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?=>>)
  end

  defp urldecode(<<?@, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?@>>)
  end

  defp urldecode(<<?A, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?A>>)
  end

  defp urldecode(<<?B, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?B>>)
  end

  defp urldecode(<<?C, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?C>>)
  end

  defp urldecode(<<?D, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?D>>)
  end

  defp urldecode(<<?E, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?E>>)
  end

  defp urldecode(<<?F, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?F>>)
  end

  defp urldecode(<<?G, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?G>>)
  end

  defp urldecode(<<?H, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?H>>)
  end

  defp urldecode(<<?I, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?I>>)
  end

  defp urldecode(<<?J, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?J>>)
  end

  defp urldecode(<<?K, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?K>>)
  end

  defp urldecode(<<?L, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?L>>)
  end

  defp urldecode(<<?M, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?M>>)
  end

  defp urldecode(<<?N, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?N>>)
  end

  defp urldecode(<<?O, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?O>>)
  end

  defp urldecode(<<?P, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?P>>)
  end

  defp urldecode(<<?Q, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?Q>>)
  end

  defp urldecode(<<?R, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?R>>)
  end

  defp urldecode(<<?S, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?S>>)
  end

  defp urldecode(<<?T, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?T>>)
  end

  defp urldecode(<<?U, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?U>>)
  end

  defp urldecode(<<?V, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?V>>)
  end

  defp urldecode(<<?W, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?W>>)
  end

  defp urldecode(<<?X, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?X>>)
  end

  defp urldecode(<<?Y, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?Y>>)
  end

  defp urldecode(<<?Z, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?Z>>)
  end

  defp urldecode(<<?_, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?_>>)
  end

  defp urldecode(<<?a, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?a>>)
  end

  defp urldecode(<<?b, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?b>>)
  end

  defp urldecode(<<?c, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?c>>)
  end

  defp urldecode(<<?d, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?d>>)
  end

  defp urldecode(<<?e, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?e>>)
  end

  defp urldecode(<<?f, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?f>>)
  end

  defp urldecode(<<?g, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?g>>)
  end

  defp urldecode(<<?h, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?h>>)
  end

  defp urldecode(<<?i, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?i>>)
  end

  defp urldecode(<<?j, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?j>>)
  end

  defp urldecode(<<?k, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?k>>)
  end

  defp urldecode(<<?l, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?l>>)
  end

  defp urldecode(<<?m, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?m>>)
  end

  defp urldecode(<<?n, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?n>>)
  end

  defp urldecode(<<?o, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?o>>)
  end

  defp urldecode(<<?p, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?p>>)
  end

  defp urldecode(<<?q, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?q>>)
  end

  defp urldecode(<<?r, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?r>>)
  end

  defp urldecode(<<?s, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?s>>)
  end

  defp urldecode(<<?t, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?t>>)
  end

  defp urldecode(<<?u, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?u>>)
  end

  defp urldecode(<<?v, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?v>>)
  end

  defp urldecode(<<?w, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?w>>)
  end

  defp urldecode(<<?x, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?x>>)
  end

  defp urldecode(<<?y, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?y>>)
  end

  defp urldecode(<<?z, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?z>>)
  end

  defp urldecode(<<?~, rest :: bits>>, acc) do
    urldecode(rest, <<acc :: bits, ?~>>)
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

  defp urlencode(<<?!, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?!>>)
  end

  defp urlencode(<<?$, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?$>>)
  end

  defp urlencode(<<?&, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?&>>)
  end

  defp urlencode(<<?', rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?'>>)
  end

  defp urlencode(<<?(, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?(>>)
  end

  defp urlencode(<<?), rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?)>>)
  end

  defp urlencode(<<?*, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?*>>)
  end

  defp urlencode(<<?+, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?+>>)
  end

  defp urlencode(<<?,, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?,>>)
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

  defp urlencode(<<?:, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?:>>)
  end

  defp urlencode(<<?;, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?;>>)
  end

  defp urlencode(<<?=, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?=>>)
  end

  defp urlencode(<<?@, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?@>>)
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

  defp urlencode(<<?~, rest :: bits>>, acc) do
    urlencode(rest, <<acc :: bits, ?~>>)
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