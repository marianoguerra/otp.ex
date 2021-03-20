defmodule :cowboy_bstr do
  use Bitwise
  def capitalize_token(b) do
    capitalize_token(b, true, <<>>)
  end

  defp capitalize_token(<<>>, _, acc) do
    acc
  end

  defp capitalize_token(<<?-, rest :: bits>>, _, acc) do
    capitalize_token(rest, true, <<acc :: binary, ?->>)
  end

  defp capitalize_token(<<c, rest :: bits>>, true, acc) do
    capitalize_token(rest, false,
                       <<acc :: binary, char_to_upper(c)>>)
  end

  defp capitalize_token(<<c, rest :: bits>>, false, acc) do
    capitalize_token(rest, false,
                       <<acc :: binary, char_to_lower(c)>>)
  end

  def to_lower(b) do
    for << <<c>> <- b >>, into: <<>> do
      <<char_to_lower(c)>>
    end
  end

  def to_upper(b) do
    for << <<c>> <- b >>, into: <<>> do
      <<char_to_upper(c)>>
    end
  end

  def char_to_lower(?A) do
    ?a
  end

  def char_to_lower(?B) do
    ?b
  end

  def char_to_lower(?C) do
    ?c
  end

  def char_to_lower(?D) do
    ?d
  end

  def char_to_lower(?E) do
    ?e
  end

  def char_to_lower(?F) do
    ?f
  end

  def char_to_lower(?G) do
    ?g
  end

  def char_to_lower(?H) do
    ?h
  end

  def char_to_lower(?I) do
    ?i
  end

  def char_to_lower(?J) do
    ?j
  end

  def char_to_lower(?K) do
    ?k
  end

  def char_to_lower(?L) do
    ?l
  end

  def char_to_lower(?M) do
    ?m
  end

  def char_to_lower(?N) do
    ?n
  end

  def char_to_lower(?O) do
    ?o
  end

  def char_to_lower(?P) do
    ?p
  end

  def char_to_lower(?Q) do
    ?q
  end

  def char_to_lower(?R) do
    ?r
  end

  def char_to_lower(?S) do
    ?s
  end

  def char_to_lower(?T) do
    ?t
  end

  def char_to_lower(?U) do
    ?u
  end

  def char_to_lower(?V) do
    ?v
  end

  def char_to_lower(?W) do
    ?w
  end

  def char_to_lower(?X) do
    ?x
  end

  def char_to_lower(?Y) do
    ?y
  end

  def char_to_lower(?Z) do
    ?z
  end

  def char_to_lower(ch) do
    ch
  end

  def char_to_upper(?a) do
    ?A
  end

  def char_to_upper(?b) do
    ?B
  end

  def char_to_upper(?c) do
    ?C
  end

  def char_to_upper(?d) do
    ?D
  end

  def char_to_upper(?e) do
    ?E
  end

  def char_to_upper(?f) do
    ?F
  end

  def char_to_upper(?g) do
    ?G
  end

  def char_to_upper(?h) do
    ?H
  end

  def char_to_upper(?i) do
    ?I
  end

  def char_to_upper(?j) do
    ?J
  end

  def char_to_upper(?k) do
    ?K
  end

  def char_to_upper(?l) do
    ?L
  end

  def char_to_upper(?m) do
    ?M
  end

  def char_to_upper(?n) do
    ?N
  end

  def char_to_upper(?o) do
    ?O
  end

  def char_to_upper(?p) do
    ?P
  end

  def char_to_upper(?q) do
    ?Q
  end

  def char_to_upper(?r) do
    ?R
  end

  def char_to_upper(?s) do
    ?S
  end

  def char_to_upper(?t) do
    ?T
  end

  def char_to_upper(?u) do
    ?U
  end

  def char_to_upper(?v) do
    ?V
  end

  def char_to_upper(?w) do
    ?W
  end

  def char_to_upper(?x) do
    ?X
  end

  def char_to_upper(?y) do
    ?Y
  end

  def char_to_upper(?z) do
    ?Z
  end

  def char_to_upper(ch) do
    ch
  end

end