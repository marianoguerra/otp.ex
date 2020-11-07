defmodule :m_xmerl_b64Bin_scan do
  use Bitwise

  def scan(str) do
    scan(str, [])
  end

  defp scan([], acc) do
    :lists.reverse([{:"$end", 1, :"$end"} | acc])
  end

  defp scan(str, acc) do
    case scan_token(str) do
      {token, rest} ->
        scan(rest, [token | acc])
    end
  end

  defp scan_token([[?\s, h] | t]) do
    scan_token([h | t])
  end

  defp scan_token([h | t])
       when h == ?A or h == ?Q or h == ?g or
              h == ?w do
    {{:b04, 1, h}, t}
  end

  defp scan_token([h | t])
       when h == ?E or h == ?I or h == ?M or
              h == ?U or h == ?Y or h == ?c or h == ?k or h == ?o or
              h == ?s or h == ?0 or h == ?4 or h == ?8 do
    {{:b16x, 1, h}, t}
  end

  defp scan_token([h | t])
       when h == ?B or h == ?C or h == ?D or
              h == ?F or h == ?G or h == ?H or h == ?J or h == ?K or
              h == ?L or h == ?N or h == ?O or h == ?P or h == ?R or
              h == ?S or h == ?T or h == ?V or h == ?W or h == ?X or
              h == ?Z do
    {{:b64x, 1, h}, t}
  end

  defp scan_token([h | t])
       when h == ?a or h == ?b or h == ?d or
              h == ?e or h == ?f or h == ?h or h == ?i or h == ?j or
              h == ?l or h == ?m or h == ?n or h == ?p or h == ?q or
              h == ?r or h == ?t or h == ?u or h == ?v or h == ?x or
              h == ?y or h == ?z do
    {{:b64x, 1, h}, t}
  end

  defp scan_token([h | t])
       when h == ?1 or h == ?2 or h == ?3 or
              h == ?5 or h == ?6 or h == ?7 or h == ?9 or h == ?+ or
              h == ?/ do
    {{:b64x, 1, h}, t}
  end

  defp scan_token('=' ++ t) do
    {{:=, 1, '='}, t}
  end

  defp scan_token([h | _T]) do
    exit({:error, {:base64Binary_scan_illegal_char, h}})
  end
end
