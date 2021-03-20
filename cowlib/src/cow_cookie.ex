defmodule :cow_cookie do
  use Bitwise
  def parse_cookie(cookie) do
    parse_cookie(cookie, [])
  end

  defp parse_cookie(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp parse_cookie(<<?\s, rest :: binary>>, acc) do
    parse_cookie(rest, acc)
  end

  defp parse_cookie(<<?\t, rest :: binary>>, acc) do
    parse_cookie(rest, acc)
  end

  defp parse_cookie(<<?,, rest :: binary>>, acc) do
    parse_cookie(rest, acc)
  end

  defp parse_cookie(<<?;, rest :: binary>>, acc) do
    parse_cookie(rest, acc)
  end

  defp parse_cookie(cookie, acc) do
    parse_cookie_name(cookie, acc, <<>>)
  end

  defp parse_cookie_name(<<>>, acc, name) do
    :lists.reverse([{<<>>, parse_cookie_trim(name)} | acc])
  end

  defp parse_cookie_name(<<?=, _ :: binary>>, _, <<>>) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<?=, rest :: binary>>, acc, name) do
    parse_cookie_value(rest, acc, name, <<>>)
  end

  defp parse_cookie_name(<<?,, _ :: binary>>, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<?;, rest :: binary>>, acc, name) do
    parse_cookie(rest,
                   [{<<>>, parse_cookie_trim(name)} | acc])
  end

  defp parse_cookie_name(<<?\t, _ :: binary>>, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<?\r, _ :: binary>>, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<?\n, _ :: binary>>, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<?\v, _ :: binary>>, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<?\f, _ :: binary>>, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_name(<<c, rest :: binary>>, acc, name) do
    parse_cookie_name(rest, acc, <<name :: binary, c>>)
  end

  defp parse_cookie_value(<<>>, acc, name, value) do
    :lists.reverse([{name, parse_cookie_trim(value)} | acc])
  end

  defp parse_cookie_value(<<?;, rest :: binary>>, acc, name, value) do
    parse_cookie(rest,
                   [{name, parse_cookie_trim(value)} | acc])
  end

  defp parse_cookie_value(<<?\t, _ :: binary>>, _, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_value(<<?\r, _ :: binary>>, _, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_value(<<?\n, _ :: binary>>, _, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_value(<<?\v, _ :: binary>>, _, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_value(<<?\f, _ :: binary>>, _, _, _) do
    :erlang.error(:badarg)
  end

  defp parse_cookie_value(<<c, rest :: binary>>, acc, name, value) do
    parse_cookie_value(rest, acc, name,
                         <<value :: binary, c>>)
  end

  defp parse_cookie_trim(value = <<>>) do
    value
  end

  defp parse_cookie_trim(value) do
    case (:binary.last(value)) do
      ?\s ->
        size = byte_size(value) - 1
        <<value2 :: size(size) - binary, _>> = value
        parse_cookie_trim(value2)
      _ ->
        value
    end
  end

  def parse_set_cookie(setCookie) do
    {nameValuePair,
       unparsedAttrs} = take_until_semicolon(setCookie, <<>>)
    {name, value} = (case (:binary.split(nameValuePair,
                                           <<?=>>)) do
                       [value0] ->
                         {<<>>, trim(value0)}
                       [name0, value0] ->
                         {trim(name0), trim(value0)}
                     end)
    case ({name, value}) do
      {<<>>, <<>>} ->
        :ignore
      _ ->
        attrs = parse_set_cookie_attrs(unparsedAttrs, %{})
        {:ok, name, value, attrs}
    end
  end

  defp parse_set_cookie_attrs(<<>>, attrs) do
    attrs
  end

  defp parse_set_cookie_attrs(<<?;, rest0 :: bits>>, attrs) do
    {av, rest} = take_until_semicolon(rest0, <<>>)
    {name, value} = (case (:binary.split(av, <<?=>>)) do
                       [name0] ->
                         {trim(name0), <<>>}
                       [name0, value0] ->
                         {trim(name0), trim(value0)}
                     end)
    case (parse_set_cookie_attr(for << <<c>> <- name >>, into: <<>> do
                                  <<case (c) do
                                      ?A ->
                                        ?a
                                      ?B ->
                                        ?b
                                      ?C ->
                                        ?c
                                      ?D ->
                                        ?d
                                      ?E ->
                                        ?e
                                      ?F ->
                                        ?f
                                      ?G ->
                                        ?g
                                      ?H ->
                                        ?h
                                      ?I ->
                                        ?i
                                      ?J ->
                                        ?j
                                      ?K ->
                                        ?k
                                      ?L ->
                                        ?l
                                      ?M ->
                                        ?m
                                      ?N ->
                                        ?n
                                      ?O ->
                                        ?o
                                      ?P ->
                                        ?p
                                      ?Q ->
                                        ?q
                                      ?R ->
                                        ?r
                                      ?S ->
                                        ?s
                                      ?T ->
                                        ?t
                                      ?U ->
                                        ?u
                                      ?V ->
                                        ?v
                                      ?W ->
                                        ?w
                                      ?X ->
                                        ?x
                                      ?Y ->
                                        ?y
                                      ?Z ->
                                        ?z
                                      _ ->
                                        c
                                    end>>
                                end,
                                  value)) do
      {:ok, attrName, attrValue} ->
        parse_set_cookie_attrs(rest,
                                 Map.put(attrs, attrName, attrValue))
      {:ignore, attrName} ->
        parse_set_cookie_attrs(rest,
                                 :maps.remove(attrName, attrs))
      :ignore ->
        parse_set_cookie_attrs(rest, attrs)
    end
  end

  defp take_until_semicolon(rest = <<?;, _ :: bits>>, acc) do
    {acc, rest}
  end

  defp take_until_semicolon(<<c, r :: bits>>, acc) do
    take_until_semicolon(r, <<acc :: binary, c>>)
  end

  defp take_until_semicolon(<<>>, acc) do
    {acc, <<>>}
  end

  defp trim(string) do
    :string.trim(string, :both, [?\s, ?\t])
  end

  defp parse_set_cookie_attr("expires", value) do
    try do
      :cow_date.parse_date(value)
    catch
      _, _ ->
        :ignore
    else
      dateTime ->
        {:ok, :expires, dateTime}
    end
  end

  defp parse_set_cookie_attr("max-age", value) do
    try do
      :erlang.binary_to_integer(value)
    catch
      _, _ ->
        :ignore
    else
      maxAge when maxAge <= 0 ->
        {:ok, :max_age, {{0, 1, 1}, {0, 0, 0}}}
      maxAge ->
        currentTime = :erlang.universaltime()
        {:ok, :max_age,
           :calendar.gregorian_seconds_to_datetime(:calendar.datetime_to_gregorian_seconds(currentTime) + maxAge)}
    end
  end

  defp parse_set_cookie_attr("domain", value) do
    case (value) do
      <<>> ->
        {:ignore, :domain}
      <<".", rest :: bits>> ->
        {:ok, :domain,
           for << <<c>> <- rest >>, into: <<>> do
             <<case (c) do
                 ?A ->
                   ?a
                 ?B ->
                   ?b
                 ?C ->
                   ?c
                 ?D ->
                   ?d
                 ?E ->
                   ?e
                 ?F ->
                   ?f
                 ?G ->
                   ?g
                 ?H ->
                   ?h
                 ?I ->
                   ?i
                 ?J ->
                   ?j
                 ?K ->
                   ?k
                 ?L ->
                   ?l
                 ?M ->
                   ?m
                 ?N ->
                   ?n
                 ?O ->
                   ?o
                 ?P ->
                   ?p
                 ?Q ->
                   ?q
                 ?R ->
                   ?r
                 ?S ->
                   ?s
                 ?T ->
                   ?t
                 ?U ->
                   ?u
                 ?V ->
                   ?v
                 ?W ->
                   ?w
                 ?X ->
                   ?x
                 ?Y ->
                   ?y
                 ?Z ->
                   ?z
                 _ ->
                   c
               end>>
           end}
      _ ->
        {:ok, :domain,
           for << <<c>> <- value >>, into: <<>> do
             <<case (c) do
                 ?A ->
                   ?a
                 ?B ->
                   ?b
                 ?C ->
                   ?c
                 ?D ->
                   ?d
                 ?E ->
                   ?e
                 ?F ->
                   ?f
                 ?G ->
                   ?g
                 ?H ->
                   ?h
                 ?I ->
                   ?i
                 ?J ->
                   ?j
                 ?K ->
                   ?k
                 ?L ->
                   ?l
                 ?M ->
                   ?m
                 ?N ->
                   ?n
                 ?O ->
                   ?o
                 ?P ->
                   ?p
                 ?Q ->
                   ?q
                 ?R ->
                   ?r
                 ?S ->
                   ?s
                 ?T ->
                   ?t
                 ?U ->
                   ?u
                 ?V ->
                   ?v
                 ?W ->
                   ?w
                 ?X ->
                   ?x
                 ?Y ->
                   ?y
                 ?Z ->
                   ?z
                 _ ->
                   c
               end>>
           end}
    end
  end

  defp parse_set_cookie_attr("path", value) do
    case (value) do
      <<"/", _ :: bits>> ->
        {:ok, :path, value}
      _ ->
        {:ignore, :path}
    end
  end

  defp parse_set_cookie_attr("secure", _) do
    {:ok, :secure, true}
  end

  defp parse_set_cookie_attr("httponly", _) do
    {:ok, :http_only, true}
  end

  defp parse_set_cookie_attr("samesite", value) do
    case (for << <<c>> <- value >>, into: <<>> do
            <<case (c) do
                ?A ->
                  ?a
                ?B ->
                  ?b
                ?C ->
                  ?c
                ?D ->
                  ?d
                ?E ->
                  ?e
                ?F ->
                  ?f
                ?G ->
                  ?g
                ?H ->
                  ?h
                ?I ->
                  ?i
                ?J ->
                  ?j
                ?K ->
                  ?k
                ?L ->
                  ?l
                ?M ->
                  ?m
                ?N ->
                  ?n
                ?O ->
                  ?o
                ?P ->
                  ?p
                ?Q ->
                  ?q
                ?R ->
                  ?r
                ?S ->
                  ?s
                ?T ->
                  ?t
                ?U ->
                  ?u
                ?V ->
                  ?v
                ?W ->
                  ?w
                ?X ->
                  ?x
                ?Y ->
                  ?y
                ?Z ->
                  ?z
                _ ->
                  c
              end>>
          end) do
      "strict" ->
        {:ok, :same_site, :strict}
      "lax" ->
        {:ok, :same_site, :lax}
      "none" ->
        {:ok, :same_site, :none}
      _ ->
        :ignore
    end
  end

  defp parse_set_cookie_attr(_, _) do
    :ignore
  end

  def cookie([]) do
    []
  end

  def cookie([{<<>>, value}]) do
    [value]
  end

  def cookie([{name, value}]) do
    [name, ?=, value]
  end

  def cookie([{<<>>, value} | tail]) do
    [value, ?;, ?\s | cookie(tail)]
  end

  def cookie([{name, value} | tail]) do
    [name, ?=, value, ?;, ?\s | cookie(tail)]
  end

  def setcookie(name, value, opts) do
    :nomatch = :binary.match(:erlang.iolist_to_binary(name),
                               [<<?=>>, <<?,>>, <<?;>>, <<?\s>>, <<?\t>>,
                                                                     <<?\r>>,
                                                                         <<?\n>>,
                                                                             <<?\v>>,
                                                                                 <<?\f>>])
    :nomatch = :binary.match(:erlang.iolist_to_binary(value),
                               [<<?,>>, <<?;>>, <<?\s>>, <<?\t>>, <<?\r>>,
                                                                      <<?\n>>,
                                                                          <<?\v>>,
                                                                              <<?\f>>])
    [name, "=", value, "; Version=1", attributes(:maps.to_list(opts))]
  end

  defp attributes([]) do
    []
  end

  defp attributes([{:domain, domain} | tail]) do
    ["; Domain=", domain | attributes(tail)]
  end

  defp attributes([{:http_only, false} | tail]) do
    attributes(tail)
  end

  defp attributes([{:http_only, true} | tail]) do
    ["; HttpOnly" | attributes(tail)]
  end

  defp attributes([{:max_age, 0} | tail]) do
    ["; Expires=Thu, 01-Jan-1970 00:00:01 GMT; Max-Age=0" | attributes(tail)]
  end

  defp attributes([{:max_age, maxAge} | tail])
      when (is_integer(maxAge) and maxAge > 0) do
    secs = :calendar.datetime_to_gregorian_seconds(:calendar.universal_time())
    expires = :cow_date.rfc2109(:calendar.gregorian_seconds_to_datetime(secs + maxAge))
    ["; Expires=", expires, "; Max-Age=", :erlang.integer_to_list(maxAge) |
                        attributes(tail)]
  end

  defp attributes([opt = {:max_age, _} | _]) do
    :erlang.error({:badarg, opt})
  end

  defp attributes([{:path, path} | tail]) do
    ["; Path=", path | attributes(tail)]
  end

  defp attributes([{:secure, false} | tail]) do
    attributes(tail)
  end

  defp attributes([{:secure, true} | tail]) do
    ["; Secure" | attributes(tail)]
  end

  defp attributes([{:same_site, :lax} | tail]) do
    ["; SameSite=Lax" | attributes(tail)]
  end

  defp attributes([{:same_site, :strict} | tail]) do
    ["; SameSite=Strict" | attributes(tail)]
  end

  defp attributes([{:same_site, :none} | tail]) do
    ["; SameSite=None" | attributes(tail)]
  end

  defp attributes([_ | tail]) do
    attributes(tail)
  end

end