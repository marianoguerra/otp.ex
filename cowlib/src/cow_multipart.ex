defmodule :cow_multipart do
  use Bitwise
  def parse_headers(<<"--", stream :: bits>>, boundary) do
    boundarySize = byte_size(boundary)
    case (stream) do
      <<^boundary :: size(boundarySize) - binary, "--",
          stream2 :: bits>> ->
        {:done, stream2}
      <<^boundary :: size(boundarySize) - binary,
          stream2 :: bits>> ->
        case (:binary.match(stream2, "\r\n\r\n")) do
          :nomatch ->
            :more
          _ ->
            before_parse_headers(stream2)
        end
      _ when byte_size(stream) < byte_size(boundary) + 4 ->
        :more
      _ ->
        skip_preamble(stream, boundary)
    end
  end

  def parse_headers(stream, boundary) do
    skip_preamble(stream, boundary)
  end

  defp skip_preamble(stream, boundary) do
    case (:binary.match(stream, <<"\r\n--", boundary :: bits>>)) do
      :nomatch ->
        skipSize = byte_size(stream) - 3
        case (skipSize > 0) do
          false ->
            :more
          true ->
            <<_ :: size(skipSize) - binary,
                stream2 :: bits>> = stream
            {:more, stream2}
        end
      {start, length} ->
        start2 = start + length
        <<_ :: size(start2) - binary, stream2 :: bits>> = stream
        case (stream2) do
          <<"--", stream3 :: bits>> ->
            {:done, stream3}
          _ ->
            case (:binary.match(stream, "\r\n\r\n")) do
              :nomatch ->
                {:more, stream2}
              _ ->
                before_parse_headers(stream2)
            end
        end
    end
  end

  defp before_parse_headers(<<"\r\n\r\n", stream :: bits>>) do
    {:ok, [], stream}
  end

  defp before_parse_headers(<<"\r\n", stream :: bits>>) do
    parse_hd_name(stream, [], <<>>)
  end

  defp parse_hd_name(<<c, rest :: bits>>, h, soFar) do
    case (c) do
      ?: ->
        parse_hd_before_value(rest, h, soFar)
      ?\s ->
        parse_hd_name_ws(rest, h, soFar)
      ?\t ->
        parse_hd_name_ws(rest, h, soFar)
      _ ->
        case (c) do
          ?A ->
            parse_hd_name(rest, h, <<soFar :: binary, ?a>>)
          ?B ->
            parse_hd_name(rest, h, <<soFar :: binary, ?b>>)
          ?C ->
            parse_hd_name(rest, h, <<soFar :: binary, ?c>>)
          ?D ->
            parse_hd_name(rest, h, <<soFar :: binary, ?d>>)
          ?E ->
            parse_hd_name(rest, h, <<soFar :: binary, ?e>>)
          ?F ->
            parse_hd_name(rest, h, <<soFar :: binary, ?f>>)
          ?G ->
            parse_hd_name(rest, h, <<soFar :: binary, ?g>>)
          ?H ->
            parse_hd_name(rest, h, <<soFar :: binary, ?h>>)
          ?I ->
            parse_hd_name(rest, h, <<soFar :: binary, ?i>>)
          ?J ->
            parse_hd_name(rest, h, <<soFar :: binary, ?j>>)
          ?K ->
            parse_hd_name(rest, h, <<soFar :: binary, ?k>>)
          ?L ->
            parse_hd_name(rest, h, <<soFar :: binary, ?l>>)
          ?M ->
            parse_hd_name(rest, h, <<soFar :: binary, ?m>>)
          ?N ->
            parse_hd_name(rest, h, <<soFar :: binary, ?n>>)
          ?O ->
            parse_hd_name(rest, h, <<soFar :: binary, ?o>>)
          ?P ->
            parse_hd_name(rest, h, <<soFar :: binary, ?p>>)
          ?Q ->
            parse_hd_name(rest, h, <<soFar :: binary, ?q>>)
          ?R ->
            parse_hd_name(rest, h, <<soFar :: binary, ?r>>)
          ?S ->
            parse_hd_name(rest, h, <<soFar :: binary, ?s>>)
          ?T ->
            parse_hd_name(rest, h, <<soFar :: binary, ?t>>)
          ?U ->
            parse_hd_name(rest, h, <<soFar :: binary, ?u>>)
          ?V ->
            parse_hd_name(rest, h, <<soFar :: binary, ?v>>)
          ?W ->
            parse_hd_name(rest, h, <<soFar :: binary, ?w>>)
          ?X ->
            parse_hd_name(rest, h, <<soFar :: binary, ?x>>)
          ?Y ->
            parse_hd_name(rest, h, <<soFar :: binary, ?y>>)
          ?Z ->
            parse_hd_name(rest, h, <<soFar :: binary, ?z>>)
          ^c ->
            parse_hd_name(rest, h, <<soFar :: binary, c>>)
        end
    end
  end

  defp parse_hd_name_ws(<<c, rest :: bits>>, h, name) do
    case (c) do
      ?\s ->
        parse_hd_name_ws(rest, h, name)
      ?\t ->
        parse_hd_name_ws(rest, h, name)
      ?: ->
        parse_hd_before_value(rest, h, name)
    end
  end

  defp parse_hd_before_value(<<?\s, rest :: bits>>, h, n) do
    parse_hd_before_value(rest, h, n)
  end

  defp parse_hd_before_value(<<?\t, rest :: bits>>, h, n) do
    parse_hd_before_value(rest, h, n)
  end

  defp parse_hd_before_value(buffer, h, n) do
    parse_hd_value(buffer, h, n, <<>>)
  end

  defp parse_hd_value(<<?\r, rest :: bits>>, headers, name, soFar) do
    case (rest) do
      <<"\n\r\n", rest2 :: bits>> ->
        {:ok, [{name, soFar} | headers], rest2}
      <<?\n, c, rest2 :: bits>> when c === ?\s or c === ?\t ->
        parse_hd_value(rest2, headers, name, soFar)
      <<?\n, rest2 :: bits>> ->
        parse_hd_name(rest2, [{name, soFar} | headers], <<>>)
    end
  end

  defp parse_hd_value(<<c, rest :: bits>>, h, n, soFar) do
    parse_hd_value(rest, h, n, <<soFar :: binary, c>>)
  end

  def parse_body(stream, boundary) do
    boundarySize = byte_size(boundary)
    case (stream) do
      <<"--", ^boundary :: size(boundarySize) - binary,
          _ :: bits>> ->
        :done
      _ ->
        case (:binary.match(stream, <<"\r\n--", boundary :: bits>>)) do
          :nomatch ->
            streamSize = byte_size(stream)
            from = streamSize - boundarySize - 3
            matchOpts = (cond do
                           from < 0 ->
                             []
                           true ->
                             [{:scope, {from, streamSize - from}}]
                         end)
            case (:binary.match(stream, "\r", matchOpts)) do
              :nomatch ->
                {:ok, stream}
              {pos, _} ->
                case (stream) do
                  <<body :: size(pos) - binary>> ->
                    {:ok, body}
                  <<body :: size(pos) - binary, rest :: bits>> ->
                    {:ok, body, rest}
                end
            end
          {pos, _} ->
            case (stream) do
              <<body :: size(pos) - binary, "\r\n">> ->
                {:done, body}
              <<body :: size(pos) - binary, "\r\n", rest :: bits>> ->
                {:done, body, rest}
              <<body :: size(pos) - binary, rest :: bits>> ->
                {:done, body, rest}
            end
        end
    end
  end

  def boundary() do
    :cow_base64url.encode(:crypto.strong_rand_bytes(48),
                            %{padding: false})
  end

  def first_part(boundary, headers) do
    ["--", boundary, "\r\n", headers_to_iolist(headers, [])]
  end

  def part(boundary, headers) do
    ["\r\n--", boundary, "\r\n", headers_to_iolist(headers, [])]
  end

  defp headers_to_iolist([], acc) do
    :lists.reverse(["\r\n" | acc])
  end

  defp headers_to_iolist([{n, v} | tail], acc) do
    headers_to_iolist(tail, ["\r\n", v, ": ", n | acc])
  end

  def close(boundary) do
    ["\r\n--", boundary, "--"]
  end

  def form_data(headers) when is_map(headers) do
    form_data(:maps.to_list(headers))
  end

  def form_data(headers) do
    {_, dispositionBin} = :lists.keyfind("content-disposition", 1, headers)
    {"form-data", params} = parse_content_disposition(dispositionBin)
    {_, fieldName} = :lists.keyfind("name", 1, params)
    case (:lists.keyfind("filename", 1, params)) do
      false ->
        {:data, fieldName}
      {_, filename} ->
        type = (case (:lists.keyfind("content-type", 1, headers)) do
                  false ->
                    "text/plain"
                  {_, t} ->
                    t
                end)
        {:file, fieldName, filename, type}
    end
  end

  def parse_content_disposition(bin) do
    parse_cd_type(bin, <<>>)
  end

  defp parse_cd_type(<<>>, acc) do
    {acc, []}
  end

  defp parse_cd_type(<<c, rest :: bits>>, acc) do
    case (c) do
      ?; ->
        {acc, parse_before_param(rest, [])}
      ?\s ->
        {acc, parse_before_param(rest, [])}
      ?\t ->
        {acc, parse_before_param(rest, [])}
      _ ->
        case (c) do
          ?A ->
            parse_cd_type(rest, <<acc :: binary, ?a>>)
          ?B ->
            parse_cd_type(rest, <<acc :: binary, ?b>>)
          ?C ->
            parse_cd_type(rest, <<acc :: binary, ?c>>)
          ?D ->
            parse_cd_type(rest, <<acc :: binary, ?d>>)
          ?E ->
            parse_cd_type(rest, <<acc :: binary, ?e>>)
          ?F ->
            parse_cd_type(rest, <<acc :: binary, ?f>>)
          ?G ->
            parse_cd_type(rest, <<acc :: binary, ?g>>)
          ?H ->
            parse_cd_type(rest, <<acc :: binary, ?h>>)
          ?I ->
            parse_cd_type(rest, <<acc :: binary, ?i>>)
          ?J ->
            parse_cd_type(rest, <<acc :: binary, ?j>>)
          ?K ->
            parse_cd_type(rest, <<acc :: binary, ?k>>)
          ?L ->
            parse_cd_type(rest, <<acc :: binary, ?l>>)
          ?M ->
            parse_cd_type(rest, <<acc :: binary, ?m>>)
          ?N ->
            parse_cd_type(rest, <<acc :: binary, ?n>>)
          ?O ->
            parse_cd_type(rest, <<acc :: binary, ?o>>)
          ?P ->
            parse_cd_type(rest, <<acc :: binary, ?p>>)
          ?Q ->
            parse_cd_type(rest, <<acc :: binary, ?q>>)
          ?R ->
            parse_cd_type(rest, <<acc :: binary, ?r>>)
          ?S ->
            parse_cd_type(rest, <<acc :: binary, ?s>>)
          ?T ->
            parse_cd_type(rest, <<acc :: binary, ?t>>)
          ?U ->
            parse_cd_type(rest, <<acc :: binary, ?u>>)
          ?V ->
            parse_cd_type(rest, <<acc :: binary, ?v>>)
          ?W ->
            parse_cd_type(rest, <<acc :: binary, ?w>>)
          ?X ->
            parse_cd_type(rest, <<acc :: binary, ?x>>)
          ?Y ->
            parse_cd_type(rest, <<acc :: binary, ?y>>)
          ?Z ->
            parse_cd_type(rest, <<acc :: binary, ?z>>)
          ^c ->
            parse_cd_type(rest, <<acc :: binary, c>>)
        end
    end
  end

  def parse_content_transfer_encoding(bin) do
    for << <<c>> <- bin >>, into: <<>> do
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
    end
  end

  def parse_content_type(bin) do
    parse_ct_type(bin, <<>>)
  end

  defp parse_ct_type(<<c, rest :: bits>>, acc) do
    case (c) do
      ?/ ->
        parse_ct_subtype(rest, acc, <<>>)
      _ ->
        case (c) do
          ?A ->
            parse_ct_type(rest, <<acc :: binary, ?a>>)
          ?B ->
            parse_ct_type(rest, <<acc :: binary, ?b>>)
          ?C ->
            parse_ct_type(rest, <<acc :: binary, ?c>>)
          ?D ->
            parse_ct_type(rest, <<acc :: binary, ?d>>)
          ?E ->
            parse_ct_type(rest, <<acc :: binary, ?e>>)
          ?F ->
            parse_ct_type(rest, <<acc :: binary, ?f>>)
          ?G ->
            parse_ct_type(rest, <<acc :: binary, ?g>>)
          ?H ->
            parse_ct_type(rest, <<acc :: binary, ?h>>)
          ?I ->
            parse_ct_type(rest, <<acc :: binary, ?i>>)
          ?J ->
            parse_ct_type(rest, <<acc :: binary, ?j>>)
          ?K ->
            parse_ct_type(rest, <<acc :: binary, ?k>>)
          ?L ->
            parse_ct_type(rest, <<acc :: binary, ?l>>)
          ?M ->
            parse_ct_type(rest, <<acc :: binary, ?m>>)
          ?N ->
            parse_ct_type(rest, <<acc :: binary, ?n>>)
          ?O ->
            parse_ct_type(rest, <<acc :: binary, ?o>>)
          ?P ->
            parse_ct_type(rest, <<acc :: binary, ?p>>)
          ?Q ->
            parse_ct_type(rest, <<acc :: binary, ?q>>)
          ?R ->
            parse_ct_type(rest, <<acc :: binary, ?r>>)
          ?S ->
            parse_ct_type(rest, <<acc :: binary, ?s>>)
          ?T ->
            parse_ct_type(rest, <<acc :: binary, ?t>>)
          ?U ->
            parse_ct_type(rest, <<acc :: binary, ?u>>)
          ?V ->
            parse_ct_type(rest, <<acc :: binary, ?v>>)
          ?W ->
            parse_ct_type(rest, <<acc :: binary, ?w>>)
          ?X ->
            parse_ct_type(rest, <<acc :: binary, ?x>>)
          ?Y ->
            parse_ct_type(rest, <<acc :: binary, ?y>>)
          ?Z ->
            parse_ct_type(rest, <<acc :: binary, ?z>>)
          ^c ->
            parse_ct_type(rest, <<acc :: binary, c>>)
        end
    end
  end

  defp parse_ct_subtype(<<>>, type, subtype) when subtype !== <<>> do
    {type, subtype, []}
  end

  defp parse_ct_subtype(<<c, rest :: bits>>, type, acc) do
    case (c) do
      ?; ->
        {type, acc, parse_before_param(rest, [])}
      ?\s ->
        {type, acc, parse_before_param(rest, [])}
      ?\t ->
        {type, acc, parse_before_param(rest, [])}
      _ ->
        case (c) do
          ?A ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?a>>)
          ?B ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?b>>)
          ?C ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?c>>)
          ?D ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?d>>)
          ?E ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?e>>)
          ?F ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?f>>)
          ?G ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?g>>)
          ?H ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?h>>)
          ?I ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?i>>)
          ?J ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?j>>)
          ?K ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?k>>)
          ?L ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?l>>)
          ?M ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?m>>)
          ?N ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?n>>)
          ?O ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?o>>)
          ?P ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?p>>)
          ?Q ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?q>>)
          ?R ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?r>>)
          ?S ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?s>>)
          ?T ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?t>>)
          ?U ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?u>>)
          ?V ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?v>>)
          ?W ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?w>>)
          ?X ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?x>>)
          ?Y ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?y>>)
          ?Z ->
            parse_ct_subtype(rest, type, <<acc :: binary, ?z>>)
          ^c ->
            parse_ct_subtype(rest, type, <<acc :: binary, c>>)
        end
    end
  end

  defp parse_before_param(<<>>, params) do
    :lists.reverse(params)
  end

  defp parse_before_param(<<c, rest :: bits>>, params) do
    case (c) do
      ?; ->
        parse_before_param(rest, params)
      ?\s ->
        parse_before_param(rest, params)
      ?\t ->
        parse_before_param(rest, params)
      _ ->
        case (c) do
          ?A ->
            parse_param_name(rest, params, <<<<>> :: binary, ?a>>)
          ?B ->
            parse_param_name(rest, params, <<<<>> :: binary, ?b>>)
          ?C ->
            parse_param_name(rest, params, <<<<>> :: binary, ?c>>)
          ?D ->
            parse_param_name(rest, params, <<<<>> :: binary, ?d>>)
          ?E ->
            parse_param_name(rest, params, <<<<>> :: binary, ?e>>)
          ?F ->
            parse_param_name(rest, params, <<<<>> :: binary, ?f>>)
          ?G ->
            parse_param_name(rest, params, <<<<>> :: binary, ?g>>)
          ?H ->
            parse_param_name(rest, params, <<<<>> :: binary, ?h>>)
          ?I ->
            parse_param_name(rest, params, <<<<>> :: binary, ?i>>)
          ?J ->
            parse_param_name(rest, params, <<<<>> :: binary, ?j>>)
          ?K ->
            parse_param_name(rest, params, <<<<>> :: binary, ?k>>)
          ?L ->
            parse_param_name(rest, params, <<<<>> :: binary, ?l>>)
          ?M ->
            parse_param_name(rest, params, <<<<>> :: binary, ?m>>)
          ?N ->
            parse_param_name(rest, params, <<<<>> :: binary, ?n>>)
          ?O ->
            parse_param_name(rest, params, <<<<>> :: binary, ?o>>)
          ?P ->
            parse_param_name(rest, params, <<<<>> :: binary, ?p>>)
          ?Q ->
            parse_param_name(rest, params, <<<<>> :: binary, ?q>>)
          ?R ->
            parse_param_name(rest, params, <<<<>> :: binary, ?r>>)
          ?S ->
            parse_param_name(rest, params, <<<<>> :: binary, ?s>>)
          ?T ->
            parse_param_name(rest, params, <<<<>> :: binary, ?t>>)
          ?U ->
            parse_param_name(rest, params, <<<<>> :: binary, ?u>>)
          ?V ->
            parse_param_name(rest, params, <<<<>> :: binary, ?v>>)
          ?W ->
            parse_param_name(rest, params, <<<<>> :: binary, ?w>>)
          ?X ->
            parse_param_name(rest, params, <<<<>> :: binary, ?x>>)
          ?Y ->
            parse_param_name(rest, params, <<<<>> :: binary, ?y>>)
          ?Z ->
            parse_param_name(rest, params, <<<<>> :: binary, ?z>>)
          ^c ->
            parse_param_name(rest, params, <<<<>> :: binary, c>>)
        end
    end
  end

  defp parse_param_name(<<>>, params, acc) do
    :lists.reverse([{acc, <<>>} | params])
  end

  defp parse_param_name(<<c, rest :: bits>>, params, acc) do
    case (c) do
      ?= ->
        parse_param_value(rest, params, acc)
      _ ->
        case (c) do
          ?A ->
            parse_param_name(rest, params, <<acc :: binary, ?a>>)
          ?B ->
            parse_param_name(rest, params, <<acc :: binary, ?b>>)
          ?C ->
            parse_param_name(rest, params, <<acc :: binary, ?c>>)
          ?D ->
            parse_param_name(rest, params, <<acc :: binary, ?d>>)
          ?E ->
            parse_param_name(rest, params, <<acc :: binary, ?e>>)
          ?F ->
            parse_param_name(rest, params, <<acc :: binary, ?f>>)
          ?G ->
            parse_param_name(rest, params, <<acc :: binary, ?g>>)
          ?H ->
            parse_param_name(rest, params, <<acc :: binary, ?h>>)
          ?I ->
            parse_param_name(rest, params, <<acc :: binary, ?i>>)
          ?J ->
            parse_param_name(rest, params, <<acc :: binary, ?j>>)
          ?K ->
            parse_param_name(rest, params, <<acc :: binary, ?k>>)
          ?L ->
            parse_param_name(rest, params, <<acc :: binary, ?l>>)
          ?M ->
            parse_param_name(rest, params, <<acc :: binary, ?m>>)
          ?N ->
            parse_param_name(rest, params, <<acc :: binary, ?n>>)
          ?O ->
            parse_param_name(rest, params, <<acc :: binary, ?o>>)
          ?P ->
            parse_param_name(rest, params, <<acc :: binary, ?p>>)
          ?Q ->
            parse_param_name(rest, params, <<acc :: binary, ?q>>)
          ?R ->
            parse_param_name(rest, params, <<acc :: binary, ?r>>)
          ?S ->
            parse_param_name(rest, params, <<acc :: binary, ?s>>)
          ?T ->
            parse_param_name(rest, params, <<acc :: binary, ?t>>)
          ?U ->
            parse_param_name(rest, params, <<acc :: binary, ?u>>)
          ?V ->
            parse_param_name(rest, params, <<acc :: binary, ?v>>)
          ?W ->
            parse_param_name(rest, params, <<acc :: binary, ?w>>)
          ?X ->
            parse_param_name(rest, params, <<acc :: binary, ?x>>)
          ?Y ->
            parse_param_name(rest, params, <<acc :: binary, ?y>>)
          ?Z ->
            parse_param_name(rest, params, <<acc :: binary, ?z>>)
          ^c ->
            parse_param_name(rest, params, <<acc :: binary, c>>)
        end
    end
  end

  defp parse_param_value(<<>>, params, name) do
    :lists.reverse([{name, <<>>} | params])
  end

  defp parse_param_value(<<c, rest :: bits>>, params, name) do
    case (c) do
      ?" ->
        parse_param_quoted_value(rest, params, name, <<>>)
      ?; ->
        parse_before_param(rest, [{name, <<>>} | params])
      ?\s ->
        parse_before_param(rest, [{name, <<>>} | params])
      ?\t ->
        parse_before_param(rest, [{name, <<>>} | params])
      ^c ->
        parse_param_value(rest, params, name, <<c>>)
    end
  end

  defp parse_param_value(<<>>, params, name, acc) do
    :lists.reverse([{name, acc} | params])
  end

  defp parse_param_value(<<c, rest :: bits>>, params, name, acc) do
    case (c) do
      ?; ->
        parse_before_param(rest, [{name, acc} | params])
      ?\s ->
        parse_before_param(rest, [{name, acc} | params])
      ?\t ->
        parse_before_param(rest, [{name, acc} | params])
      ^c ->
        parse_param_value(rest, params, name,
                            <<acc :: binary, c>>)
    end
  end

  defp parse_param_quoted_value(<<?\\, c, rest :: bits>>, params, name, acc) do
    parse_param_quoted_value(rest, params, name,
                               <<acc :: binary, c>>)
  end

  defp parse_param_quoted_value(<<?", rest :: bits>>, params, name, acc) do
    parse_before_param(rest, [{name, acc} | params])
  end

  defp parse_param_quoted_value(<<c, rest :: bits>>, params, name, acc)
      when c !== ?\r do
    parse_param_quoted_value(rest, params, name,
                               <<acc :: binary, c>>)
  end

end