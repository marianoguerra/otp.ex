defmodule :m_uri_string do
  use Bitwise

  def normalize(uRIMap) do
    normalize(uRIMap, [])
  end

  def normalize(uRIMap, []) when is_map(uRIMap) do
    try do
      recompose(normalize_map(uRIMap))
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def normalize(uRIMap, [:return_map]) when is_map(uRIMap) do
    try do
      normalize_map(uRIMap)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def normalize(uRIString, []) do
    case parse(uRIString) do
      value when is_map(value) ->
        try do
          recompose(normalize_map(value))
        catch
          {:error, atom, restData} ->
            {:error, atom, restData}
        end

      error ->
        error
    end
  end

  def normalize(uRIString, [:return_map]) do
    case parse(uRIString) do
      value when is_map(value) ->
        try do
          normalize_map(value)
        catch
          {:error, atom, restData} ->
            {:error, atom, restData}
        end

      error ->
        error
    end
  end

  def parse(uRIString) when is_binary(uRIString) do
    try do
      parse_uri_reference(uRIString, %{})
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def parse(uRIString) when is_list(uRIString) do
    try do
      binary = :unicode.characters_to_binary(uRIString)
      map = parse_uri_reference(binary, %{})
      convert_mapfields_to_list(map)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def recompose(map) do
    case is_valid_map(map) do
      false ->
        {:error, :invalid_map, map}

      true ->
        try do
          t0 = update_scheme(map, :empty)
          t1 = update_userinfo(map, t0)
          t2 = update_host(map, t1)
          t3 = update_port(map, t2)
          t4 = update_path(map, t3)
          t5 = update_query(map, t4)
          update_fragment(map, t5)
        catch
          {:error, atom, restData} ->
            {:error, atom, restData}
        end
    end
  end

  def resolve(uRIMap, baseURIMap) do
    resolve(uRIMap, baseURIMap, [])
  end

  def resolve(uRIMap, baseURIMap, options)
      when is_map(uRIMap) do
    case resolve_map(uRIMap, baseURIMap) do
      targetURIMap when is_map(targetURIMap) ->
        case options do
          [:return_map] ->
            targetURIMap

          [] ->
            recompose(targetURIMap)
        end

      error ->
        error
    end
  end

  def resolve(uRIString, baseURIMap, options) do
    case parse(uRIString) do
      uRIMap when is_map(uRIMap) ->
        resolve(uRIMap, baseURIMap, options)

      error ->
        error
    end
  end

  def transcode(uRIString, options) when is_binary(uRIString) do
    try do
      inEnc = :proplists.get_value(:in_encoding, options, :utf8)
      outEnc = :proplists.get_value(:out_encoding, options, :utf8)
      list = convert_to_list(uRIString, inEnc)
      output = transcode(list, [], inEnc, outEnc)
      convert_to_binary(output, :utf8, outEnc)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def transcode(uRIString, options) when is_list(uRIString) do
    inEnc = :proplists.get_value(:in_encoding, options, :utf8)
    outEnc = :proplists.get_value(:out_encoding, options, :utf8)
    flattened = flatten_list(uRIString, inEnc)

    try do
      transcode(flattened, [], inEnc, outEnc)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def allowed_characters() do
    input = :lists.seq(0, 127)
    scheme = :lists.filter(&is_scheme/1, input)
    userInfo = :lists.filter(&is_userinfo/1, input)
    host = :lists.filter(&is_host/1, input)
    iPv4 = :lists.filter(&is_ipv4/1, input)
    iPv6 = :lists.filter(&is_ipv6/1, input)
    regName = :lists.filter(&is_reg_name/1, input)
    path = :lists.filter(&is_path/1, input)
    query = :lists.filter(&is_query/1, input)
    fragment = :lists.filter(&is_fragment/1, input)
    reserved = :lists.filter(&is_reserved/1, input)
    unreserved = :lists.filter(&is_unreserved/1, input)

    [
      {:scheme, scheme},
      {:userinfo, userInfo},
      {:host, host},
      {:ipv4, iPv4},
      {:ipv6, iPv6},
      {:regname, regName},
      {:path, path},
      {:query, query},
      {:fragment, fragment},
      {:reserved, reserved},
      {:unreserved, unreserved}
    ]
  end

  def percent_decode(uRIMap) when is_map(uRIMap) do
    fun = fn
      k, v
      when k === :userinfo or k === :host or
             k === :path or k === :query or k === :fragment ->
        case raw_decode(v) do
          {:error, reason, input} ->
            throw({:error, {:invalid, {k, {reason, input}}}})

          else__ ->
            else__
        end

      _, v ->
        v
    end

    try do
      :maps.map(fun, uRIMap)
    catch
      return ->
        return
    end
  end

  def percent_decode(uRI) when is_list(uRI) or is_binary(uRI) do
    raw_decode(uRI)
  end

  def compose_query(list) do
    compose_query(list, [{:encoding, :utf8}])
  end

  def compose_query([], _Options) do
    []
  end

  def compose_query(list, options) do
    try do
      compose_query(list, options, false, <<>>)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  defp compose_query([{key, true} | rest], options, isList, acc) do
    separator = get_separator(rest)
    k = form_urlencode(key, options)
    isListNew = isList or is_list(key)
    compose_query(rest, options, isListNew, <<acc::binary, k::binary, separator::binary>>)
  end

  defp compose_query([{key, value} | rest], options, isList, acc) do
    separator = get_separator(rest)
    k = form_urlencode(key, options)
    v = form_urlencode(value, options)
    isListNew = isList or is_list(key) or is_list(value)

    compose_query(
      rest,
      options,
      isListNew,
      <<acc::binary, k::binary, "=", v::binary, separator::binary>>
    )
  end

  defp compose_query([], _Options, isList, acc) do
    case isList do
      true ->
        convert_to_list(acc, :utf8)

      false ->
        acc
    end
  end

  def dissect_query(<<>>) do
    []
  end

  def dissect_query([]) do
    []
  end

  def dissect_query(queryString) when is_list(queryString) do
    try do
      b = convert_to_binary(queryString, :utf8, :utf8)
      dissect_query_key(b, true, [], <<>>, <<>>)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  def dissect_query(queryString) do
    try do
      dissect_query_key(queryString, false, [], <<>>, <<>>)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  defp convert_mapfields_to_list(map) do
    fun = fn
      _, v when is_binary(v) ->
        :unicode.characters_to_list(v)

      _, v ->
        v
    end

    :maps.map(fun, map)
  end

  defp parse_uri_reference(<<>>, _) do
    %{path: <<>>}
  end

  defp parse_uri_reference(uRIString, uRI) do
    try do
      parse_scheme_start(uRIString, uRI)
    catch
      {_, _, _} ->
        parse_relative_part(uRIString, uRI)
    end
  end

  defp parse_relative_part(<<"//"::utf8, rest::binary>>, uRI) do
    try do
      parse_userinfo(rest, uRI)
    catch
      {_, _, _} ->
        {t, uRI1} = parse_host(rest, uRI)
        host = calculate_parsed_host_port(rest, t)
        uRI2 = maybe_add_path(uRI1)
        Map.put(uRI2, :host, remove_brackets(host))
    else
      {t, uRI1} ->
        userinfo = calculate_parsed_userinfo(rest, t)
        uRI2 = maybe_add_path(uRI1)
        Map.put(uRI2, :userinfo, userinfo)
    end
  end

  defp parse_relative_part(<<?/::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    Map.put(uRI1, :path, <<?/::utf8, path::binary>>)
  end

  defp parse_relative_part(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    uRI2 = maybe_add_path(uRI1)
    Map.put(uRI2, :query, query)
  end

  defp parse_relative_part(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    uRI2 = maybe_add_path(uRI1)
    Map.put(uRI2, :fragment, fragment)
  end

  defp parse_relative_part(<<char::utf8, rest::binary>>, uRI) do
    case is_segment_nz_nc(char) do
      true ->
        {t, uRI1} = parse_segment_nz_nc(rest, uRI)
        path = calculate_parsed_part(rest, t)
        Map.put(uRI1, :path, <<char::utf8, path::binary>>)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_segment(<<?/::utf8, rest::binary>>, uRI) do
    parse_segment(rest, uRI)
  end

  defp parse_segment(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_segment(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_segment(<<char::utf8, rest::binary>>, uRI) do
    case is_pchar(char) do
      true ->
        parse_segment(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_segment(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp parse_segment_nz_nc(<<?/::utf8, rest::binary>>, uRI) do
    parse_segment(rest, uRI)
  end

  defp parse_segment_nz_nc(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_segment_nz_nc(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_segment_nz_nc(<<char::utf8, rest::binary>>, uRI) do
    case is_segment_nz_nc(char) do
      true ->
        parse_segment_nz_nc(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_segment_nz_nc(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp is_pchar(?%) do
    true
  end

  defp is_pchar(?:) do
    true
  end

  defp is_pchar(?@) do
    true
  end

  defp is_pchar(char) do
    is_unreserved(char) or is_sub_delim(char)
  end

  defp is_segment_nz_nc(?%) do
    true
  end

  defp is_segment_nz_nc(?@) do
    true
  end

  defp is_segment_nz_nc(char) do
    is_unreserved(char) or is_sub_delim(char)
  end

  defp parse_scheme_start(<<char::utf8, rest::binary>>, uRI) do
    case is_alpha(char) do
      true ->
        {t, uRI1} = parse_scheme(rest, uRI)
        scheme = calculate_parsed_scheme(rest, t)
        uRI2 = maybe_add_path(uRI1)
        Map.put(uRI2, :scheme, <<char::utf8, scheme::binary>>)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp maybe_add_path(map) do
    case :maps.is_key(:path, map) do
      false ->
        Map.put(map, :path, <<>>)

      _Else ->
        map
    end
  end

  defp parse_scheme(<<?:::utf8, rest::binary>>, uRI) do
    {_, uRI1} = parse_hier(rest, uRI)
    {rest, uRI1}
  end

  defp parse_scheme(<<char::utf8, rest::binary>>, uRI) do
    case is_scheme(char) do
      true ->
        parse_scheme(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_scheme(<<>>, _URI) do
    throw({:error, :invalid_uri, <<>>})
  end

  defp is_scheme(?+) do
    true
  end

  defp is_scheme(?-) do
    true
  end

  defp is_scheme(?.) do
    true
  end

  defp is_scheme(char) do
    is_alpha(char) or is_digit(char)
  end

  defp parse_hier(<<"//"::utf8, rest::binary>>, uRI) do
    try do
      parse_userinfo(rest, uRI)
    catch
      {_, _, _} ->
        {t, uRI1} = parse_host(rest, uRI)
        host = calculate_parsed_host_port(rest, t)
        {rest, Map.put(uRI1, :host, remove_brackets(host))}
    else
      {t, uRI1} ->
        userinfo = calculate_parsed_userinfo(rest, t)
        {rest, Map.put(uRI1, :userinfo, userinfo)}
    end
  end

  defp parse_hier(<<?/::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    {rest, Map.put(uRI1, :path, <<?/::utf8, path::binary>>)}
  end

  defp parse_hier(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_hier(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_hier(<<char::utf8, rest::binary>>, uRI) do
    case is_pchar(char) do
      true ->
        {t, uRI1} = parse_segment(rest, uRI)
        path = calculate_parsed_part(rest, t)
        {rest, Map.put(uRI1, :path, <<char::utf8, path::binary>>)}

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_hier(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp parse_userinfo(<<?@::utf8>>, uRI) do
    {<<>>, Map.put(uRI, :host, <<>>)}
  end

  defp parse_userinfo(<<?@::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_host(rest, uRI)
    host = calculate_parsed_host_port(rest, t)
    {rest, Map.put(uRI1, :host, remove_brackets(host))}
  end

  defp parse_userinfo(<<char::utf8, rest::binary>>, uRI) do
    case is_userinfo(char) do
      true ->
        parse_userinfo(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_userinfo(<<>>, _URI) do
    throw({:error, :invalid_uri, <<>>})
  end

  defp is_userinfo(?%) do
    true
  end

  defp is_userinfo(?:) do
    true
  end

  defp is_userinfo(char) do
    is_unreserved(char) or is_sub_delim(char)
  end

  defp parse_host(<<?:::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_port(rest, uRI)
    h = calculate_parsed_host_port(rest, t)
    port = get_port(h)
    {rest, Map.put(uRI1, :port, port)}
  end

  defp parse_host(<<?/::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    {rest, Map.put(uRI1, :path, <<?/::utf8, path::binary>>)}
  end

  defp parse_host(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_host(<<?[::utf8, rest::binary>>, uRI) do
    parse_ipv6_bin(rest, [], uRI)
  end

  defp parse_host(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_host(<<char::utf8, rest::binary>>, uRI) do
    case is_digit(char) do
      true ->
        try do
          parse_ipv4_bin(rest, [char], uRI)
        catch
          {_, _, _} ->
            parse_reg_name(<<char::utf8, rest::binary>>, uRI)
        end

      false ->
        parse_reg_name(<<char::utf8, rest::binary>>, uRI)
    end
  end

  defp parse_host(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp parse_reg_name(<<?:::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_port(rest, uRI)
    h = calculate_parsed_host_port(rest, t)
    port = get_port(h)
    {rest, Map.put(uRI1, :port, port)}
  end

  defp parse_reg_name(<<?/::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    {rest, Map.put(uRI1, :path, <<?/::utf8, path::binary>>)}
  end

  defp parse_reg_name(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_reg_name(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_reg_name(<<char::utf8, rest::binary>>, uRI) do
    case is_reg_name(char) do
      true ->
        parse_reg_name(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_reg_name(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp is_reg_name(?%) do
    true
  end

  defp is_reg_name(char) do
    is_unreserved(char) or is_sub_delim(char)
  end

  defp parse_ipv4_bin(<<?:::utf8, rest::binary>>, acc, uRI) do
    _ = validate_ipv4_address(:lists.reverse(acc))
    {t, uRI1} = parse_port(rest, uRI)
    h = calculate_parsed_host_port(rest, t)
    port = get_port(h)
    {rest, Map.put(uRI1, :port, port)}
  end

  defp parse_ipv4_bin(<<?/::utf8, rest::binary>>, acc, uRI) do
    _ = validate_ipv4_address(:lists.reverse(acc))
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    {rest, Map.put(uRI1, :path, <<?/::utf8, path::binary>>)}
  end

  defp parse_ipv4_bin(<<??::utf8, rest::binary>>, acc, uRI) do
    _ = validate_ipv4_address(:lists.reverse(acc))
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_ipv4_bin(<<?#::utf8, rest::binary>>, acc, uRI) do
    _ = validate_ipv4_address(:lists.reverse(acc))
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_ipv4_bin(<<char::utf8, rest::binary>>, acc, uRI) do
    case is_ipv4(char) do
      true ->
        parse_ipv4_bin(rest, [char | acc], uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_ipv4_bin(<<>>, acc, uRI) do
    _ = validate_ipv4_address(:lists.reverse(acc))
    {<<>>, uRI}
  end

  defp is_ipv4(?.) do
    true
  end

  defp is_ipv4(char) do
    is_digit(char)
  end

  defp validate_ipv4_address(addr) do
    case :inet.parse_ipv4strict_address(addr) do
      {:ok, _} ->
        addr

      {:error, _} ->
        throw({:error, :invalid_uri, addr})
    end
  end

  defp parse_ipv6_bin(<<?]::utf8, rest::binary>>, acc, uRI) do
    _ = validate_ipv6_address(:lists.reverse(acc))
    parse_ipv6_bin_end(rest, uRI)
  end

  defp parse_ipv6_bin(<<char::utf8, rest::binary>>, acc, uRI) do
    case is_ipv6(char) do
      true ->
        parse_ipv6_bin(rest, [char | acc], uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_ipv6_bin(<<>>, _Acc, _URI) do
    throw({:error, :invalid_uri, <<>>})
  end

  defp is_ipv6(?:) do
    true
  end

  defp is_ipv6(?.) do
    true
  end

  defp is_ipv6(char) do
    is_hex_digit(char)
  end

  defp parse_ipv6_bin_end(<<?:::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_port(rest, uRI)
    h = calculate_parsed_host_port(rest, t)
    port = get_port(h)
    {rest, Map.put(uRI1, :port, port)}
  end

  defp parse_ipv6_bin_end(<<?/::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    {rest, Map.put(uRI1, :path, <<?/::utf8, path::binary>>)}
  end

  defp parse_ipv6_bin_end(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_ipv6_bin_end(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_ipv6_bin_end(<<char::utf8, rest::binary>>, uRI) do
    case is_ipv6(char) do
      true ->
        parse_ipv6_bin_end(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_ipv6_bin_end(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp validate_ipv6_address(addr) do
    case :inet.parse_ipv6strict_address(addr) do
      {:ok, _} ->
        addr

      {:error, _} ->
        throw({:error, :invalid_uri, addr})
    end
  end

  defp parse_port(<<?/::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_segment(rest, uRI)
    path = calculate_parsed_part(rest, t)
    {rest, Map.put(uRI1, :path, <<?/::utf8, path::binary>>)}
  end

  defp parse_port(<<??::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_query(rest, uRI)
    query = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :query, query)}
  end

  defp parse_port(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_port(<<char::utf8, rest::binary>>, uRI) do
    case is_digit(char) do
      true ->
        parse_port(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_port(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp parse_query(<<?#::utf8, rest::binary>>, uRI) do
    {t, uRI1} = parse_fragment(rest, uRI)
    fragment = calculate_parsed_query_fragment(rest, t)
    {rest, Map.put(uRI1, :fragment, fragment)}
  end

  defp parse_query(<<char::utf8, rest::binary>>, uRI) do
    case is_query(char) do
      true ->
        parse_query(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_query(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp is_query(?/) do
    true
  end

  defp is_query(??) do
    true
  end

  defp is_query(char) do
    is_pchar(char)
  end

  defp parse_fragment(<<char::utf8, rest::binary>>, uRI) do
    case is_fragment(char) do
      true ->
        parse_fragment(rest, uRI)

      false ->
        throw({:error, :invalid_uri, [char]})
    end
  end

  defp parse_fragment(<<>>, uRI) do
    {<<>>, uRI}
  end

  defp is_fragment(?/) do
    true
  end

  defp is_fragment(??) do
    true
  end

  defp is_fragment(char) do
    is_pchar(char)
  end

  defp is_reserved(?:) do
    true
  end

  defp is_reserved(?/) do
    true
  end

  defp is_reserved(??) do
    true
  end

  defp is_reserved(?#) do
    true
  end

  defp is_reserved(?[) do
    true
  end

  defp is_reserved(?]) do
    true
  end

  defp is_reserved(?@) do
    true
  end

  defp is_reserved(?!) do
    true
  end

  defp is_reserved(?$) do
    true
  end

  defp is_reserved(?&) do
    true
  end

  defp is_reserved(?') do
    true
  end

  defp is_reserved(?() do
    true
  end

  defp is_reserved(?)) do
    true
  end

  defp is_reserved(?*) do
    true
  end

  defp is_reserved(?+) do
    true
  end

  defp is_reserved(?,) do
    true
  end

  defp is_reserved(?;) do
    true
  end

  defp is_reserved(?=) do
    true
  end

  defp is_reserved(_) do
    false
  end

  defp is_sub_delim(?!) do
    true
  end

  defp is_sub_delim(?$) do
    true
  end

  defp is_sub_delim(?&) do
    true
  end

  defp is_sub_delim(?') do
    true
  end

  defp is_sub_delim(?() do
    true
  end

  defp is_sub_delim(?)) do
    true
  end

  defp is_sub_delim(?*) do
    true
  end

  defp is_sub_delim(?+) do
    true
  end

  defp is_sub_delim(?,) do
    true
  end

  defp is_sub_delim(?;) do
    true
  end

  defp is_sub_delim(?=) do
    true
  end

  defp is_sub_delim(_) do
    false
  end

  defp is_unreserved(?-) do
    true
  end

  defp is_unreserved(?.) do
    true
  end

  defp is_unreserved(?_) do
    true
  end

  defp is_unreserved(?~) do
    true
  end

  defp is_unreserved(char) do
    is_alpha(char) or is_digit(char)
  end

  defp is_alpha(c)
       when (?A <= c and c <= ?Z) or
              (?a <= c and c <= ?z) do
    true
  end

  defp is_alpha(_) do
    false
  end

  defp is_digit(c) when ?0 <= c and c <= ?9 do
    true
  end

  defp is_digit(_) do
    false
  end

  defp is_hex_digit(c)
       when (?0 <= c and c <= ?9) or
              (?a <= c and c <= ?f) or (?A <= c and c <= ?F) do
    true
  end

  defp is_hex_digit(_) do
    false
  end

  defp remove_brackets(<<?[::utf8, rest::binary>>) do
    {h, t} = :erlang.split_binary(rest, byte_size(rest) - 1)

    case t === <<?]::utf8>> do
      true ->
        h

      false ->
        rest
    end
  end

  defp remove_brackets(addr) do
    addr
  end

  defp calculate_parsed_scheme(input, <<>>) do
    strip_last_char(input, [?:])
  end

  defp calculate_parsed_scheme(input, unparsed) do
    get_parsed_binary(input, unparsed)
  end

  defp calculate_parsed_part(input, <<>>) do
    strip_last_char(input, [??, ?#])
  end

  defp calculate_parsed_part(input, unparsed) do
    get_parsed_binary(input, unparsed)
  end

  defp calculate_parsed_userinfo(input, <<>>) do
    strip_last_char(input, [??, ?#, ?@])
  end

  defp calculate_parsed_userinfo(input, unparsed) do
    get_parsed_binary(input, unparsed)
  end

  defp calculate_parsed_host_port(input, <<>>) do
    strip_last_char(input, [?:, ??, ?#, ?/])
  end

  defp calculate_parsed_host_port(input, unparsed) do
    get_parsed_binary(input, unparsed)
  end

  defp calculate_parsed_query_fragment(input, <<>>) do
    strip_last_char(input, [?#])
  end

  defp calculate_parsed_query_fragment(input, unparsed) do
    get_parsed_binary(input, unparsed)
  end

  defp get_port(<<>>) do
    :undefined
  end

  defp get_port(b) do
    try do
      :erlang.binary_to_integer(b)
    catch
      :error, :badarg ->
        throw({:error, :invalid_uri, b})
    end
  end

  defp strip_last_char(<<>>, _) do
    <<>>
  end

  defp strip_last_char(input, [c0]) do
    case :binary.last(input) do
      ^c0 ->
        init_binary(input)

      _Else ->
        input
    end
  end

  defp strip_last_char(input, [c0, c1]) do
    case :binary.last(input) do
      ^c0 ->
        init_binary(input)

      ^c1 ->
        init_binary(input)

      _Else ->
        input
    end
  end

  defp strip_last_char(input, [c0, c1, c2]) do
    case :binary.last(input) do
      ^c0 ->
        init_binary(input)

      ^c1 ->
        init_binary(input)

      ^c2 ->
        init_binary(input)

      _Else ->
        input
    end
  end

  defp strip_last_char(input, [c0, c1, c2, c3]) do
    case :binary.last(input) do
      ^c0 ->
        init_binary(input)

      ^c1 ->
        init_binary(input)

      ^c2 ->
        init_binary(input)

      ^c3 ->
        init_binary(input)

      _Else ->
        input
    end
  end

  defp get_parsed_binary(input, unparsed) do
    {first, _} =
      :erlang.split_binary(
        input,
        byte_size(input) - byte_size_exl_head(unparsed)
      )

    first
  end

  defp init_binary(b) do
    {init, _} = :erlang.split_binary(b, byte_size(b) - 1)
    init
  end

  defp byte_size_exl_head(<<>>) do
    0
  end

  defp byte_size_exl_head(binary) do
    byte_size(binary) + 1
  end

  defp encode_scheme([]) do
    throw({:error, :invalid_scheme, ''})
  end

  defp encode_scheme(<<>>) do
    throw({:error, :invalid_scheme, <<>>})
  end

  defp encode_scheme(scheme) do
    case validate_scheme(scheme) do
      true ->
        scheme

      false ->
        throw({:error, :invalid_scheme, scheme})
    end
  end

  defp encode_userinfo(cs) do
    encode(cs, &is_userinfo/1)
  end

  defp encode_host(cs) do
    case classify_host(cs) do
      :regname ->
        cs

      :ipv4 ->
        cs

      :ipv6 ->
        bracket_ipv6(cs)

      :other ->
        encode(cs, &is_reg_name/1)
    end
  end

  defp encode_path(cs) do
    encode(cs, &is_path/1)
  end

  defp encode_query(cs) do
    encode(cs, &is_query/1)
  end

  defp encode_fragment(cs) do
    encode(cs, &is_fragment/1)
  end

  defp decode(cs) do
    decode(cs, <<>>)
  end

  defp decode(l, acc) when is_list(l) do
    b0 = :unicode.characters_to_binary(l)
    b1 = decode(b0, acc)
    :unicode.characters_to_list(b1)
  end

  defp decode(<<?%, c0, c1, cs::binary>>, acc) do
    case is_hex_digit(c0) and is_hex_digit(c1) do
      true ->
        b =
          cond do
            c0 >= ?0 and c0 <= ?9 ->
              c0 - ?0

            c0 >= ?A and c0 <= ?F ->
              c0 - ?A + 10

            c0 >= ?a and c0 <= ?f ->
              c0 - ?a + 10
          end * 16 +
            cond do
              c1 >= ?0 and c1 <= ?9 ->
                c1 - ?0

              c1 >= ?A and c1 <= ?F ->
                c1 - ?A + 10

              c1 >= ?a and c1 <= ?f ->
                c1 - ?a + 10
            end

        case is_unreserved(b) do
          false ->
            h0 = hex_to_upper(c0)
            h1 = hex_to_upper(c1)
            decode(cs, <<acc::binary, ?%, h0, h1>>)

          true ->
            decode(cs, <<acc::binary, b>>)
        end

      false ->
        throw({:error, :invalid_percent_encoding, <<?%, c0, c1>>})
    end
  end

  defp decode(<<c, cs::binary>>, acc) do
    decode(cs, <<acc::binary, c>>)
  end

  defp decode(<<>>, acc) do
    check_utf8(acc)
  end

  defp raw_decode(cs) do
    raw_decode(cs, <<>>)
  end

  defp raw_decode(l, acc) when is_list(l) do
    try do
      b0 = :unicode.characters_to_binary(l)
      b1 = raw_decode(b0, acc)
      :unicode.characters_to_list(b1)
    catch
      {:error, atom, restData} ->
        {:error, atom, restData}
    end
  end

  defp raw_decode(<<?%, c0, c1, cs::binary>>, acc) do
    case is_hex_digit(c0) and is_hex_digit(c1) do
      true ->
        b =
          cond do
            c0 >= ?0 and c0 <= ?9 ->
              c0 - ?0

            c0 >= ?A and c0 <= ?F ->
              c0 - ?A + 10

            c0 >= ?a and c0 <= ?f ->
              c0 - ?a + 10
          end * 16 +
            cond do
              c1 >= ?0 and c1 <= ?9 ->
                c1 - ?0

              c1 >= ?A and c1 <= ?F ->
                c1 - ?A + 10

              c1 >= ?a and c1 <= ?f ->
                c1 - ?a + 10
            end

        raw_decode(cs, <<acc::binary, b>>)

      false ->
        throw({:error, :invalid_percent_encoding, <<?%, c0, c1>>})
    end
  end

  defp raw_decode(<<c, cs::binary>>, acc) do
    raw_decode(cs, <<acc::binary, c>>)
  end

  defp raw_decode(<<>>, acc) do
    check_utf8(acc)
  end

  defp check_utf8(cs) do
    case :unicode.characters_to_list(cs) do
      {:incomplete, _, _} ->
        throw({:error, :invalid_utf8, cs})

      {:error, _, _} ->
        throw({:error, :invalid_utf8, cs})

      _ ->
        cs
    end
  end

  defp hex_to_upper(h) when ?a <= h and h <= ?f do
    h - 32
  end

  defp hex_to_upper(h)
       when (?0 <= h and h <= ?9) or
              (?A <= h and h <= ?F) do
    h
  end

  defp hex_to_upper(h) do
    throw({:error, :invalid_input, h})
  end

  def is_host(?:) do
    true
  end

  def is_host(char) do
    is_unreserved(char) or is_sub_delim(char)
  end

  def is_path(?/) do
    true
  end

  def is_path(char) do
    is_pchar(char)
  end

  defp encode(component, fun) when is_list(component) do
    b = :unicode.characters_to_binary(component)
    :unicode.characters_to_list(encode(b, fun, <<>>))
  end

  defp encode(component, fun) when is_binary(component) do
    encode(component, fun, <<>>)
  end

  defp encode(<<char::utf8, rest::binary>>, fun, acc) do
    c = encode_codepoint_binary(char, fun)
    encode(rest, fun, <<acc::binary, c::binary>>)
  end

  defp encode(<<char, rest::binary>>, _Fun, _Acc) do
    throw({:error, :invalid_input, <<char, rest::binary>>})
  end

  defp encode(<<>>, _Fun, acc) do
    acc
  end

  defp encode_codepoint_binary(c, fun) do
    case fun.(c) do
      false ->
        percent_encode_binary(c)

      true ->
        <<c>>
    end
  end

  defp percent_encode_binary(code) do
    percent_encode_binary(<<code::utf8>>, <<>>)
  end

  defp percent_encode_binary(
         <<a::size(4), b::size(4), rest::binary>>,
         acc
       ) do
    percent_encode_binary(
      rest,
      <<acc::binary, ?%,
        cond do
          a >= 0 and a <= 9 ->
            a + ?0

          a >= 10 and a <= 15 ->
            a + ?A - 10
        end,
        cond do
          b >= 0 and b <= 9 ->
            b + ?0

          b >= 10 and b <= 15 ->
            b + ?A - 10
        end>>
    )
  end

  defp percent_encode_binary(<<>>, acc) do
    acc
  end

  defp validate_scheme([]) do
    true
  end

  defp validate_scheme([h | t]) do
    case is_scheme(h) do
      true ->
        validate_scheme(t)

      false ->
        false
    end
  end

  defp validate_scheme(<<>>) do
    true
  end

  defp validate_scheme(<<h, rest::binary>>) do
    case is_scheme(h) do
      true ->
        validate_scheme(rest)

      false ->
        false
    end
  end

  defp classify_host([]) do
    :other
  end

  defp classify_host(addr) when is_binary(addr) do
    a = :unicode.characters_to_list(addr)
    classify_host_ipv6(a)
  end

  defp classify_host(addr) do
    classify_host_ipv6(addr)
  end

  defp classify_host_ipv6(addr) do
    case is_ipv6_address(addr) do
      true ->
        :ipv6

      false ->
        classify_host_ipv4(addr)
    end
  end

  defp classify_host_ipv4(addr) do
    case is_ipv4_address(addr) do
      true ->
        :ipv4

      false ->
        classify_host_regname(addr)
    end
  end

  defp classify_host_regname([]) do
    :regname
  end

  defp classify_host_regname([h | t]) do
    case is_reg_name(h) do
      true ->
        classify_host_regname(t)

      false ->
        :other
    end
  end

  defp is_ipv4_address(addr) do
    case :inet.parse_ipv4strict_address(addr) do
      {:ok, _} ->
        true

      {:error, _} ->
        false
    end
  end

  defp is_ipv6_address(addr) do
    case :inet.parse_ipv6strict_address(addr) do
      {:ok, _} ->
        true

      {:error, _} ->
        false
    end
  end

  defp bracket_ipv6(addr) when is_binary(addr) do
    concat(<<?[, addr::binary>>, <<?]>>)
  end

  defp bracket_ipv6(addr) when is_list(addr) do
    [?[ | addr] ++ ']'
  end

  defp is_valid_map(%{path: path} = map) do
    (starts_with_two_slash(path) and is_valid_map_host(map)) or
      (:maps.is_key(
         :userinfo,
         map
       ) and is_valid_map_host(map)) or
      (:maps.is_key(
         :port,
         map
       ) and is_valid_map_host(map)) or all_fields_valid(map)
  end

  defp is_valid_map(%{}) do
    false
  end

  defp is_valid_map_host(map) do
    :maps.is_key(:host, map) and all_fields_valid(map)
  end

  defp all_fields_valid(map) do
    fun = fn
      :scheme, _, acc ->
        acc

      :userinfo, _, acc ->
        acc

      :host, _, acc ->
        acc

      :port, _, acc ->
        acc

      :path, _, acc ->
        acc

      :query, _, acc ->
        acc

      :fragment, _, acc ->
        acc

      _, _, _ ->
        false
    end

    :maps.fold(fun, true, map)
  end

  defp starts_with_two_slash([?/, ?/ | _]) do
    true
  end

  defp starts_with_two_slash(<<"//"::utf8, _::binary>>) do
    true
  end

  defp starts_with_two_slash(_) do
    false
  end

  defp update_scheme(%{scheme: scheme}, _) do
    add_colon_postfix(encode_scheme(scheme))
  end

  defp update_scheme(%{}, _) do
    :empty
  end

  defp update_userinfo(%{userinfo: userinfo}, :empty) do
    add_auth_prefix(encode_userinfo(userinfo))
  end

  defp update_userinfo(%{userinfo: userinfo}, uRI) do
    concat(uRI, add_auth_prefix(encode_userinfo(userinfo)))
  end

  defp update_userinfo(%{}, :empty) do
    :empty
  end

  defp update_userinfo(%{}, uRI) do
    uRI
  end

  defp update_host(%{host: host}, :empty) do
    add_auth_prefix(encode_host(host))
  end

  defp update_host(%{host: host} = map, uRI) do
    concat(uRI, add_host_prefix(map, encode_host(host)))
  end

  defp update_host(%{}, :empty) do
    :empty
  end

  defp update_host(%{}, uRI) do
    uRI
  end

  defp update_port(%{port: :undefined}, uRI) do
    concat(uRI, ":")
  end

  defp update_port(%{port: port}, uRI) do
    concat(uRI, add_colon(encode_port(port)))
  end

  defp update_port(%{}, uRI) do
    uRI
  end

  defp update_path(%{path: path}, :empty) do
    encode_path(path)
  end

  defp update_path(%{host: _, path: path0}, uRI) do
    path1 = maybe_flatten_list(path0)
    path = make_path_absolute(path1)
    concat(uRI, encode_path(path))
  end

  defp update_path(%{path: path}, uRI) do
    concat(uRI, encode_path(path))
  end

  defp update_path(%{}, :empty) do
    :empty
  end

  defp update_path(%{}, uRI) do
    uRI
  end

  defp update_query(%{query: query}, :empty) do
    encode_query(query)
  end

  defp update_query(%{query: query}, uRI) do
    concat(uRI, add_question_mark(encode_query(query)))
  end

  defp update_query(%{}, :empty) do
    :empty
  end

  defp update_query(%{}, uRI) do
    uRI
  end

  defp update_fragment(%{fragment: fragment}, :empty) do
    add_hashmark(encode_fragment(fragment))
  end

  defp update_fragment(%{fragment: fragment}, uRI) do
    concat(uRI, add_hashmark(encode_fragment(fragment)))
  end

  defp update_fragment(%{}, :empty) do
    ''
  end

  defp update_fragment(%{}, uRI) do
    uRI
  end

  defp concat(a, b) when is_binary(a) and is_binary(b) do
    <<a::binary, b::binary>>
  end

  defp concat(a, b) when is_binary(a) and is_list(b) do
    :unicode.characters_to_list(a) ++ b
  end

  defp concat(a, b) when is_list(a) do
    a ++ maybe_to_list(b)
  end

  defp add_hashmark(comp) when is_binary(comp) do
    <<?#, comp::binary>>
  end

  defp add_hashmark(comp) when is_list(comp) do
    [?# | comp]
  end

  defp add_question_mark(comp) when is_binary(comp) do
    <<??, comp::binary>>
  end

  defp add_question_mark(comp) when is_list(comp) do
    [?? | comp]
  end

  defp add_colon(comp) when is_binary(comp) do
    <<?:, comp::binary>>
  end

  defp add_colon_postfix(comp) when is_binary(comp) do
    <<comp::binary, ?:>>
  end

  defp add_colon_postfix(comp) when is_list(comp) do
    comp ++ ':'
  end

  defp add_auth_prefix(comp) when is_binary(comp) do
    <<"//", comp::binary>>
  end

  defp add_auth_prefix(comp) when is_list(comp) do
    [?/, ?/ | comp]
  end

  defp add_host_prefix(%{userinfo: _}, host) when is_binary(host) do
    <<?@, host::binary>>
  end

  defp add_host_prefix(%{}, host) when is_binary(host) do
    <<"//", host::binary>>
  end

  defp add_host_prefix(%{userinfo: _}, host) when is_list(host) do
    [?@ | host]
  end

  defp add_host_prefix(%{}, host) when is_list(host) do
    [?/, ?/ | host]
  end

  defp maybe_to_list(comp) when is_binary(comp) do
    :unicode.characters_to_list(comp)
  end

  defp maybe_to_list(comp) do
    comp
  end

  defp encode_port(port) do
    :erlang.integer_to_binary(port)
  end

  defp make_path_absolute(<<>>) do
    <<>>
  end

  defp make_path_absolute('') do
    ''
  end

  defp make_path_absolute(<<"/", _::binary>> = path) do
    path
  end

  defp make_path_absolute([?/ | _] = path) do
    path
  end

  defp make_path_absolute(path) when is_binary(path) do
    concat(<<?/>>, path)
  end

  defp make_path_absolute(path) when is_list(path) do
    concat('/', path)
  end

  defp maybe_flatten_list(path) when is_binary(path) do
    path
  end

  defp maybe_flatten_list(path) do
    :unicode.characters_to_list(path)
  end

  defp resolve_map(uRIMap = %{scheme: _}, _) do
    normalize_path_segment(uRIMap)
  end

  defp resolve_map(uRIMap, %{scheme: _} = baseURIMap) do
    resolve_map(uRIMap, baseURIMap, resolve_path_type(uRIMap))
  end

  defp resolve_map(_URIMap, baseURIMap) when is_map(baseURIMap) do
    {:error, :invalid_scheme, ''}
  end

  defp resolve_map(uRIMap, baseURIString) do
    case parse(baseURIString) do
      baseURIMap = %{scheme: _} ->
        resolve_map(uRIMap, baseURIMap, resolve_path_type(uRIMap))

      baseURIMap when is_map(baseURIMap) ->
        {:error, :invalid_scheme, ''}

      error ->
        error
    end
  end

  defp resolve_path_type(uRIMap) do
    case :erlang.iolist_to_binary(:maps.get(:path, uRIMap, <<>>)) do
      <<>> ->
        :empty_path

      <<?/, _::bits>> ->
        :absolute_path

      _ ->
        :relative_path
    end
  end

  defp resolve_map(uRI = %{host: _}, %{scheme: scheme}, _) do
    normalize_path_segment(Map.put(uRI, :scheme, scheme))
  end

  defp resolve_map(uRI, baseURI, :empty_path) do
    keys =
      case :maps.is_key(:query, uRI) do
        true ->
          [:scheme, :userinfo, :host, :port, :path]

        false ->
          [:scheme, :userinfo, :host, :port, :path, :query]
      end

    :maps.merge(uRI, :maps.with(keys, baseURI))
  end

  defp resolve_map(uRI, baseURI, :absolute_path) do
    normalize_path_segment(
      :maps.merge(
        uRI,
        :maps.with(
          [:scheme, :userinfo, :host, :port],
          baseURI
        )
      )
    )
  end

  defp resolve_map(uRI = %{path: path}, baseURI, :relative_path) do
    normalize_path_segment(
      :maps.merge(
        Map.put(
          uRI,
          :path,
          merge_paths(
            path,
            baseURI
          )
        ),
        :maps.with(
          [:scheme, :userinfo, :host, :port],
          baseURI
        )
      )
    )
  end

  defp merge_paths(path, baseURI = %{path: basePath0}) do
    case {baseURI, :erlang.iolist_size(basePath0)} do
      {%{host: _}, 0} ->
        merge_paths_absolute(path)

      _ ->
        case :string.split(basePath0, <<?/>>, :trailing) do
          [basePath, _] when is_binary(path) ->
            :unicode.characters_to_binary([basePath, ?/, path])

          [basePath, _] when is_list(path) ->
            :unicode.characters_to_list([basePath, ?/, path])

          [_] ->
            path
        end
    end
  end

  defp merge_paths_absolute(path) when is_binary(path) do
    <<?/, path::binary>>
  end

  defp merge_paths_absolute(path) when is_list(path) do
    :unicode.characters_to_list([?/, path])
  end

  defp transcode([?%, _C0, _C1 | _Rest] = l, acc, inEnc, outEnc) do
    transcode_pct(l, acc, <<>>, inEnc, outEnc)
  end

  defp transcode([_C | _Rest] = l, acc, inEnc, outEnc) do
    transcode(l, acc, [], inEnc, outEnc)
  end

  defp transcode([?%, _C0, _C1 | _Rest] = l, acc, list, inEncoding, outEncoding) do
    transcode_pct(l, list ++ acc, <<>>, inEncoding, outEncoding)
  end

  defp transcode([c | rest], acc, list, inEncoding, outEncoding) do
    transcode(rest, acc, [c | list], inEncoding, outEncoding)
  end

  defp transcode([], acc, list, _InEncoding, _OutEncoding) do
    :lists.reverse(list ++ acc)
  end

  defp transcode_pct([?%, c0, c1 | rest] = l, acc, b, inEncoding, outEncoding) do
    case is_hex_digit(c0) and is_hex_digit(c1) do
      true ->
        int =
          cond do
            c0 >= ?0 and c0 <= ?9 ->
              c0 - ?0

            c0 >= ?A and c0 <= ?F ->
              c0 - ?A + 10

            c0 >= ?a and c0 <= ?f ->
              c0 - ?a + 10
          end * 16 +
            cond do
              c1 >= ?0 and c1 <= ?9 ->
                c1 - ?0

              c1 >= ?A and c1 <= ?F ->
                c1 - ?A + 10

              c1 >= ?a and c1 <= ?f ->
                c1 - ?a + 10
            end

        transcode_pct(rest, acc, <<b::binary, int>>, inEncoding, outEncoding)

      false ->
        throw({:error, :invalid_percent_encoding, l})
    end
  end

  defp transcode_pct([_C | _Rest] = l, acc, b, inEncoding, outEncoding) do
    outBinary = convert_to_binary(b, inEncoding, outEncoding)
    pctEncUtf8 = percent_encode_segment(outBinary)
    out = :lists.reverse(convert_to_list(pctEncUtf8, :utf8))
    transcode(l, out ++ acc, [], inEncoding, outEncoding)
  end

  defp transcode_pct([], acc, b, inEncoding, outEncoding) do
    outBinary = convert_to_binary(b, inEncoding, outEncoding)
    pctEncUtf8 = percent_encode_segment(outBinary)
    out = convert_to_list(pctEncUtf8, :utf8)
    :lists.reverse(acc, out)
  end

  defp convert_to_binary(binary, inEncoding, outEncoding) do
    case :unicode.characters_to_binary(binary, inEncoding, outEncoding) do
      {:error, _List, restData} ->
        throw({:error, :invalid_input, restData})

      {:incomplete, _List, restData} ->
        throw({:error, :invalid_input, restData})

      result ->
        result
    end
  end

  defp convert_to_list(binary, inEncoding) do
    case :unicode.characters_to_list(
           binary,
           inEncoding
         ) do
      {:error, _List, restData} ->
        throw({:error, :invalid_input, restData})

      {:incomplete, _List, restData} ->
        throw({:error, :invalid_input, restData})

      result ->
        result
    end
  end

  defp flatten_list([], _) do
    []
  end

  defp flatten_list(l, inEnc) do
    flatten_list(l, inEnc, [])
  end

  defp flatten_list([h | t], inEnc, acc) when is_binary(h) do
    l = convert_to_list(h, inEnc)
    flatten_list(t, inEnc, :lists.reverse(l, acc))
  end

  defp flatten_list([h | t], inEnc, acc) when is_list(h) do
    flatten_list(h ++ t, inEnc, acc)
  end

  defp flatten_list([h | t], inEnc, acc) do
    flatten_list(t, inEnc, [h | acc])
  end

  defp flatten_list([], _InEnc, acc) do
    :lists.reverse(acc)
  end

  defp flatten_list(arg, _, _) do
    throw({:error, :invalid_input, arg})
  end

  defp percent_encode_segment(segment) do
    percent_encode_binary(segment, <<>>)
  end

  defp get_separator([]) do
    <<>>
  end

  defp get_separator(_L) do
    "&"
  end

  defp form_urlencode(cs, [{:encoding, :latin1}]) when is_list(cs) do
    b = convert_to_binary(cs, :utf8, :utf8)
    html5_byte_encode(base10_encode(b))
  end

  defp form_urlencode(cs, [{:encoding, :latin1}])
       when is_binary(cs) do
    html5_byte_encode(base10_encode(cs))
  end

  defp form_urlencode(cs, [{:encoding, encoding}])
       when (is_list(cs) and encoding === :utf8) or
              encoding === :unicode do
    b = convert_to_binary(cs, :utf8, encoding)
    html5_byte_encode(b)
  end

  defp form_urlencode(cs, [{:encoding, encoding}])
       when (is_binary(cs) and encoding === :utf8) or
              encoding === :unicode do
    html5_byte_encode(cs)
  end

  defp form_urlencode(cs, [{:encoding, encoding}])
       when is_list(cs) or
              is_binary(cs) do
    throw({:error, :invalid_encoding, encoding})
  end

  defp form_urlencode(cs, _) do
    throw({:error, :invalid_input, cs})
  end

  defp base10_encode(cs) do
    base10_encode(cs, <<>>)
  end

  defp base10_encode(<<>>, acc) do
    acc
  end

  defp base10_encode(<<h::utf8, t::binary>>, acc) when h > 255 do
    base10 =
      convert_to_binary(
        :erlang.integer_to_list(
          h,
          10
        ),
        :utf8,
        :utf8
      )

    base10_encode(
      t,
      <<acc::binary, "&#", base10::binary, ?;>>
    )
  end

  defp base10_encode(<<h::utf8, t::binary>>, acc) do
    base10_encode(t, <<acc::binary, h>>)
  end

  defp html5_byte_encode(b) do
    html5_byte_encode(b, <<>>)
  end

  defp html5_byte_encode(<<>>, acc) do
    acc
  end

  defp html5_byte_encode(<<?\s, t::binary>>, acc) do
    html5_byte_encode(t, <<acc::binary, ?+>>)
  end

  defp html5_byte_encode(<<h, t::binary>>, acc) do
    case is_url_char(h) do
      true ->
        html5_byte_encode(t, <<acc::binary, h>>)

      false ->
        <<a::size(4), b::size(4)>> = <<h>>

        html5_byte_encode(
          t,
          <<acc::binary, ?%,
            cond do
              a >= 0 and a <= 9 ->
                a + ?0

              a >= 10 and a <= 15 ->
                a + ?A - 10
            end,
            cond do
              b >= 0 and b <= 9 ->
                b + ?0

              b >= 10 and b <= 15 ->
                b + ?A - 10
            end>>
        )
    end
  end

  defp html5_byte_encode(h, _Acc) do
    throw({:error, :invalid_input, h})
  end

  defp is_url_char(c)
       when c === 42 or c === 45 or c === 46 or
              c === 95 or (48 <= c and c <= 57) or
              (65 <= c and c <= 90) or (97 <= c and c <= 122) do
    true
  end

  defp is_url_char(_) do
    false
  end

  defp dissect_query_key(<<?=, t::binary>>, isList, acc, key, value) do
    dissect_query_value(t, isList, acc, key, value)
  end

  defp dissect_query_key(<<"&#", t::binary>>, isList, acc, key, value) do
    dissect_query_key(t, isList, acc, <<key::binary, "&#">>, value)
  end

  defp dissect_query_key(t = <<?&, _::binary>>, isList, acc, key, <<>>) do
    dissect_query_value(t, isList, acc, key, true)
  end

  defp dissect_query_key(<<h, t::binary>>, isList, acc, key, value) do
    dissect_query_key(t, isList, acc, <<key::binary, h>>, value)
  end

  defp dissect_query_key(t = <<>>, isList, acc, key, <<>>) do
    dissect_query_value(t, isList, acc, key, true)
  end

  defp dissect_query_value(<<?&, t::binary>>, isList, acc, key, value) do
    k = form_urldecode(isList, key)
    v = form_urldecode(isList, value)
    dissect_query_key(t, isList, [{k, v} | acc], <<>>, <<>>)
  end

  defp dissect_query_value(<<h, t::binary>>, isList, acc, key, value) do
    dissect_query_value(t, isList, acc, key, <<value::binary, h>>)
  end

  defp dissect_query_value(<<>>, isList, acc, key, value) do
    k = form_urldecode(isList, key)
    v = form_urldecode(isList, value)
    :lists.reverse([{k, v} | acc])
  end

  defp form_urldecode(_, true) do
    true
  end

  defp form_urldecode(true, b) do
    result = base10_decode(form_urldecode(b, <<>>))
    convert_to_list(result, :utf8)
  end

  defp form_urldecode(false, b) do
    base10_decode(form_urldecode(b, <<>>))
  end

  defp form_urldecode(<<>>, acc) do
    acc
  end

  defp form_urldecode(<<?+, t::binary>>, acc) do
    form_urldecode(t, <<acc::binary, ?\s>>)
  end

  defp form_urldecode(<<?%, c0, c1, t::binary>>, acc) do
    case is_hex_digit(c0) and is_hex_digit(c1) do
      true ->
        v =
          cond do
            c0 >= ?0 and c0 <= ?9 ->
              c0 - ?0

            c0 >= ?A and c0 <= ?F ->
              c0 - ?A + 10

            c0 >= ?a and c0 <= ?f ->
              c0 - ?a + 10
          end * 16 +
            cond do
              c1 >= ?0 and c1 <= ?9 ->
                c1 - ?0

              c1 >= ?A and c1 <= ?F ->
                c1 - ?A + 10

              c1 >= ?a and c1 <= ?f ->
                c1 - ?a + 10
            end

        form_urldecode(t, <<acc::binary, v>>)

      false ->
        l = convert_to_list(<<?%, c0, c1, t::binary>>, :utf8)
        throw({:error, :invalid_percent_encoding, l})
    end
  end

  defp form_urldecode(<<h::utf8, t::binary>>, acc) do
    form_urldecode(t, <<acc::binary, h::utf8>>)
  end

  defp form_urldecode(<<h, _::binary>>, _Acc) do
    throw({:error, :invalid_character, [h]})
  end

  defp base10_decode(cs) do
    base10_decode(cs, <<>>)
  end

  defp base10_decode(<<>>, acc) do
    acc
  end

  defp base10_decode(<<"&#", t::binary>>, acc) do
    base10_decode_unicode(t, acc)
  end

  defp base10_decode(<<h::utf8, t::binary>>, acc) do
    base10_decode(t, <<acc::binary, h::utf8>>)
  end

  defp base10_decode(<<h, _::binary>>, _) do
    throw({:error, :invalid_input, [h]})
  end

  defp base10_decode_unicode(b, acc) do
    base10_decode_unicode(b, 0, acc)
  end

  defp base10_decode_unicode(<<h::utf8, t::binary>>, codepoint, acc)
       when ?0 <= h and h <= ?9 do
    res = codepoint * 10 + (h - ?0)
    base10_decode_unicode(t, res, acc)
  end

  defp base10_decode_unicode(<<?;, t::binary>>, codepoint, acc) do
    base10_decode(t, <<acc::binary, codepoint::utf8>>)
  end

  defp base10_decode_unicode(<<h, _::binary>>, _, _) do
    throw({:error, :invalid_input, [h]})
  end

  defp normalize_map(uRIMap) do
    normalize_path_segment(
      normalize_scheme_based(normalize_percent_encoding(normalize_case(uRIMap)))
    )
  end

  defp normalize_case(%{scheme: scheme, host: host} = map) do
    Map.merge(map, %{scheme: to_lower(scheme), host: to_lower(host)})
  end

  defp normalize_case(%{host: host} = map) do
    Map.put(map, :host, to_lower(host))
  end

  defp normalize_case(%{scheme: scheme} = map) do
    Map.put(map, :scheme, to_lower(scheme))
  end

  defp normalize_case(%{} = map) do
    map
  end

  defp normalize_percent_encoding(map) do
    fun = fn
      k, v
      when k === :userinfo or k === :host or
             k === :path or k === :query or k === :fragment ->
        decode(v)

      _, v ->
        v
    end

    :maps.map(fun, map)
  end

  defp to_lower(cs) when is_list(cs) do
    b = convert_to_binary(cs, :utf8, :utf8)
    convert_to_list(to_lower(b), :utf8)
  end

  defp to_lower(cs) when is_binary(cs) do
    to_lower(cs, <<>>)
  end

  defp to_lower(<<c, cs::binary>>, acc)
       when ?A <= c and
              c <= ?Z do
    to_lower(cs, <<acc::binary, c + 32>>)
  end

  defp to_lower(<<c, cs::binary>>, acc) do
    to_lower(cs, <<acc::binary, c>>)
  end

  defp to_lower(<<>>, acc) do
    acc
  end

  defp normalize_path_segment(map) do
    path = :maps.get(:path, map, :undefined)
    Map.put(map, :path, remove_dot_segments(path))
  end

  defp remove_dot_segments(path) when is_binary(path) do
    remove_dot_segments(path, <<>>)
  end

  defp remove_dot_segments(path) when is_list(path) do
    b = convert_to_binary(path, :utf8, :utf8)
    b1 = remove_dot_segments(b, <<>>)
    convert_to_list(b1, :utf8)
  end

  defp remove_dot_segments(<<>>, output) do
    output
  end

  defp remove_dot_segments(<<"../", t::binary>>, output) do
    remove_dot_segments(t, output)
  end

  defp remove_dot_segments(<<"./", t::binary>>, output) do
    remove_dot_segments(t, output)
  end

  defp remove_dot_segments(<<"/./", t::binary>>, output) do
    remove_dot_segments(<<?/, t::binary>>, output)
  end

  defp remove_dot_segments("/.", output) do
    remove_dot_segments(<<?/>>, output)
  end

  defp remove_dot_segments(<<"/../", t::binary>>, output) do
    out1 = remove_last_segment(output)
    remove_dot_segments(<<?/, t::binary>>, out1)
  end

  defp remove_dot_segments("/..", output) do
    out1 = remove_last_segment(output)
    remove_dot_segments(<<?/>>, out1)
  end

  defp remove_dot_segments(<<?.>>, output) do
    remove_dot_segments(<<>>, output)
  end

  defp remove_dot_segments("..", output) do
    remove_dot_segments(<<>>, output)
  end

  defp remove_dot_segments(input, output) do
    {first, rest} = first_path_segment(input)

    remove_dot_segments(
      rest,
      <<output::binary, first::binary>>
    )
  end

  defp first_path_segment(input) do
    f = first_path_segment(input, <<>>)
    :erlang.split_binary(input, byte_size(f))
  end

  defp first_path_segment(<<?/, t::binary>>, acc) do
    first_path_segment_end(
      <<t::binary>>,
      <<acc::binary, ?/>>
    )
  end

  defp first_path_segment(<<c, t::binary>>, acc) do
    first_path_segment_end(
      <<t::binary>>,
      <<acc::binary, c>>
    )
  end

  defp first_path_segment_end(<<>>, acc) do
    acc
  end

  defp first_path_segment_end(<<?/, _::binary>>, acc) do
    acc
  end

  defp first_path_segment_end(<<c, t::binary>>, acc) do
    first_path_segment_end(
      <<t::binary>>,
      <<acc::binary, c>>
    )
  end

  defp remove_last_segment(<<>>) do
    <<>>
  end

  defp remove_last_segment(b) do
    {init, last} = :erlang.split_binary(b, byte_size(b) - 1)

    case last do
      <<?/>> ->
        init

      _Char ->
        remove_last_segment(init)
    end
  end

  defp normalize_scheme_based(map) do
    scheme = :maps.get(:scheme, map, :undefined)
    port = :maps.get(:port, map, :undefined)
    path = :maps.get(:path, map, :undefined)
    normalize_scheme_based(map, scheme, port, path)
  end

  defp normalize_scheme_based(map, scheme, port, path)
       when scheme === 'http' or
              scheme === "http" do
    normalize_http(map, port, path)
  end

  defp normalize_scheme_based(map, scheme, port, path)
       when scheme === 'https' or
              scheme === "https" do
    normalize_https(map, port, path)
  end

  defp normalize_scheme_based(map, scheme, port, _Path)
       when scheme === 'ftp' or
              scheme === "ftp" do
    normalize_ftp(map, port)
  end

  defp normalize_scheme_based(map, scheme, port, _Path)
       when scheme === 'ssh' or
              scheme === "ssh" do
    normalize_ssh_sftp(map, port)
  end

  defp normalize_scheme_based(map, scheme, port, _Path)
       when scheme === 'sftp' or
              scheme === "sftp" do
    normalize_ssh_sftp(map, port)
  end

  defp normalize_scheme_based(map, scheme, port, _Path)
       when scheme === 'tftp' or
              scheme === "tftp" do
    normalize_tftp(map, port)
  end

  defp normalize_scheme_based(map, _, _, _) do
    map
  end

  defp normalize_http(map, port, path) do
    m1 = normalize_port(map, port, 80)
    normalize_http_path(m1, path)
  end

  defp normalize_https(map, port, path) do
    m1 = normalize_port(map, port, 443)
    normalize_http_path(m1, path)
  end

  defp normalize_ftp(map, port) do
    normalize_port(map, port, 21)
  end

  defp normalize_ssh_sftp(map, port) do
    normalize_port(map, port, 22)
  end

  defp normalize_tftp(map, port) do
    normalize_port(map, port, 69)
  end

  defp normalize_port(map, port, default) do
    case port do
      ^default ->
        :maps.remove(:port, map)

      _Else ->
        map
    end
  end

  defp normalize_http_path(map, path) do
    case path do
      '' ->
        Map.put(map, :path, '/')

      <<>> ->
        Map.put(map, :path, "/")

      _Else ->
        map
    end
  end
end
