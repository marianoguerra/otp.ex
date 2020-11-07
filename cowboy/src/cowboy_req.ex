defmodule :cowboy_req do
  use Bitwise

  def method(%{:method => method}) do
    method
  end

  def version(%{:version => version}) do
    version
  end

  def peer(%{:peer => peer}) do
    peer
  end

  def sock(%{:sock => sock}) do
    sock
  end

  def cert(%{:cert => cert}) do
    cert
  end

  def scheme(%{:scheme => scheme}) do
    scheme
  end

  def host(%{:host => host}) do
    host
  end

  def host_info(%{:host_info => hostInfo}) do
    hostInfo
  end

  def port(%{:port => port}) do
    port
  end

  def path(%{:path => path}) do
    path
  end

  def path_info(%{:path_info => pathInfo}) do
    pathInfo
  end

  def qs(%{:qs => qs}) do
    qs
  end

  def parse_qs(%{:qs => qs}) do
    try do
      :cow_qs.parse_qs(qs)
    catch
      _, _ ->
        :erlang.raise(
          :exit,
          {:request_error, :qs,
           :"Malformed query string; application/x-www-form-urlencoded expected."},
          __STACKTRACE__
        )
    end
  end

  def match_qs(fields, req) do
    case filter(
           fields,
           kvlist_to_map(fields, parse_qs(req))
         ) do
      {:ok, map} ->
        map

      {:error, errors} ->
        exit(
          {:request_error, {:match_qs, errors},
           :"Query string validation constraints failed for the reasons provided."}
        )
    end
  end

  def uri(req) do
    uri(req, %{})
  end

  def uri(
        %{:scheme => scheme0, :host => host0, :port => port0, :path => path0, :qs => qs0},
        opts
      ) do
    scheme =
      case :maps.get(:scheme, opts, scheme0) do
        s = :undefined ->
          s

        s ->
          :erlang.iolist_to_binary(s)
      end

    host = :maps.get(:host, opts, host0)
    port = :maps.get(:port, opts, port0)

    {path, qs} =
      case :maps.get(:path, opts, path0) do
        "*" ->
          {<<>>, <<>>}

        p ->
          {p, :maps.get(:qs, opts, qs0)}
      end

    fragment = :maps.get(:fragment, opts, :undefined)
    [uri_host(scheme, scheme0, port, host), uri_path(path), uri_qs(qs), uri_fragment(fragment)]
  end

  defp uri_host(_, _, _, :undefined) do
    <<>>
  end

  defp uri_host(scheme, scheme0, port, host) do
    case :erlang.iolist_size(host) do
      0 ->
        <<>>

      _ ->
        [uri_scheme(scheme), "//", host, uri_port(scheme, scheme0, port)]
    end
  end

  defp uri_scheme(:undefined) do
    <<>>
  end

  defp uri_scheme(scheme) do
    case :erlang.iolist_size(scheme) do
      0 ->
        scheme

      _ ->
        [scheme, ?:]
    end
  end

  defp uri_port(_, _, :undefined) do
    <<>>
  end

  defp uri_port(:undefined, "http", 80) do
    <<>>
  end

  defp uri_port(:undefined, "https", 443) do
    <<>>
  end

  defp uri_port("http", _, 80) do
    <<>>
  end

  defp uri_port("https", _, 443) do
    <<>>
  end

  defp uri_port(_, _, port) do
    [?:, :erlang.integer_to_binary(port)]
  end

  defp uri_path(:undefined) do
    <<>>
  end

  defp uri_path(path) do
    path
  end

  defp uri_qs(:undefined) do
    <<>>
  end

  defp uri_qs(qs) do
    case :erlang.iolist_size(qs) do
      0 ->
        qs

      _ ->
        [??, qs]
    end
  end

  defp uri_fragment(:undefined) do
    <<>>
  end

  defp uri_fragment(fragment) do
    case :erlang.iolist_size(fragment) do
      0 ->
        fragment

      _ ->
        [?#, fragment]
    end
  end

  def binding(name, req) do
    binding(name, req, :undefined)
  end

  def binding(name, %{:bindings => bindings}, default)
      when is_atom(name) do
    case bindings do
      %{^name => value} ->
        value

      _ ->
        default
    end
  end

  def binding(name, _, default) when is_atom(name) do
    default
  end

  def bindings(%{:bindings => bindings}) do
    bindings
  end

  def bindings(_) do
    %{}
  end

  def header(name, req) do
    header(name, req, :undefined)
  end

  def header(name, %{:headers => headers}, default) do
    :maps.get(name, headers, default)
  end

  def headers(%{:headers => headers}) do
    headers
  end

  def parse_header(name = "content-length", req) do
    parse_header(name, req, 0)
  end

  def parse_header(name = "cookie", req) do
    parse_header(name, req, [])
  end

  def parse_header(name, req) do
    parse_header(name, req, :undefined)
  end

  def parse_header(name, req, default) do
    try do
      parse_header(name, req, default, parse_header_fun(name))
    catch
      _, _ ->
        :erlang.raise(
          :exit,
          {:request_error, {:header, name},
           :"Malformed header. Please consult the relevant specification."},
          __STACKTRACE__
        )
    end
  end

  defp parse_header_fun("accept") do
    &:cow_http_hd.parse_accept/1
  end

  defp parse_header_fun("accept-charset") do
    &:cow_http_hd.parse_accept_charset/1
  end

  defp parse_header_fun("accept-encoding") do
    &:cow_http_hd.parse_accept_encoding/1
  end

  defp parse_header_fun("accept-language") do
    &:cow_http_hd.parse_accept_language/1
  end

  defp parse_header_fun("access-control-request-headers") do
    &:cow_http_hd.parse_access_control_request_headers/1
  end

  defp parse_header_fun("access-control-request-method") do
    &:cow_http_hd.parse_access_control_request_method/1
  end

  defp parse_header_fun("authorization") do
    &:cow_http_hd.parse_authorization/1
  end

  defp parse_header_fun("connection") do
    &:cow_http_hd.parse_connection/1
  end

  defp parse_header_fun("content-encoding") do
    &:cow_http_hd.parse_content_encoding/1
  end

  defp parse_header_fun("content-language") do
    &:cow_http_hd.parse_content_language/1
  end

  defp parse_header_fun("content-length") do
    &:cow_http_hd.parse_content_length/1
  end

  defp parse_header_fun("content-type") do
    &:cow_http_hd.parse_content_type/1
  end

  defp parse_header_fun("cookie") do
    &:cow_cookie.parse_cookie/1
  end

  defp parse_header_fun("expect") do
    &:cow_http_hd.parse_expect/1
  end

  defp parse_header_fun("if-match") do
    &:cow_http_hd.parse_if_match/1
  end

  defp parse_header_fun("if-modified-since") do
    &:cow_http_hd.parse_if_modified_since/1
  end

  defp parse_header_fun("if-none-match") do
    &:cow_http_hd.parse_if_none_match/1
  end

  defp parse_header_fun("if-range") do
    &:cow_http_hd.parse_if_range/1
  end

  defp parse_header_fun("if-unmodified-since") do
    &:cow_http_hd.parse_if_unmodified_since/1
  end

  defp parse_header_fun("max-forwards") do
    &:cow_http_hd.parse_max_forwards/1
  end

  defp parse_header_fun("origin") do
    &:cow_http_hd.parse_origin/1
  end

  defp parse_header_fun("proxy-authorization") do
    &:cow_http_hd.parse_proxy_authorization/1
  end

  defp parse_header_fun("range") do
    &:cow_http_hd.parse_range/1
  end

  defp parse_header_fun("sec-websocket-extensions") do
    &:cow_http_hd.parse_sec_websocket_extensions/1
  end

  defp parse_header_fun("sec-websocket-protocol") do
    &:cow_http_hd.parse_sec_websocket_protocol_req/1
  end

  defp parse_header_fun("sec-websocket-version") do
    &:cow_http_hd.parse_sec_websocket_version_req/1
  end

  defp parse_header_fun("trailer") do
    &:cow_http_hd.parse_trailer/1
  end

  defp parse_header_fun("upgrade") do
    &:cow_http_hd.parse_upgrade/1
  end

  defp parse_header_fun("x-forwarded-for") do
    &:cow_http_hd.parse_x_forwarded_for/1
  end

  defp parse_header(name, req, default, parseFun) do
    case header(name, req) do
      :undefined ->
        default

      value ->
        parseFun.(value)
    end
  end

  def filter_cookies(names0, req = %{:headers => headers}) do
    names =
      for n <- names0 do
        cond do
          is_atom(n) ->
            :erlang.atom_to_binary(n, :utf8)

          true ->
            n
        end
      end

    case header("cookie", req) do
      :undefined ->
        req

      value0 ->
        cookies0 = :binary.split(value0, <<?;>>)

        cookies =
          :lists.filter(
            fn cookie ->
              :lists.member(cookie_name(cookie), names)
            end,
            cookies0
          )

        value =
          :erlang.iolist_to_binary(
            :lists.join(
              ?;,
              cookies
            )
          )

        %{req | :headers => %{headers | "cookie" => value}}
    end
  end

  defp cookie_name(<<?\s, rest::binary>>) do
    cookie_name(rest)
  end

  defp cookie_name(<<?\t, rest::binary>>) do
    cookie_name(rest)
  end

  defp cookie_name(name) do
    cookie_name(name, <<>>)
  end

  defp cookie_name(<<>>, name) do
    name
  end

  defp cookie_name(<<?=, _::bits>>, name) do
    name
  end

  defp cookie_name(<<c, rest::bits>>, acc) do
    cookie_name(rest, <<acc::binary, c>>)
  end

  def parse_cookies(req) do
    parse_header("cookie", req)
  end

  def match_cookies(fields, req) do
    case filter(
           fields,
           kvlist_to_map(fields, parse_cookies(req))
         ) do
      {:ok, map} ->
        map

      {:error, errors} ->
        exit(
          {:request_error, {:match_cookies, errors},
           :"Cookie validation constraints failed for the reasons provided."}
        )
    end
  end

  def has_body(%{:has_body => hasBody}) do
    hasBody
  end

  def body_length(%{:body_length => length}) do
    length
  end

  def read_body(req) do
    read_body(req, %{})
  end

  def read_body(req = %{:has_body => false}, _) do
    {:ok, <<>>, req}
  end

  def read_body(req = %{:has_read_body => true}, _) do
    {:ok, <<>>, req}
  end

  def read_body(req, opts) do
    length = :maps.get(:length, opts, 8_000_000)
    period = :maps.get(:period, opts, 15000)
    timeout = :maps.get(:timeout, opts, period + 1000)
    ref = make_ref()
    cast({:read_body, self(), ref, length, period}, req)

    receive do
      {:request_body, ^ref, :nofin, body} ->
        {:more, body, req}

      {:request_body, ^ref, :fin, bodyLength, body} ->
        {:ok, body, set_body_length(req, bodyLength)}
    after
      timeout ->
        exit(:timeout)
    end
  end

  defp set_body_length(req = %{:headers => headers}, bodyLength) do
    %{
      req
      | :headers => %{headers | "content-length" => :erlang.integer_to_binary(bodyLength)},
        :body_length => bodyLength,
        :has_read_body => true
    }
  end

  def read_urlencoded_body(req) do
    read_urlencoded_body(
      req,
      %{:length => 64000, :period => 5000}
    )
  end

  def read_urlencoded_body(req0, opts) do
    case read_body(req0, opts) do
      {:ok, body, req} ->
        try do
          {:ok, :cow_qs.parse_qs(body), req}
        catch
          _, _ ->
            :erlang.raise(
              :exit,
              {:request_error, :urlencoded_body,
               :"Malformed body; application/x-www-form-urlencoded expected."},
              __STACKTRACE__
            )
        end

      {:more, body, _} ->
        length = :maps.get(:length, opts, 64000)

        cond do
          byte_size(body) < length ->
            exit(
              {:request_error, :timeout,
               :"The request body was not received within the configured time."}
            )

          true ->
            exit(
              {:request_error, :payload_too_large,
               :"The request body is larger than allowed by configuration."}
            )
        end
    end
  end

  def read_and_match_urlencoded_body(fields, req) do
    read_and_match_urlencoded_body(fields, req, %{:length => 64000, :period => 5000})
  end

  def read_and_match_urlencoded_body(fields, req0, opts) do
    {:ok, qs, req} = read_urlencoded_body(req0, opts)

    case filter(fields, kvlist_to_map(fields, qs)) do
      {:ok, map} ->
        {:ok, map, req}

      {:error, errors} ->
        exit(
          {:request_error, {:read_and_match_urlencoded_body, errors},
           :"Urlencoded request body validation constraints failed for the reasons provided."}
        )
    end
  end

  def read_part(req) do
    read_part(req, %{:length => 64000, :period => 5000})
  end

  def read_part(req, opts) do
    case :maps.is_key(:multipart, req) do
      true ->
        {data, req2} = stream_multipart(req, opts, :headers)
        read_part(data, opts, req2)

      false ->
        read_part(init_multipart(req), opts)
    end
  end

  defp read_part(buffer, opts, req = %{:multipart => {boundary, _}}) do
    try do
      :cow_multipart.parse_headers(buffer, boundary)
    catch
      _, _ ->
        :erlang.raise(
          :exit,
          {:request_error, {:multipart, :headers}, :"Malformed body; multipart expected."},
          __STACKTRACE__
        )
    else
      :more ->
        {data, req2} = stream_multipart(req, opts, :headers)
        read_part(<<buffer::binary, data::binary>>, opts, req2)

      {:more, buffer2} ->
        {data, req2} = stream_multipart(req, opts, :headers)
        read_part(<<buffer2::binary, data::binary>>, opts, req2)

      {:ok, headers0, rest} ->
        headers = :maps.from_list(headers0)
        true = map_size(headers) === length(headers0)
        {:ok, headers, %{req | :multipart => {boundary, rest}}}

      {:done, _} ->
        {:done, %{req | :multipart => :done}}
    end
  end

  def read_part_body(req) do
    read_part_body(req, %{})
  end

  def read_part_body(req, opts) do
    case :maps.is_key(:multipart, req) do
      true ->
        read_part_body(<<>>, opts, req, <<>>)

      false ->
        read_part_body(init_multipart(req), opts)
    end
  end

  defp read_part_body(buffer, opts, req = %{:multipart => {boundary, _}}, acc) do
    length = :maps.get(:length, opts, 8_000_000)

    case byte_size(acc) > length do
      true ->
        {:more, acc, %{req | :multipart => {boundary, buffer}}}

      false ->
        {data, req2} = stream_multipart(req, opts, :body)

        case :cow_multipart.parse_body(
               <<buffer::binary, data::binary>>,
               boundary
             ) do
          {:ok, body} ->
            read_part_body(<<>>, opts, req2, <<acc::binary, body::binary>>)

          {:ok, body, rest} ->
            read_part_body(rest, opts, req2, <<acc::binary, body::binary>>)

          :done ->
            {:ok, acc, req2}

          {:done, body} ->
            {:ok, <<acc::binary, body::binary>>, req2}

          {:done, body, rest} ->
            {:ok, <<acc::binary, body::binary>>, %{req2 | :multipart => {boundary, rest}}}
        end
    end
  end

  defp init_multipart(req) do
    {"multipart", _, params} = parse_header("content-type", req)

    case :lists.keyfind("boundary", 1, params) do
      {_, boundary} ->
        %{req | :multipart => {boundary, <<>>}}

      false ->
        exit(
          {:request_error, {:multipart, :boundary},
           :"Missing boundary parameter for multipart media type."}
        )
    end
  end

  defp stream_multipart(req = %{:multipart => :done}, _, _) do
    {<<>>, req}
  end

  defp stream_multipart(req = %{:multipart => {_, <<>>}}, opts, type) do
    case read_body(req, opts) do
      {:more, data, req2} ->
        {data, req2}

      {:ok, <<>>, _} ->
        exit({:request_error, {:multipart, type}, :"Malformed body; multipart expected."})

      {:ok, data, req2} ->
        {data, req2}
    end
  end

  defp stream_multipart(req = %{:multipart => {boundary, buffer}}, _, _) do
    {buffer, %{req | :multipart => {boundary, <<>>}}}
  end

  def set_resp_cookie(name, value, req) do
    set_resp_cookie(name, value, req, %{})
  end

  def set_resp_cookie(name, value, req, opts) do
    cookie = :cow_cookie.setcookie(name, value, opts)
    respCookies = :maps.get(:resp_cookies, req, %{})
    %{req | :resp_cookies => %{respCookies | name => cookie}}
  end

  def set_resp_header(name, value, req = %{:resp_headers => respHeaders}) do
    %{req | :resp_headers => %{respHeaders | name => value}}
  end

  def set_resp_header(name, value, req) do
    %{req | :resp_headers => %{name => value}}
  end

  def set_resp_headers(
        headers,
        req = %{:resp_headers => respHeaders}
      ) do
    %{req | :resp_headers => :maps.merge(respHeaders, headers)}
  end

  def set_resp_headers(headers, req) do
    %{req | :resp_headers => headers}
  end

  def resp_header(name, req) do
    resp_header(name, req, :undefined)
  end

  def resp_header(name, %{:resp_headers => headers}, default) do
    :maps.get(name, headers, default)
  end

  def resp_header(_, %{}, default) do
    default
  end

  def resp_headers(%{:resp_headers => respHeaders}) do
    respHeaders
  end

  def resp_headers(%{}) do
    %{}
  end

  def set_resp_body(body, req) do
    %{req | :resp_body => body}
  end

  def has_resp_header(name, %{:resp_headers => respHeaders}) do
    :maps.is_key(name, respHeaders)
  end

  def has_resp_header(_, _) do
    false
  end

  def has_resp_body(%{:resp_body => {:sendfile, _, _, _}}) do
    true
  end

  def has_resp_body(%{:resp_body => respBody}) do
    :erlang.iolist_size(respBody) > 0
  end

  def has_resp_body(_) do
    false
  end

  def delete_resp_header(name, req = %{:resp_headers => respHeaders}) do
    %{req | :resp_headers => :maps.remove(name, respHeaders)}
  end

  def delete_resp_header(_, req) do
    req
  end

  def inform(status, req) do
    inform(status, %{}, req)
  end

  def inform(_, _, %{:has_sent_resp => _}) do
    :erlang.error(:function_clause)
  end

  def inform(status, headers, req)
      when is_integer(status) or
             is_binary(status) do
    cast({:inform, status, headers}, req)
  end

  def reply(status, req) do
    reply(status, %{}, req)
  end

  def reply(status, headers, req = %{:resp_body => body}) do
    reply(status, headers, body, req)
  end

  def reply(status, headers, req) do
    reply(status, headers, <<>>, req)
  end

  def reply(_, _, _, %{:has_sent_resp => _}) do
    :erlang.error(:function_clause)
  end

  def reply(status, headers, {:sendfile, _, 0, _}, req)
      when is_integer(status) or is_binary(status) do
    do_reply(status, %{headers | "content-length" => "0"}, <<>>, req)
  end

  def reply(status, headers, sendFile = {:sendfile, _, len, _}, req)
      when is_integer(status) or is_binary(status) do
    do_reply(
      status,
      %{headers | "content-length" => :erlang.integer_to_binary(len)},
      sendFile,
      req
    )
  end

  def reply(status, headers, body, req)
      when status === 204 or status === 304 do
    0 = :erlang.iolist_size(body)
    do_reply(status, headers, body, req)
  end

  def reply(status = <<"204", _::bits>>, headers, body, req) do
    0 = :erlang.iolist_size(body)
    do_reply(status, headers, body, req)
  end

  def reply(status = <<"304", _::bits>>, headers, body, req) do
    0 = :erlang.iolist_size(body)
    do_reply(status, headers, body, req)
  end

  def reply(status, headers, body, req)
      when is_integer(status) or is_binary(status) do
    do_reply(
      status,
      %{headers | "content-length" => :erlang.integer_to_binary(:erlang.iolist_size(body))},
      body,
      req
    )
  end

  defp do_reply(status, headers, _, req = %{:method => "HEAD"}) do
    cast(
      {:response, status, response_headers(headers, req), <<>>},
      req
    )

    done_replying(req, true)
  end

  defp do_reply(status, headers, body, req) do
    cast(
      {:response, status, response_headers(headers, req), body},
      req
    )

    done_replying(req, true)
  end

  defp done_replying(req, hasSentResp) do
    :maps.without(
      [:resp_cookies, :resp_headers, :resp_body],
      %{req | :has_sent_resp => hasSentResp}
    )
  end

  def stream_reply(status, req) do
    stream_reply(status, %{}, req)
  end

  def stream_reply(_, _, %{:has_sent_resp => _}) do
    :erlang.error(:function_clause)
  end

  def stream_reply(status = 204, headers = %{}, req) do
    reply(status, headers, <<>>, req)
  end

  def stream_reply(status = <<"204", _::bits>>, headers = %{}, req) do
    reply(status, headers, <<>>, req)
  end

  def stream_reply(status = 304, headers = %{}, req) do
    reply(status, headers, <<>>, req)
  end

  def stream_reply(status = <<"304", _::bits>>, headers = %{}, req) do
    reply(status, headers, <<>>, req)
  end

  def stream_reply(status, headers = %{}, req)
      when is_integer(status) or is_binary(status) do
    cast(
      {:headers, status, response_headers(headers, req)},
      req
    )

    done_replying(req, :headers)
  end

  def stream_body(_, _, %{:method => "HEAD", :has_sent_resp => :headers}) do
    :ok
  end

  def stream_body({:sendfile, _, 0, _}, :nofin, _) do
    :ok
  end

  def stream_body({:sendfile, _, 0, _}, isFin = :fin, req = %{:has_sent_resp => :headers}) do
    stream_body({:data, self(), isFin, <<>>}, req)
  end

  def stream_body({:sendfile, o, b, p}, isFin, req = %{:has_sent_resp => :headers})
      when is_integer(o) and o >= 0 and is_integer(b) and
             b > 0 do
    stream_body(
      {:data, self(), isFin, {:sendfile, o, b, p}},
      req
    )
  end

  def stream_body(data, isFin = :nofin, req = %{:has_sent_resp => :headers})
      when not is_tuple(data) do
    case :erlang.iolist_size(data) do
      0 ->
        :ok

      _ ->
        stream_body({:data, self(), isFin, data}, req)
    end
  end

  def stream_body(data, isFin, req = %{:has_sent_resp => :headers})
      when not is_tuple(data) do
    stream_body({:data, self(), isFin, data}, req)
  end

  defp stream_body(msg, req = %{:pid => pid}) do
    cast(msg, req)

    receive do
      {:data_ack, ^pid} ->
        :ok
    end
  end

  def stream_events(event, isFin, req) when is_map(event) do
    stream_events([event], isFin, req)
  end

  def stream_events(events, isFin, req = %{:has_sent_resp => :headers}) do
    stream_body(
      {:data, self(), isFin, :cow_sse.events(events)},
      req
    )
  end

  def stream_trailers(trailers, req = %{:has_sent_resp => :headers}) do
    cast({:trailers, trailers}, req)
  end

  def push(path, headers, req) do
    push(path, headers, req, %{})
  end

  def push(path, headers, req = %{:scheme => scheme0, :host => host0, :port => port0}, opts) do
    method = :maps.get(:method, opts, "GET")
    scheme = :maps.get(:scheme, opts, scheme0)
    host = :maps.get(:host, opts, host0)
    port = :maps.get(:port, opts, port0)
    qs = :maps.get(:qs, opts, <<>>)

    cast(
      {:push, method, scheme, host, port, path, qs, headers},
      req
    )
  end

  def cast(msg, %{:pid => pid, :streamid => streamID}) do
    send(pid, {{pid, streamID}, msg})
    :ok
  end

  def response_headers(headers0, req) do
    respHeaders = :maps.get(:resp_headers, req, %{})

    headers =
      :maps.merge(
        %{"date" => :cowboy_clock.rfc1123(), "server" => "Cowboy"},
        :maps.merge(respHeaders, headers0)
      )

    case :maps.get(:resp_cookies, req, :undefined) do
      :undefined ->
        headers

      respCookies ->
        %{headers | "set-cookie" => :maps.values(respCookies)}
    end
  end

  defp kvlist_to_map(fields, kvList) do
    keys =
      for k <- fields do
        case k do
          {key, _} ->
            key

          {key, _, _} ->
            key

          key ->
            key
        end
      end

    kvlist_to_map(keys, kvList, %{})
  end

  defp kvlist_to_map(_, [], map) do
    map
  end

  defp kvlist_to_map(keys, [{key, value} | tail], map) do
    try do
      :erlang.binary_to_existing_atom(key, :utf8)
    catch
      :error, :badarg ->
        kvlist_to_map(keys, tail, map)
    else
      atom ->
        case :lists.member(atom, keys) do
          true ->
            case :maps.find(atom, map) do
              {:ok, mapValue} when is_list(mapValue) ->
                kvlist_to_map(keys, tail, %{map | atom => [value | mapValue]})

              {:ok, mapValue} ->
                kvlist_to_map(keys, tail, %{map | atom => [value, mapValue]})

              :error ->
                kvlist_to_map(keys, tail, %{map | atom => value})
            end

          false ->
            kvlist_to_map(keys, tail, map)
        end
    end
  end

  defp filter(fields, map0) do
    filter(fields, map0, %{})
  end

  defp filter([], map, errors) do
    case :maps.size(errors) do
      0 ->
        {:ok, map}

      _ ->
        {:error, errors}
    end
  end

  defp filter([{key, constraints} | tail], map, errors) do
    filter_constraints(tail, map, errors, key, :maps.get(key, map), constraints)
  end

  defp filter([{key, constraints, default} | tail], map, errors) do
    case :maps.find(key, map) do
      {:ok, value} ->
        filter_constraints(tail, map, errors, key, value, constraints)

      :error ->
        filter(tail, %{map | key => default}, errors)
    end
  end

  defp filter([key | tail], map, errors) do
    case :maps.is_key(key, map) do
      true ->
        filter(tail, map, errors)

      false ->
        filter(tail, map, %{errors | key => :required})
    end
  end

  defp filter_constraints(tail, map, errors, key, value0, constraints) do
    case :cowboy_constraints.validate(
           value0,
           constraints
         ) do
      {:ok, value} ->
        filter(tail, %{map | key => value}, errors)

      {:error, reason} ->
        filter(tail, map, %{errors | key => reason})
    end
  end
end
