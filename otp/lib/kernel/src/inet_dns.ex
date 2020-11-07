defmodule :m_inet_dns do
  use Bitwise
  import :lists, only: [reverse: 1]
  require Record
  Record.defrecord(:r_connect_opts, :connect_opts, ifaddr: :any, port: 0, fd: -1, opts: [])

  Record.defrecord(:r_listen_opts, :listen_opts,
    ifaddr: :any,
    port: 0,
    backlog: 5,
    fd: -1,
    opts: []
  )

  Record.defrecord(:r_udp_opts, :udp_opts, ifaddr: :any, port: 0, fd: -1, opts: [{:active, true}])

  Record.defrecord(:r_sctp_opts, :sctp_opts,
    ifaddr: :undefined,
    port: 0,
    fd: -1,
    type: :seqpacket,
    opts: [
      {:mode, :binary},
      {:buffer, 65536},
      {:sndbuf, 65536},
      {:recbuf, 1024},
      {:sctp_events, :undefined}
    ]
  )

  Record.defrecord(:r_dns_header, :dns_header,
    id: 0,
    qr: 0,
    opcode: 0,
    aa: 0,
    tc: 0,
    rd: 0,
    ra: 0,
    pr: 0,
    rcode: 0
  )

  Record.defrecord(:r_dns_rec, :dns_rec,
    header: :undefined,
    qdlist: [],
    anlist: [],
    nslist: [],
    arlist: []
  )

  Record.defrecord(:r_dns_rr, :dns_rr,
    domain: '',
    type: :any,
    class: :in,
    cnt: 0,
    ttl: 0,
    data: [],
    tm: :undefined,
    bm: [],
    func: false
  )

  Record.defrecord(:r_dns_rr_opt, :dns_rr_opt,
    domain: '',
    type: :opt,
    udp_payload_size: 1280,
    ext_rcode: 0,
    version: 0,
    z: 0,
    data: []
  )

  Record.defrecord(:r_dns_query, :dns_query,
    domain: :undefined,
    type: :undefined,
    class: :undefined
  )

  def record_type(r_dns_rr()) do
    :rr
  end

  def record_type(r_dns_rr_opt()) do
    :rr
  end

  def record_type(rec) do
    record_adts(rec)
  end

  def rr(r_dns_rr() = rR) do
    dns_rr(rR)
  end

  def rr(r_dns_rr_opt() = rR) do
    dns_rr_opt(rR)
  end

  def rr(r_dns_rr() = rR, l) do
    dns_rr(rR, l)
  end

  def rr(r_dns_rr_opt() = rR, l) do
    dns_rr_opt(rR, l)
  end

  def make_rr() do
    make_dns_rr()
  end

  def make_rr(l) when is_list(l) do
    case rr_type(l, :any) do
      :opt ->
        make_dns_rr_opt(l)

      _ ->
        make_dns_rr(l)
    end
  end

  def make_rr(:type, :opt) do
    make_dns_rr_opt()
  end

  def make_rr(f, v) when is_atom(f) do
    make_dns_rr(f, v)
  end

  def make_rr(r_dns_rr() = rR, l) when is_list(l) do
    case rr_type(l, r_dns_rr(rR, :type)) do
      :opt ->
        ts = common_fields__rr__rr_opt()

        make_dns_rr_opt(
          for {t, _} = opt <- dns_rr(rR),
              lists_member(t, ts) do
            opt
          end ++ l
        )

      _ ->
        make_dns_rr(rR, l)
    end
  end

  def make_rr(r_dns_rr_opt() = rR, l) when is_list(l) do
    case rr_type(l, r_dns_rr_opt(rR, :type)) do
      :opt ->
        make_dns_rr_opt(rR, l)

      _ ->
        ts = common_fields__rr__rr_opt()

        make_dns_rr(
          for {t, _} = opt <- dns_rr_opt(rR),
              lists_member(t, ts) do
            opt
          end ++ l
        )
    end
  end

  def make_rr(r_dns_rr() = rR, :type, :opt) do
    make_rr(rR, [{:type, :opt}])
  end

  def make_rr(r_dns_rr() = rR, f, v) do
    make_dns_rr(rR, f, v)
  end

  def make_rr(r_dns_rr_opt() = rR, :type, :opt) do
    rR
  end

  def make_rr(r_dns_rr_opt() = rR, :type, t) do
    make_rr(rR, [{:type, t}])
  end

  def make_rr(r_dns_rr_opt() = rR, f, v) do
    make_dns_rr_opt(rR, f, v)
  end

  defp rr_type([], t) do
    t
  end

  defp rr_type([{:type, t} | opts], _) do
    rr_type(opts, t)
  end

  defp rr_type([_ | opts], t) do
    rr_type(opts, t)
  end

  defp common_fields__rr__rr_opt() do
    for t <- Keyword.keys(r_dns_rr_opt(r_dns_rr_opt())),
        lists_member(t, Keyword.keys(r_dns_rr(r_dns_rr()))) do
      t
    end
  end

  defp lists_member(_, []) do
    false
  end

  defp lists_member(h, [h | _]) do
    true
  end

  defp lists_member(h, [_ | t]) do
    lists_member(h, t)
  end

  def decode(buffer) when is_binary(buffer) do
    try do
      do_decode(buffer)
    catch
      reason ->
        {:error, reason}
    else
      dnsRec ->
        {:ok, dnsRec}
    end
  end

  defp do_decode(
         <<id::size(16), qR::size(1), opcode::size(4), aA::size(1), tC::size(1), rD::size(1),
           rA::size(1), pR::size(1), _::size(2), rcode::size(4), qdCount::size(16),
           anCount::size(16), nsCount::size(16), arCount::size(16), qdBuf::binary>> = buffer
       ) do
    {anBuf, qdList, qdTC} = decode_query_section(qdBuf, qdCount, buffer)
    {nsBuf, anList, anTC} = decode_rr_section(anBuf, anCount, buffer)
    {arBuf, nsList, nsTC} = decode_rr_section(nsBuf, nsCount, buffer)
    {rest, arList, arTC} = decode_rr_section(arBuf, arCount, buffer)

    case rest do
      <<>> ->
        hdrTC = decode_boolean(tC)

        dnsHdr =
          r_dns_header(
            id: id,
            qr: decode_boolean(qR),
            opcode: decode_opcode(opcode),
            aa: decode_boolean(aA),
            tc: hdrTC,
            rd: decode_boolean(rD),
            ra: decode_boolean(rA),
            pr: decode_boolean(pR),
            rcode: rcode
          )

        case :erlang.or(
               :erlang.or(
                 :erlang.or(qdTC, anTC),
                 nsTC
               ),
               arTC
             ) do
          true when not hdrTC ->
            throw(:formerr)

          _ ->
            r_dns_rec(
              header: dnsHdr,
              qdlist: qdList,
              anlist: anList,
              nslist: nsList,
              arlist: arList
            )
        end

      _ ->
        throw(:formerr)
    end
  end

  defp do_decode(_) do
    throw(:formerr)
  end

  defp decode_query_section(bin, n, buffer) do
    decode_query_section(bin, n, buffer, [])
  end

  defp decode_query_section(<<>> = rest, n, _Buffer, qs) do
    {rest, reverse(qs), n !== 0}
  end

  defp decode_query_section(rest, 0, _Buffer, qs) do
    {rest, reverse(qs), false}
  end

  defp decode_query_section(bin, n, buffer, qs) do
    case decode_name(bin, buffer) do
      {<<type::size(16), class::size(16), rest::binary>>, name} ->
        dnsQuery = r_dns_query(domain: name, type: decode_type(type), class: decode_class(class))
        decode_query_section(rest, n - 1, buffer, [dnsQuery | qs])

      _ ->
        throw(:formerr)
    end
  end

  defp decode_rr_section(bin, n, buffer) do
    decode_rr_section(bin, n, buffer, [])
  end

  defp decode_rr_section(<<>> = rest, n, _Buffer, rRs) do
    {rest, reverse(rRs), n !== 0}
  end

  defp decode_rr_section(rest, 0, _Buffer, rRs) do
    {rest, reverse(rRs), false}
  end

  defp decode_rr_section(bin, n, buffer, rRs) do
    case decode_name(bin, buffer) do
      {<<t::size(16)-unsigned, c::size(16)-unsigned, tTL::size(4)-binary, len::size(16),
         d::size(len)-binary, rest::binary>>, name} ->
        type = decode_type(t)
        class = decode_class(c)
        data = decode_data(d, class, type, buffer)

        rR =
          case type do
            :opt ->
              <<extRcode, version, z::size(16)>> = tTL

              r_dns_rr_opt(
                domain: name,
                type: type,
                udp_payload_size: c,
                ext_rcode: extRcode,
                version: version,
                z: z,
                data: data
              )

            _ ->
              <<timeToLive::size(32)-signed>> = tTL

              r_dns_rr(
                domain: name,
                type: type,
                class: class,
                ttl:
                  cond do
                    timeToLive < 0 ->
                      0

                    true ->
                      timeToLive
                  end,
                data: data
              )
          end

        decode_rr_section(rest, n - 1, buffer, [rR | rRs])

      _ ->
        throw(:formerr)
    end
  end

  def encode(q) do
    qdCount = length(r_dns_rec(q, :qdlist))
    anCount = length(r_dns_rec(q, :anlist))
    nsCount = length(r_dns_rec(q, :nslist))
    arCount = length(r_dns_rec(q, :arlist))
    b0 = encode_header(r_dns_rec(q, :header), qdCount, anCount, nsCount, arCount)
    c0 = :gb_trees.empty()
    {b1, c1} = encode_query_section(b0, c0, r_dns_rec(q, :qdlist))
    {b2, c2} = encode_res_section(b1, c1, r_dns_rec(q, :anlist))
    {b3, c3} = encode_res_section(b2, c2, r_dns_rec(q, :nslist))
    {b, _} = encode_res_section(b3, c3, r_dns_rec(q, :arlist))
    b
  end

  defp encode_header(r_dns_header(id: id) = h, qdCount, anCount, nsCount, arCount) do
    qR = encode_boolean(r_dns_header(h, :qr))
    opcode = encode_opcode(r_dns_header(h, :opcode))
    aA = encode_boolean(r_dns_header(h, :aa))
    tC = encode_boolean(r_dns_header(h, :tc))
    rD = encode_boolean(r_dns_header(h, :rd))
    rA = encode_boolean(r_dns_header(h, :ra))
    pR = encode_boolean(r_dns_header(h, :pr))
    rcode = r_dns_header(h, :rcode)

    <<id::size(16), qR::size(1), opcode::size(4), aA::size(1), tC::size(1), rD::size(1),
      rA::size(1), pR::size(1), 0::size(2), rcode::size(4), qdCount::size(16), anCount::size(16),
      nsCount::size(16), arCount::size(16)>>
  end

  defp encode_query_section(bin, comp, []) do
    {bin, comp}
  end

  defp encode_query_section(bin0, comp0, [r_dns_query(domain: dName) = q | qs]) do
    type = encode_type(r_dns_query(q, :type))
    class = encode_class(r_dns_query(q, :class))
    {bin, comp} = encode_name(bin0, comp0, byte_size(bin0), dName)
    encode_query_section(<<bin::binary, type::size(16), class::size(16)>>, comp, qs)
  end

  defp encode_res_section(bin, comp, []) do
    {bin, comp}
  end

  defp encode_res_section(bin, comp, [
         r_dns_rr(domain: dName, type: type, class: class, ttl: tTL, data: data)
         | rs
       ]) do
    encode_res_section_rr(bin, comp, rs, dName, type, class, <<tTL::size(32)-signed>>, data)
  end

  defp encode_res_section(bin, comp, [
         r_dns_rr_opt(
           domain: dName,
           udp_payload_size: udpPayloadSize,
           ext_rcode: extRCode,
           version: version,
           z: z,
           data: data
         )
         | rs
       ]) do
    encode_res_section_rr(
      bin,
      comp,
      rs,
      dName,
      :opt,
      udpPayloadSize,
      <<extRCode, version, z::size(16)>>,
      data
    )
  end

  defp encode_res_section_rr(bin0, comp0, rs, dName, type, class, tTL, data) do
    t = encode_type(type)
    c = encode_class(class)
    {bin, comp1} = encode_name(bin0, comp0, byte_size(bin0), dName)

    {dataBin, comp} =
      encode_data(comp1, byte_size(bin) + 2 + 2 + byte_size(tTL) + 2, type, class, data)

    dataSize = byte_size(dataBin)

    encode_res_section(
      <<bin::binary, t::size(16), c::size(16), tTL::binary, dataSize::size(16), dataBin::binary>>,
      comp,
      rs
    )
  end

  defp decode_type(type) do
    case type do
      1 ->
        :a

      2 ->
        :ns

      3 ->
        :md

      4 ->
        :mf

      5 ->
        :cname

      6 ->
        :soa

      7 ->
        :mb

      8 ->
        :mg

      9 ->
        :mr

      10 ->
        :null

      11 ->
        :wks

      12 ->
        :ptr

      13 ->
        :hinfo

      14 ->
        :minfo

      15 ->
        :mx

      16 ->
        :txt

      28 ->
        :aaaa

      33 ->
        :srv

      35 ->
        :naptr

      41 ->
        :opt

      99 ->
        :spf

      100 ->
        :uinfo

      101 ->
        :uid

      102 ->
        :gid

      103 ->
        :unspec

      252 ->
        :axfr

      253 ->
        :mailb

      254 ->
        :maila

      255 ->
        :any

      _ ->
        type
    end
  end

  defp encode_type(type) do
    case type do
      :a ->
        1

      :ns ->
        2

      :md ->
        3

      :mf ->
        4

      :cname ->
        5

      :soa ->
        6

      :mb ->
        7

      :mg ->
        8

      :mr ->
        9

      :null ->
        10

      :wks ->
        11

      :ptr ->
        12

      :hinfo ->
        13

      :minfo ->
        14

      :mx ->
        15

      :txt ->
        16

      :aaaa ->
        28

      :srv ->
        33

      :naptr ->
        35

      :opt ->
        41

      :spf ->
        99

      :uinfo ->
        100

      :uid ->
        101

      :gid ->
        102

      :unspec ->
        103

      :axfr ->
        252

      :mailb ->
        253

      :maila ->
        254

      :any ->
        255

      ^type when is_integer(type) ->
        type
    end
  end

  defp decode_class(class) do
    case class do
      1 ->
        :in

      3 ->
        :chaos

      4 ->
        :hs

      255 ->
        :any

      _ ->
        class
    end
  end

  defp encode_class(class) do
    case class do
      :in ->
        1

      :chaos ->
        3

      :hs ->
        4

      :any ->
        255

      ^class when is_integer(class) ->
        class
    end
  end

  defp decode_opcode(opcode) do
    case opcode do
      0 ->
        :query

      1 ->
        :iquery

      2 ->
        :status

      _ when is_integer(opcode) ->
        opcode
    end
  end

  defp encode_opcode(opcode) do
    case opcode do
      :query ->
        0

      :iquery ->
        1

      :status ->
        2

      _ when is_integer(opcode) ->
        opcode
    end
  end

  defp encode_boolean(true) do
    1
  end

  defp encode_boolean(false) do
    0
  end

  defp encode_boolean(b) when is_integer(b) do
    b
  end

  defp decode_boolean(0) do
    false
  end

  defp decode_boolean(i) when is_integer(i) do
    true
  end

  defp decode_data(<<a, b, c, d>>, :in, :a, _) do
    {a, b, c, d}
  end

  defp decode_data(
         <<a::size(16), b::size(16), c::size(16), d::size(16), e::size(16), f::size(16),
           g::size(16), h::size(16)>>,
         :in,
         :aaaa,
         _
       ) do
    {a, b, c, d, e, f, g, h}
  end

  defp decode_data(dom, _, :ns, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(dom, _, :md, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(dom, _, :mf, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(dom, _, :cname, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(data0, _, :soa, buffer) do
    {data1, mName} = decode_name(data0, buffer)
    {data, rName} = decode_name(data1, buffer)

    case data do
      <<serial::size(32), refresh::size(32)-signed, retry::size(32)-signed,
        expiry::size(32)-signed, minimum::size(32)>> ->
        {mName, rName, serial, refresh, retry, expiry, minimum}

      _ ->
        throw(:formerr)
    end
  end

  defp decode_data(dom, _, :mb, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(dom, _, :mg, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(dom, _, :mr, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(data, _, :null, _) do
    data
  end

  defp decode_data(<<a, b, c, d, proto, bitMap::binary>>, :in, :wks, _Buffer) do
    {{a, b, c, d}, proto, bitMap}
  end

  defp decode_data(dom, _, :ptr, buffer) do
    decode_domain(dom, buffer)
  end

  defp decode_data(
         <<cpuLen, cPU::size(cpuLen)-binary, osLen, oS::size(osLen)-binary>>,
         _,
         :hinfo,
         _
       ) do
    {:erlang.binary_to_list(cPU), :erlang.binary_to_list(oS)}
  end

  defp decode_data(data0, _, :minfo, buffer) do
    {data1, rM} = decode_name(data0, buffer)
    {data, eM} = decode_name(data1, buffer)

    case data do
      <<>> ->
        {rM, eM}

      _ ->
        throw(:formerr)
    end
  end

  defp decode_data(<<prio::size(16), dom::binary>>, _, :mx, buffer) do
    {prio, decode_domain(dom, buffer)}
  end

  defp decode_data(
         <<prio::size(16), weight::size(16), port::size(16), dom::binary>>,
         _,
         :srv,
         buffer
       ) do
    {prio, weight, port, decode_domain(dom, buffer)}
  end

  defp decode_data(<<order::size(16), preference::size(16), data0::binary>>, _, :naptr, buffer) do
    {data1, flags} = decode_string(data0)
    {data2, services} = decode_string(data1)
    {data, regexp} = decode_characters(data2, :utf8)
    replacement = decode_domain(data, buffer)

    {order, preference, :string.lowercase(flags), :string.lowercase(services), regexp,
     replacement}
  end

  defp decode_data(data, _, :txt, _) do
    decode_txt(data)
  end

  defp decode_data(data, _, :spf, _) do
    decode_txt(data)
  end

  defp decode_data(data, _, _, _) do
    data
  end

  defp decode_txt(<<>>) do
    []
  end

  defp decode_txt(bin) do
    {rest, string} = decode_string(bin)
    [string | decode_txt(rest)]
  end

  defp decode_string(<<len, bin::size(len)-binary, rest::binary>>) do
    {rest, :erlang.binary_to_list(bin)}
  end

  defp decode_string(_) do
    throw(:formerr)
  end

  defp decode_characters(
         <<len, bin::size(len)-binary, rest::binary>>,
         encoding
       ) do
    {rest, :unicode.characters_to_list(bin, encoding)}
  end

  defp decode_characters(_, _) do
    throw(:formerr)
  end

  defp decode_domain(bin, buffer) do
    case decode_name(bin, buffer) do
      {<<>>, name} ->
        name

      _ ->
        throw(:formerr)
    end
  end

  defp decode_name(bin, buffer) do
    decode_name(bin, buffer, [], bin, 0)
  end

  defp decode_name(_, buffer, _Labels, _Tail, cnt)
       when cnt > byte_size(buffer) do
    throw(:formerr)
  end

  defp decode_name(<<0, rest::binary>>, _Buffer, labels, tail, cnt) do
    {cond do
       cnt !== 0 ->
         tail

       true ->
         rest
     end, decode_name_labels(labels)}
  end

  defp decode_name(
         <<0::size(2), len::size(6), label::size(len)-binary, rest::binary>>,
         buffer,
         labels,
         tail,
         cnt
       ) do
    decode_name(
      rest,
      buffer,
      [label | labels],
      cond do
        cnt !== 0 ->
          tail

        true ->
          rest
      end,
      cnt
    )
  end

  defp decode_name(<<3::size(2), ptr::size(14), rest::binary>>, buffer, labels, tail, cnt) do
    case buffer do
      <<_::size(ptr)-binary, bin::binary>> ->
        decode_name(
          bin,
          buffer,
          labels,
          cond do
            cnt !== 0 ->
              tail

            true ->
              rest
          end,
          cnt + 2
        )

      _ ->
        throw(:formerr)
    end
  end

  defp decode_name(_, _, _, _, _) do
    throw(:formerr)
  end

  defp decode_name_labels([]) do
    '.'
  end

  defp decode_name_labels(labels) do
    decode_name_labels(labels, '')
  end

  defp decode_name_labels([label], name) do
    decode_name_label(label, name)
  end

  defp decode_name_labels([label | labels], name) do
    decode_name_labels(
      labels,
      '.' ++ decode_name_label(label, name)
    )
  end

  defp decode_name_label(<<>>, _Name) do
    throw(:formerr)
  end

  defp decode_name_label(label, name) do
    decode_name_label(label, name, byte_size(label))
  end

  defp decode_name_label(_, name, 0) do
    name
  end

  defp decode_name_label(label, name, n) do
    m = n - 1

    case label do
      <<_::size(m)-binary, ?\\, _::binary>> ->
        decode_name_label(label, '\\\\' ++ name, m)

      <<_::size(m)-binary, ?., _::binary>> ->
        decode_name_label(label, '\\.' ++ name, m)

      <<_::size(m)-binary, c, _::binary>> ->
        decode_name_label(label, [c | name], m)

      _ ->
        :erlang.error(:badarg, [label, name, n])
    end
  end

  defp encode_data(comp, _, :a, :in, {a, b, c, d}) do
    {<<a, b, c, d>>, comp}
  end

  defp encode_data(comp, _, :aaaa, :in, {a, b, c, d, e, f, g, h}) do
    {<<a::size(16), b::size(16), c::size(16), d::size(16), e::size(16), f::size(16), g::size(16),
       h::size(16)>>, comp}
  end

  defp encode_data(comp, pos, :ns, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, pos, :md, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, pos, :mf, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, pos, :cname, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp0, pos, :soa, :in, {mName, rName, serial, refresh, retry, expiry, minimum}) do
    {b1, comp1} = encode_name(comp0, pos, mName)
    {b, comp} = encode_name(b1, comp1, pos + byte_size(b1), rName)

    {<<b::binary, serial::size(32), refresh::size(32)-signed, retry::size(32)-signed,
       expiry::size(32)-signed, minimum::size(32)>>, comp}
  end

  defp encode_data(comp, pos, :mb, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, pos, :mg, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, pos, :mr, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, _, :null, :in, data) do
    {:erlang.iolist_to_binary(data), comp}
  end

  defp encode_data(comp, _, :wks, :in, {{a, b, c, d}, proto, bitMap}) do
    bitMapBin = :erlang.iolist_to_binary(bitMap)
    {<<a, b, c, d, proto, bitMapBin::binary>>, comp}
  end

  defp encode_data(comp, pos, :ptr, :in, domain) do
    encode_name(comp, pos, domain)
  end

  defp encode_data(comp, _, :hinfo, :in, {cPU, oS}) do
    bin = encode_string(:erlang.iolist_to_binary(cPU))
    {encode_string(bin, :erlang.iolist_to_binary(oS)), comp}
  end

  defp encode_data(comp0, pos, :minfo, :in, {rM, eM}) do
    {bin, comp} = encode_name(comp0, pos, rM)
    encode_name(bin, comp, pos + byte_size(bin), eM)
  end

  defp encode_data(comp, pos, :mx, :in, {pref, exch}) do
    encode_name(<<pref::size(16)>>, comp, pos + 2, exch)
  end

  defp encode_data(comp, pos, :srv, :in, {prio, weight, port, target}) do
    encode_name(
      <<prio::size(16), weight::size(16), port::size(16)>>,
      comp,
      pos + 2 + 2 + 2,
      target
    )
  end

  defp encode_data(
         comp,
         pos,
         :naptr,
         :in,
         {order, preference, flags, services, regexp, replacement}
       ) do
    b0 = <<order::size(16), preference::size(16)>>
    b1 = encode_string(b0, :erlang.iolist_to_binary(flags))

    b2 =
      encode_string(
        b1,
        :erlang.iolist_to_binary(services)
      )

    b3 =
      encode_string(
        b2,
        :unicode.characters_to_binary(regexp, :unicode, :utf8)
      )

    {b, _} = encode_name(b3, :gb_trees.empty(), pos + byte_size(b3), replacement)
    {b, comp}
  end

  defp encode_data(comp, _, :txt, :in, data) do
    {encode_txt(data), comp}
  end

  defp encode_data(comp, _, :spf, :in, data) do
    {encode_txt(data), comp}
  end

  defp encode_data(comp, _Pos, _Type, _Class, data) do
    {:erlang.iolist_to_binary(data), comp}
  end

  defp encode_txt(strings) do
    encode_txt(<<>>, strings)
  end

  defp encode_txt(bin, []) do
    bin
  end

  defp encode_txt(bin, [s | ss]) do
    encode_txt(
      encode_string(
        bin,
        :erlang.iolist_to_binary(s)
      ),
      ss
    )
  end

  defp encode_string(stringBin) do
    encode_string(<<>>, stringBin)
  end

  defp encode_string(bin, stringBin) do
    size = byte_size(stringBin)

    cond do
      size <= 255 ->
        <<bin::binary, size, stringBin::binary>>
    end
  end

  defp encode_name(comp, pos, name) do
    encode_name(<<>>, comp, pos, name)
  end

  defp encode_name(bin0, comp0, pos, name) do
    case encode_labels(bin0, comp0, pos, name2labels(name)) do
      {bin, _} = result
      when byte_size(bin) - byte_size(bin0) <= 255 ->
        result

      _ ->
        :erlang.error(:badarg, [bin0, comp0, pos, name])
    end
  end

  defp name2labels('') do
    []
  end

  defp name2labels('.') do
    []
  end

  defp name2labels(cs) do
    name2labels(<<>>, cs)
  end

  defp name2labels(label, '') do
    [label]
  end

  defp name2labels(label, '.') do
    [label]
  end

  defp name2labels(label, '.' ++ cs) do
    [label | name2labels(<<>>, cs)]
  end

  defp name2labels(label, '\\' ++ [c | cs]) do
    name2labels(<<label::binary, c>>, cs)
  end

  defp name2labels(label, [c | cs]) do
    name2labels(<<label::binary, c>>, cs)
  end

  defp encode_labels(bin, comp, _Pos, []) do
    {<<bin::binary, 0>>, comp}
  end

  defp encode_labels(bin, comp0, pos, [l | ls] = labels)
       when 1 <= byte_size(l) and byte_size(l) <= 63 do
    case :gb_trees.lookup(labels, comp0) do
      :none ->
        comp =
          cond do
            pos < 3 <<< 14 ->
              :gb_trees.insert(labels, pos, comp0)

            true ->
              comp0
          end

        size = byte_size(l)
        encode_labels(<<bin::binary, size, l::binary>>, comp, pos + 1 + size, ls)

      {:value, ptr} ->
        {<<bin::binary, 3::size(2), ptr::size(14)>>, comp0}
    end
  end
end
