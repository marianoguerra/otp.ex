defmodule :m_otp_internal do
  use Bitwise

  def obsolete(:auth, :cookie, 0) do
    {:deprecated, 'use erlang:get_cookie/0 instead'}
  end

  def obsolete(:auth, :cookie, 1) do
    {:deprecated, 'use erlang:set_cookie/2 instead'}
  end

  def obsolete(:auth, :is_auth, 1) do
    {:deprecated, 'use net_adm:ping/1 instead'}
  end

  def obsolete(:disk_log, :accessible_logs, 0) do
    {:deprecated, 'use disk_log:all/0 instead'}
  end

  def obsolete(:disk_log, :lclose, 1) do
    {:deprecated, 'use disk_log:close/1 instead'}
  end

  def obsolete(:disk_log, :lclose, 2) do
    {:deprecated, 'use disk_log:close/1 instead'}
  end

  def obsolete(:calendar, :local_time_to_universal_time, 1) do
    {:deprecated, 'use calendar:local_time_to_universal_time_dst/1 instead'}
  end

  def obsolete(:code, :rehash, 0) do
    {:deprecated, 'the code path cache feature has been removed'}
  end

  def obsolete(:crypto, :block_decrypt, 3) do
    {:deprecated,
     'use crypto:crypto_one_time/4 or crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1 instead',
     'OTP 24'}
  end

  def obsolete(:crypto, :block_decrypt, 4) do
    {:deprecated,
     'use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 or crypto:crypto_(dyn_iv)?_init + crypto:crypto_(dyn_iv)?_update + crypto:crypto_final instead',
     'OTP 24'}
  end

  def obsolete(:crypto, :block_encrypt, 3) do
    {:deprecated,
     'use crypto:crypto_one_time/4 or crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1 instead',
     'OTP 24'}
  end

  def obsolete(:crypto, :block_encrypt, 4) do
    {:deprecated,
     'use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 or crypto:crypto_(dyn_iv)?_init + crypto:crypto_(dyn_iv)?_update + crypto:crypto_final instead',
     'OTP 24'}
  end

  def obsolete(:crypto, :cmac, 3) do
    {:deprecated, 'use crypto:mac/4 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :cmac, 4) do
    {:deprecated, 'use crypto:macN/5 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :hmac, 3) do
    {:deprecated, 'use crypto:mac/4 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :hmac, 4) do
    {:deprecated, 'use crypto:macN/5 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :hmac_final, 1) do
    {:deprecated, 'use crypto:mac_final/1 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :hmac_final_n, 2) do
    {:deprecated, 'use crypto:mac_finalN/2 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :hmac_init, 2) do
    {:deprecated, 'use crypto:mac_init/3 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :hmac_update, 2) do
    {:deprecated, 'use crypto:mac_update/2 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :poly1305, 2) do
    {:deprecated, 'use crypto:mac/3 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :rand_uniform, 2) do
    {:deprecated, 'use rand:uniform/1 instead'}
  end

  def obsolete(:crypto, :stream_decrypt, 2) do
    {:deprecated, 'use crypto:crypto_update/2 instead', 'OTP 24'}
  end

  def obsolete(:crypto, :stream_encrypt, 2) do
    {:deprecated, 'use crypto:crypto_update/2 instead', 'OTP 24'}
  end

  def obsolete(:erlang, :now, 0) do
    {:deprecated,
     'see the "Time and Time Correction in Erlang" chapter of the ERTS User\'s Guide for more information'}
  end

  def obsolete(:filename, :safe_relative_path, 1) do
    {:deprecated, 'use filelib:safe_relative_path/2 instead', 'OTP 25'}
  end

  def obsolete(:http_uri, :decode, 1) do
    {:deprecated, 'use uri_string functions instead', 'OTP 25'}
  end

  def obsolete(:http_uri, :encode, 1) do
    {:deprecated, 'use uri_string functions instead', 'OTP 25'}
  end

  def obsolete(:http_uri, :parse, 1) do
    {:deprecated, 'use uri_string functions instead', 'OTP 25'}
  end

  def obsolete(:http_uri, :parse, 2) do
    {:deprecated, 'use uri_string functions instead', 'OTP 25'}
  end

  def obsolete(:http_uri, :scheme_defaults, 0) do
    {:deprecated, 'use uri_string functions instead', 'OTP 25'}
  end

  def obsolete(:httpd, :parse_query, 1) do
    {:deprecated, 'use uri_string:dissect_query/1 instead'}
  end

  def obsolete(:httpd_util, :flatlength, 1) do
    {:deprecated, 'use erlang:iolist_size/1 instead', 'OTP 26'}
  end

  def obsolete(:httpd_util, :hexlist_to_integer, 1) do
    {:deprecated, 'use erlang:list_to_integer/2 with base 16 instead', 'OTP 26'}
  end

  def obsolete(:httpd_util, :integer_to_hexlist, 1) do
    {:deprecated, 'use erlang:integer_to_list/2 with base 16 instead', 'OTP 26'}
  end

  def obsolete(:httpd_util, :strip, 1) do
    {:deprecated, 'use string:trim/1 instead', 'OTP 26'}
  end

  def obsolete(:httpd_util, :suffix, 1) do
    {:deprecated, 'use filename:extension/1 and string:trim/2 instead', 'OTP 26'}
  end

  def obsolete(:megaco, :format_versions, 1) do
    {:deprecated, 'use megaco:print_version_info/0,1 instead.', 'OTP 24'}
  end

  def obsolete(:net, :broadcast, 3) do
    {:deprecated, 'use rpc:eval_everywhere/3 instead'}
  end

  def obsolete(:net, :call, 4) do
    {:deprecated, 'use rpc:call/4 instead'}
  end

  def obsolete(:net, :cast, 4) do
    {:deprecated, 'use rpc:cast/4 instead'}
  end

  def obsolete(:net, :ping, 1) do
    {:deprecated, 'use net_adm:ping/1 instead'}
  end

  def obsolete(:net, :relay, 1) do
    {:deprecated, 'use slave:relay/1 instead'}
  end

  def obsolete(:net, :sleep, 1) do
    {:deprecated, 'use \'receive after T -> ok end\' instead'}
  end

  def obsolete(:queue, :lait, 1) do
    {:deprecated, 'use queue:liat/1 instead'}
  end

  def obsolete(:snmp, :add_agent_caps, 2) do
    {:deprecated, 'use snmpa:add_agent_caps/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :c, 1) do
    {:deprecated, 'use snmpa:c/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :c, 2) do
    {:deprecated, 'use snmpa:c/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :change_log_size, 1) do
    {:deprecated, 'use snmpa:change_log_size/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :compile, 3) do
    {:deprecated, 'use snmpa:compile/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :current_address, 0) do
    {:deprecated, 'use snmpa:current_address/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :current_community, 0) do
    {:deprecated, 'use snmpa:current_community/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :current_context, 0) do
    {:deprecated, 'use snmpa:current_context/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :current_net_if_data, 0) do
    {:deprecated, 'use snmpa:current_net_if_data/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :current_request_id, 0) do
    {:deprecated, 'use snmpa:current_request_id/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :del_agent_caps, 1) do
    {:deprecated, 'use snmpa:del_agent_caps/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :dump_mibs, 0) do
    {:deprecated, 'use snmpa:dump_mibs/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :dump_mibs, 1) do
    {:deprecated, 'use snmpa:dump_mibs/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :enum_to_int, 2) do
    {:deprecated, 'use snmpa:enum_to_int/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :enum_to_int, 3) do
    {:deprecated, 'use snmpa:enum_to_int/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :get, 2) do
    {:deprecated, 'use snmpa:get/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :get_agent_caps, 0) do
    {:deprecated, 'use snmpa:get_agent_caps/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :get_symbolic_store_db, 0) do
    {:deprecated, 'use snmpa:get_symbolic_store_db/0 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :info, 1) do
    {:deprecated, 'use snmpa:info/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :int_to_enum, 2) do
    {:deprecated, 'use snmpa:int_to_enum/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :int_to_enum, 3) do
    {:deprecated, 'use snmpa:int_to_enum/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :is_consistent, 1) do
    {:deprecated, 'use snmpa:is_consistent/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :load_mibs, 2) do
    {:deprecated, 'use snmpa:load_mibs/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :log_to_txt, 2) do
    {:deprecated, 'use snmpa:log_to_txt/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :log_to_txt, 3) do
    {:deprecated, 'use snmpa:log_to_txt/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :log_to_txt, 4) do
    {:deprecated, 'use snmpa:log_to_txt/4 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :mib_to_hrl, 1) do
    {:deprecated, 'use snmpa:mib_to_hrl/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :name_to_oid, 1) do
    {:deprecated, 'use snmpa:name_to_oid/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :name_to_oid, 2) do
    {:deprecated, 'use snmpa:name_to_oid/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :oid_to_name, 1) do
    {:deprecated, 'use snmpa:oid_to_name/1 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :oid_to_name, 2) do
    {:deprecated, 'use snmpa:oid_to_name/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :register_subagent, 3) do
    {:deprecated, 'use snmpa:register_subagent/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :send_notification, 3) do
    {:deprecated, 'use snmpa:send_notification/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :send_notification, 4) do
    {:deprecated, 'use snmpa:send_notification/4 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :send_notification, 5) do
    {:deprecated, 'use snmpa:send_notification/5 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :send_notification, 6) do
    {:deprecated, 'use snmpa:send_notification/6 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :send_trap, 3) do
    {:deprecated, 'use snmpa:send_trap/3 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :send_trap, 4) do
    {:deprecated, 'use snmpa:send_trap/4 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :unload_mibs, 2) do
    {:deprecated, 'use snmpa:unload_mibs/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmp, :unregister_subagent, 2) do
    {:deprecated, 'use snmpa:unregister_subagent/2 instead.', 'OTP 24'}
  end

  def obsolete(:snmpa, :old_info_format, 1) do
    {:deprecated, 'use "new" format instead', 'OTP 24'}
  end

  def obsolete(:snmpm, :async_get, 3) do
    {:deprecated, 'use snmpm:async_get2/3 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get, 4) do
    {:deprecated, 'use snmpm:async_get2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get, 5) do
    {:deprecated, 'use snmpm:async_get2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get, 6) do
    {:deprecated, 'use snmpm:async_get2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_bulk, 5) do
    {:deprecated, 'use snmpm:async_get_bulk2/5 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_bulk, 6) do
    {:deprecated, 'use snmpm:async_get_bulk2/6 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_bulk, 7) do
    {:deprecated, 'use snmpm:async_get_bulk2/6 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_bulk, 8) do
    {:deprecated, 'use snmpm:async_get_bulk2/6 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_next, 3) do
    {:deprecated, 'use snmpm:async_get_next2/3 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_next, 4) do
    {:deprecated, 'use snmpm:async_get_next2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_next, 5) do
    {:deprecated, 'use snmpm:async_get_next2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_get_next, 6) do
    {:deprecated, 'use snmpm:async_get_next2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_set, 3) do
    {:deprecated, 'use snmpm:async_set2/3 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_set, 4) do
    {:deprecated, 'use snmpm:async_set2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_set, 5) do
    {:deprecated, 'use snmpm:async_set2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :async_set, 6) do
    {:deprecated, 'use snmpm:async_set2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get, 3) do
    {:deprecated, 'use snmpm:sync_get2/3 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get, 4) do
    {:deprecated, 'use snmpm:sync_get2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get, 5) do
    {:deprecated, 'use snmpm:sync_get2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get, 6) do
    {:deprecated, 'use snmpm:sync_get2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_bulk, 5) do
    {:deprecated, 'use snmpm:sync_get_bulk2/5 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_bulk, 6) do
    {:deprecated, 'use snmpm:sync_get_bulk2/6 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_bulk, 7) do
    {:deprecated, 'use snmpm:sync_get_bulk2/6 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_bulk, 8) do
    {:deprecated, 'use snmpm:sync_get_bulk2/6 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_next, 3) do
    {:deprecated, 'use snmpm:sync_get_next2/3 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_next, 4) do
    {:deprecated, 'use snmpm:sync_get_next2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_next, 5) do
    {:deprecated, 'use snmpm:sync_get_next2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_get_next, 6) do
    {:deprecated, 'use snmpm:sync_get_next2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_set, 3) do
    {:deprecated, 'use snmpm:sync_set2/3 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_set, 4) do
    {:deprecated, 'use snmpm:sync_set2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_set, 5) do
    {:deprecated, 'use snmpm:sync_set2/4 instead.', 'OTP 25'}
  end

  def obsolete(:snmpm, :sync_set, 6) do
    {:deprecated, 'use snmpm:sync_set2/4 instead.', 'OTP 25'}
  end

  def obsolete(:ssl, :cipher_suites, 0) do
    {:deprecated, 'use cipher_suites/2,3 instead', 'OTP 24'}
  end

  def obsolete(:ssl, :cipher_suites, 1) do
    {:deprecated, 'use cipher_suites/2,3 instead', 'OTP 24'}
  end

  def obsolete(:sys, :get_debug, 3) do
    {:deprecated,
     'incorrectly documented and only for internal use. Can often be replaced with sys:get_log/1'}
  end

  def obsolete(:wxCalendarCtrl, :enableYearChange, 1) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxCalendarCtrl, :enableYearChange, 2) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxClientDC, :new, 0) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxCursor, :new, 3) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxCursor, :new, 4) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxDC, :computeScaleAndOrigin, 1) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxGraphicsRenderer, :createLinearGradientBrush, 7) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxGraphicsRenderer, :createRadialGradientBrush, 8) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxGridCellEditor, :endEdit, 4) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxGridCellEditor, :paintBackground, 3) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxIdleEvent, :canSend, 1) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxMDIClientWindow, :new, 1) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxMDIClientWindow, :new, 2) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxPaintDC, :new, 0) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxPostScriptDC, :getResolution, 0) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxPostScriptDC, :setResolution, 1) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:wxWindowDC, :new, 0) do
    {:deprecated, 'not available in wxWidgets-2.9 and later'}
  end

  def obsolete(:core_lib, :get_anno, 1) do
    {:removed, 'use cerl:get_ann/1 instead'}
  end

  def obsolete(:core_lib, :is_literal, 1) do
    {:removed, 'use cerl:is_literal/1 instead'}
  end

  def obsolete(:core_lib, :is_literal_list, 1) do
    {:removed, 'use cerl:is_literal_list/1 instead'}
  end

  def obsolete(:core_lib, :literal_value, 1) do
    {:removed, 'use cerl:concrete/1 instead'}
  end

  def obsolete(:core_lib, :set_anno, 2) do
    {:removed, 'use cerl:set_ann/2 instead'}
  end

  def obsolete(:crypto, :aes_cbc_128_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :aes_cbc_128_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :aes_cbc_256_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :aes_cbc_256_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :aes_cbc_ivec, 2) do
    {:removed, 'use crypto:next_iv/2 instead'}
  end

  def obsolete(:crypto, :aes_cfb_128_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :aes_cfb_128_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :aes_ctr_decrypt, 3) do
    {:removed, 'use crypto:stream_decrypt/2 instead'}
  end

  def obsolete(:crypto, :aes_ctr_encrypt, 3) do
    {:removed, 'use crypto:stream_encrypt/2 instead'}
  end

  def obsolete(:crypto, :aes_ctr_stream_decrypt, 2) do
    {:removed, 'use crypto:stream_decrypt/2 instead'}
  end

  def obsolete(:crypto, :aes_ctr_stream_encrypt, 2) do
    {:removed, 'use crypto:stream_encrypt/2 instead'}
  end

  def obsolete(:crypto, :aes_ctr_stream_init, 2) do
    {:removed, 'use crypto:stream_init/3 instead'}
  end

  def obsolete(:crypto, :blowfish_cbc_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :blowfish_cbc_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :blowfish_cfb64_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :blowfish_cfb64_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :blowfish_ecb_decrypt, 2) do
    {:removed, 'use crypto:block_decrypt/3 instead'}
  end

  def obsolete(:crypto, :blowfish_ecb_encrypt, 2) do
    {:removed, 'use crypto:block_encrypt/3 instead'}
  end

  def obsolete(:crypto, :blowfish_ofb64_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :blowfish_ofb64_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :des3_cbc_decrypt, 5) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :des3_cbc_encrypt, 5) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :des3_cfb_decrypt, 5) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :des3_cfb_encrypt, 5) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :des3_ede3_cbc_decrypt, 5) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :des_cbc_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :des_cbc_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :des_cbc_ivec, 2) do
    {:removed, 'use crypto:next_iv/2 instead'}
  end

  def obsolete(:crypto, :des_cfb_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :des_cfb_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :des_cfb_ivec, 2) do
    {:removed, 'use crypto:next_iv/3 instead'}
  end

  def obsolete(:crypto, :des_ecb_decrypt, 2) do
    {:removed, 'use crypto:block_decrypt/3 instead'}
  end

  def obsolete(:crypto, :des_ecb_encrypt, 2) do
    {:removed, 'use crypto:block_encrypt/3 instead'}
  end

  def obsolete(:crypto, :des_ede3_cbc_encrypt, 5) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :dh_compute_key, 3) do
    {:removed, 'use crypto:compute_key/4 instead'}
  end

  def obsolete(:crypto, :dh_generate_key, 1) do
    {:removed, 'use crypto:generate_key/2 instead'}
  end

  def obsolete(:crypto, :dh_generate_key, 2) do
    {:removed, 'use crypto:generate_key/3 instead'}
  end

  def obsolete(:crypto, :erlint, 1) do
    {:removed, 'only needed by other removed functions'}
  end

  def obsolete(:crypto, :info, 0) do
    {:removed, 'use crypto:module_info/0 instead'}
  end

  def obsolete(:crypto, :md4, 1) do
    {:removed, 'use crypto:hash/2 instead'}
  end

  def obsolete(:crypto, :md4_final, 1) do
    {:removed, 'use crypto:hash_final/1 instead'}
  end

  def obsolete(:crypto, :md4_init, 0) do
    {:removed, 'use crypto:hash_init/1 instead'}
  end

  def obsolete(:crypto, :md4_update, 2) do
    {:removed, 'use crypto:hash_update/2 instead'}
  end

  def obsolete(:crypto, :md5, 1) do
    {:removed, 'use crypto:hash/2 instead'}
  end

  def obsolete(:crypto, :md5_final, 1) do
    {:removed, 'use crypto:hash_final/1 instead'}
  end

  def obsolete(:crypto, :md5_init, 0) do
    {:removed, 'use crypto:hash_init/1 instead'}
  end

  def obsolete(:crypto, :md5_mac, 2) do
    {:removed, 'use crypto:hmac/3 instead'}
  end

  def obsolete(:crypto, :md5_mac_96, 2) do
    {:removed, 'use crypto:hmac/4 instead'}
  end

  def obsolete(:crypto, :md5_update, 2) do
    {:removed, 'use crypto:hash_update/2 instead'}
  end

  def obsolete(:crypto, :mod_exp, 3) do
    {:removed, 'use crypto:mod_pow/3 instead'}
  end

  def obsolete(:crypto, :mpint, 1) do
    {:removed, 'only needed by other removed functions'}
  end

  def obsolete(:crypto, :rand_bytes, 1) do
    {:removed, 'use crypto:strong_rand_bytes/1 instead'}
  end

  def obsolete(:crypto, :rc2_40_cbc_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :rc2_40_cbc_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :rc2_cbc_decrypt, 3) do
    {:removed, 'use crypto:block_decrypt/4 instead'}
  end

  def obsolete(:crypto, :rc2_cbc_encrypt, 3) do
    {:removed, 'use crypto:block_encrypt/4 instead'}
  end

  def obsolete(:crypto, :rc4_encrypt, 2) do
    {:removed, 'use crypto:stream_encrypt/2 instead'}
  end

  def obsolete(:crypto, :rc4_encrypt_with_state, 2) do
    {:removed, 'use crypto:stream_encrypt/2 instead'}
  end

  def obsolete(:crypto, :rc4_set_key, 2) do
    {:removed, 'use crypto:stream_init/2 instead'}
  end

  def obsolete(:crypto, :sha, 1) do
    {:removed, 'use crypto:hash/2 instead'}
  end

  def obsolete(:crypto, :sha_final, 1) do
    {:removed, 'use crypto:hash_final/1 instead'}
  end

  def obsolete(:crypto, :sha_init, 0) do
    {:removed, 'use crypto:hash_init/1 instead'}
  end

  def obsolete(:crypto, :sha_mac, 2) do
    {:removed, 'use crypto:hmac/3 instead'}
  end

  def obsolete(:crypto, :sha_mac, 3) do
    {:removed, 'use crypto:hmac/4 instead'}
  end

  def obsolete(:crypto, :sha_mac_96, 2) do
    {:removed, 'use crypto:hmac/4 instead'}
  end

  def obsolete(:crypto, :sha_update, 2) do
    {:removed, 'use crypto:hash_update/2 instead'}
  end

  def obsolete(:crypto, :strong_rand_mpint, 3) do
    {:removed, 'only needed by other removed functions'}
  end

  def obsolete(:erl_lint, :modify_line, 2) do
    {:removed, 'use erl_parse:map_anno/2 instead'}
  end

  def obsolete(:erl_parse, :get_attribute, 2) do
    {:removed, 'erl_anno:{column,line,location,text}/1 instead'}
  end

  def obsolete(:erl_parse, :get_attributes, 1) do
    {:removed, 'erl_anno:{column,line,location,text}/1 instead'}
  end

  def obsolete(:erl_parse, :set_line, 2) do
    {:removed, 'use erl_anno:set_line/2'}
  end

  def obsolete(:erl_scan, :set_attribute, 3) do
    {:removed, 'use erl_anno:set_line/2 instead'}
  end

  def obsolete(:erlang, :get_stacktrace, 0) do
    {:removed, 'use the new try/catch syntax for retrieving the stack backtrace'}
  end

  def obsolete(:erlang, :hash, 2) do
    {:removed, 'use erlang:phash2/2 instead'}
  end

  def obsolete(:httpd_conf, :check_enum, 2) do
    {:removed, 'use lists:member/2 instead'}
  end

  def obsolete(:httpd_conf, :clean, 1) do
    {:removed, 'use sting:strip/1 instead or possibly the re module'}
  end

  def obsolete(:httpd_conf, :custom_clean, 3) do
    {:removed, 'use sting:strip/1 instead or possibly the re module'}
  end

  def obsolete(:httpd_conf, :is_directory, 1) do
    {:removed, 'use filelib:is_dir/1 instead'}
  end

  def obsolete(:httpd_conf, :is_file, 1) do
    {:removed, 'use filelib:is_file/1 instead'}
  end

  def obsolete(:httpd_conf, :make_integer, 1) do
    {:removed, 'use erlang:list_to_integer/1 instead'}
  end

  def obsolete(:rpc, :safe_multi_server_call, 2) do
    {:removed, 'use rpc:multi_server_call/2 instead'}
  end

  def obsolete(:rpc, :safe_multi_server_call, 3) do
    {:removed, 'use rpc:multi_server_call/3 instead'}
  end

  def obsolete(:ssl, :connection_info, 1) do
    {:removed, 'use ssl:connection_information/[1,2] instead'}
  end

  def obsolete(:ssl, :negotiated_next_protocol, 1) do
    {:removed, 'use ssl:negotiated_protocol/1 instead'}
  end

  def obsolete(:auth, :node_cookie, _) do
    {:deprecated, 'use erlang:set_cookie/2 and net_adm:ping/1 instead'}
  end

  def obsolete(:crypto, :next_iv, _) do
    {:deprecated, 'see the \'New and Old API\' chapter of the CRYPTO User\'s guide', 'OTP 24'}
  end

  def obsolete(:crypto, :stream_init, _) do
    {:deprecated,
     'use crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1 or crypto:crypto_one_time/4 instead',
     'OTP 24'}
  end

  def obsolete(:filename, :find_src, _) do
    {:deprecated, 'use filelib:find_source/1,3 instead', 'OTP 24'}
  end

  def obsolete(:ssl, :ssl_accept, _) do
    {:deprecated, 'use ssl_handshake/1,2,3 instead', 'OTP 24'}
  end

  def obsolete(:asn1ct, :decode, _) do
    {:removed, 'use Mod:decode/2 instead'}
  end

  def obsolete(:asn1ct, :encode, _) do
    {:removed, 'use Mod:encode/2 instead'}
  end

  def obsolete(:crypto, :dss_sign, _) do
    {:removed, 'use crypto:sign/4 instead'}
  end

  def obsolete(:crypto, :dss_verify, _) do
    {:removed, 'use crypto:verify/5 instead'}
  end

  def obsolete(:crypto, :rsa_sign, _) do
    {:removed, 'use crypto:sign/4 instead'}
  end

  def obsolete(:crypto, :rsa_verify, _) do
    {:removed, 'use crypto:verify/5 instead'}
  end

  def obsolete(:erl_scan, :attributes_info, _) do
    {:removed, 'use erl_anno:{column,line,location,text}/1 instead'}
  end

  def obsolete(:erl_scan, :token_info, _) do
    {:removed, 'use erl_scan:{category,column,line,location,symbol,text}/1 instead'}
  end

  def obsolete(:gen_fsm, _, _) do
    {:deprecated, 'use the \'gen_statem\' module instead'}
  end

  def obsolete(:pg2, _, _) do
    {:deprecated, 'use \'pg\' instead', 'OTP 24'}
  end

  def obsolete(:random, _, _) do
    {:deprecated, 'use the \'rand\' module instead'}
  end

  def obsolete(:os_mon_mib, _, _) do
    {:removed, 'this module was removed in OTP 22.0'}
  end

  def obsolete(_, _, _) do
    :no
  end

  def obsolete_type(:crypto, :retired_cbc_cipher_aliases, 0) do
    {:deprecated, 'Use aes_*_cbc or des_ede3_cbc'}
  end

  def obsolete_type(:crypto, :retired_cfb_cipher_aliases, 0) do
    {:deprecated, 'Use aes_*_cfb8, aes_*_cfb128 or des_ede3_cfb'}
  end

  def obsolete_type(:crypto, :retired_ctr_cipher_aliases, 0) do
    {:deprecated, 'Use aes_*_ctr'}
  end

  def obsolete_type(:crypto, :retired_ecb_cipher_aliases, 0) do
    {:deprecated, 'Use aes_*_ecb'}
  end

  def obsolete_type(:erl_scan, :column, 0) do
    {:removed, 'use erl_anno:column() instead'}
  end

  def obsolete_type(:erl_scan, :line, 0) do
    {:removed, 'use erl_anno:line() instead'}
  end

  def obsolete_type(:erl_scan, :location, 0) do
    {:removed, 'use erl_anno:location() instead'}
  end

  def obsolete_type(_, _, _) do
    :no
  end
end
