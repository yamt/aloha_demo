% Copyright (c)2013 YAMAMOTO Takashi,
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

-module(aloha_datapath).
-export([handle_conn/1, packet_out/3]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

-define(OFP_VERSION, 4).

-record(datapath, {sock, parser, id, q = []}).

msg(Body, Xid) -> #ofp_message{version=?OFP_VERSION, xid=Xid, body=Body}.

handle_conn(Sock) ->
    Dp = #datapath{sock=Sock},
    ofp_setup(Dp),
    {ok, Parser} = ofp_parser:new(?OFP_VERSION),
    loop(Dp#datapath{parser=Parser}).

ofp_setup(#datapath{sock=Sock} = Dp) ->
    send_msg(Dp, #ofp_hello{}),
    send_msg(Dp, #ofp_features_request{}),
    send_msg(Dp, #ofp_set_config{miss_send_len=no_buffer}),
    send_msg(Dp, #ofp_flow_mod{table_id=0, command=add,
        instructions=[
            #ofp_instruction_apply_actions{
                actions=[
                    #ofp_action_output{port=controller,max_len=no_buffer}
                ]
            }
        ]}),
    send_msg(Dp, #ofp_desc_request{}),
    send_msg(Dp, #ofp_port_desc_request{}),
    send_msg(Dp, #ofp_barrier_request{}),
    % XXX
    aloha_socket:setopts(Sock, [{nodelay, true}]).

loop(#datapath{sock=Sock, parser=Parser} = Dp) ->
    aloha_socket:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            {ok, NewParser, Msgs} = ofp_parser:parse(Parser, Data),
            lists:foreach(fun(M) -> self() ! {msg, Sock, M} end, Msgs),
            loop(Dp#datapath{parser=NewParser});
        {tcp_closed, Sock} ->
            lager:info("OF ~p closed", [Sock]),
            done(Dp);
        {msg, Sock, #ofp_message{xid=Xid, body=Body}=Msg} ->
            lager:debug("OF msg received ~p~n", [aloha_utils:pr(Msg, ?MODULE)]),
            Dp2 = handle_msg(Body, Xid, Dp),
            loop(Dp2);
        {packet_out, Port, BinPacket} ->
            send_packet_out(BinPacket, Port, Dp),
            loop(Dp);
        M ->
            lager:info("unknown msg ~p~n", [M]),
            done(Dp)
    end.

done(Dp) ->
    % exit
    case Dp#datapath.id of
        undefined -> ok;
        Id -> ets:delete(aloha_datapath, Id)
    end.

handle_msg(#ofp_hello{} = Msg, _Xid, Dp) ->
    lager:info("hello received ~p", [Msg]),
    Dp;
handle_msg(#ofp_features_reply{datapath_mac = Mac, datapath_id = Id}, _Xid,
           #datapath{id = undefined, q = Q} = Dp) ->
    DpId = {Id, Mac},
    lists:foreach(fun(Msg) ->
        lager:info("reply msg ~p", [Msg]),
        self() ! Msg
    end, Q),
    lager:info("register datapath process ~p for ~w", [self(), DpId]),
    true = ets:insert_new(aloha_datapath, {DpId, self()}),
    Dp#datapath{id = DpId, q = []};
handle_msg(Msg, Xid, #datapath{id = undefined, q = Q} = Dp) ->
    lager:info("hold msg ~p", [Msg]),
    Dp#datapath{q = Q ++ [#ofp_message{xid = Xid, body = Msg}]};
handle_msg(#ofp_echo_request{data=Data}, Xid, Dp) ->
    send_msg(Dp, #ofp_echo_reply{data=Data}, Xid),
    Dp;
handle_msg(#ofp_packet_in{match=Match, data=Data}, _Xid, Dp) ->
    <<IntInPort:32>> = match_field(Match, in_port),
    InPort = decode_enum(port_no, IntInPort),
    packet_in(Data, InPort, Dp),
    Dp;
handle_msg(Msg, _Xid, Dp) ->
    lager:info("unhandled OF msg ~p", [aloha_utils:pr(Msg, ?MODULE)]),
    Dp.

match_field(Match, Field) ->
    case lists:keyfind(Field, #ofp_field.name, Match#ofp_match.fields) of
        false ->
            false;
        #ofp_field{class=openflow_basic, has_mask=true, value=Value,
          mask=Mask} ->
            {Value, Mask};
        #ofp_field{class=openflow_basic, has_mask=false, value=Value} ->
            Value
    end.

send_msg(Dp, Body) ->
    send_msg(Dp, Body, get_xid()).

send_msg(Dp, Body, Xid) ->
    Msg = msg(Body, Xid),
    lager:debug("OF msg sent ~w~n", [aloha_utils:pr(Msg, ?MODULE)]),
    {ok, BinMsg} = of_protocol:encode(Msg),
    case aloha_socket:send(Dp#datapath.sock, BinMsg) of
        ok -> ok;
        {error, Reason} -> lager:info("send error ~p", [Reason])
    end.

decode_enum(Enum, Value) ->
    EnumMod = list_to_atom("ofp_v" ++ integer_to_list(?OFP_VERSION) ++ "_enum"),
    ofp_utils:get_enum_name(EnumMod, Enum, Value).

packet_in(Packet, InPort, #datapath{id = DpId}) ->
    lager:debug("packet_in ~w ~w ~w~n", [DpId, InPort, Packet]),
    NicPid = find_or_create_nic(DpId, InPort),
    gen_server:cast(NicPid, {packet, Packet}).

packet_out(BinPacket, Port, DpId) ->
    lager:debug("packet_out ~p ~w ~w~n", [DpId, Port, BinPacket]),
    case catch ets:lookup_element(aloha_datapath, DpId, 2) of
        Pid when is_pid(Pid) -> Pid ! {packet_out, Port, BinPacket};
        _ -> lager:info("no datapath process for ~w", [DpId])
    end.

send_packet_out(BinPacket, Port, Dp) ->
    send_msg(Dp, #ofp_packet_out{actions = [#ofp_action_output{port = Port}],
                                 in_port = controller, data = BinPacket}).

find_or_create_nic(DpId, InPort) ->
    Key = {DpId, InPort},
    case catch ets:lookup_element(aloha_nic, Key, 2) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            create_nic(DpId, InPort),
            find_or_create_nic(DpId, InPort)
    end.

create_nic(DpId, InPort) ->
    Key = {DpId, InPort},
    HwAddr = <<16#0003478ca1b3:48>>,  % taken from my unused machine
    IPAddr = <<192,0,2,1>>,
    IPv6Addr = <<16#2001:16, 16#db8:16, 0:(16*5), 1:16>>,  % RFC 3849
    {ok, Pid} = gen_server:start(aloha_nic,
                                 [{key, Key},
                                  {addr, HwAddr},
                                  {ip_addr, IPAddr},
                                  {ipv6_addr, IPv6Addr},
                                  % use a smaller mtu as we do
                                  % ether over OF over TCP/IP over ether.
                                  % just a guess.
                                  {mtu, 1300},
                                  {backend, {?MODULE, packet_out,
                                             [InPort, DpId]}}], []),
    lager:info("nic ~p created", [Pid]).

get_xid() ->
    % XXX
    Xid = case get(xid) of
        undefined -> 1;
        N -> N
    end,
    put(xid, Xid + 1),
    Xid.

