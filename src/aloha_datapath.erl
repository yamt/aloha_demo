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

-record(datapath, {sock, parser}).

msg(Body, Xid) -> #ofp_message{version=?OFP_VERSION, xid=Xid, body=Body}.

handle_conn(Sock) ->
    Dp = #datapath{sock=Sock},
    ofp_setup(Dp),
    {ok, Parser} = ofp_parser:new(?OFP_VERSION),
    loop(Dp#datapath{parser=Parser}).

ofp_setup(#datapath{sock=Sock} = Dp) ->
    send_msg(Dp, #ofp_hello{}),
    send_msg(Dp, #ofp_set_config{miss_send_len=no_buffer}),
    send_msg(Dp, #ofp_flow_mod{table_id=0, command=add,
        instructions=[
            #ofp_instruction_apply_actions{
                actions=[
                    #ofp_action_output{port=controller,max_len=no_buffer}
                ]
            }
        ]}),
    send_msg(Dp, #ofp_barrier_request{}),
    % XXX
    aloha_socket:setopts(Sock, [{nodelay, true}]).

loop(#datapath{sock=Sock, parser=Parser} = Dp) ->
    aloha_socket:setopts(Sock, [{active, once}]),
    lager:debug("loop ~p~n", [Parser]),
    receive
        {tcp, Sock, Data} ->
            lager:debug("tcp receive ~w~n", [Data]),
            {ok, NewParser, Msgs} = ofp_parser:parse(Parser, Data),
            lists:foreach(fun(M) -> self() ! {msg, Sock, M} end, Msgs),
            loop(Dp#datapath{parser=NewParser});
        {tcp_closed, Sock} ->
            lager:info("OF ~p closed", [Sock]);
        {msg, Sock, #ofp_message{xid=Xid, body=Body}=Msg} ->
            lager:debug("msg received ~p~n", [Msg]),
            handle_msg(Dp, Body, Xid),
            loop(Dp);
        M ->
            lager:info("unknown ~p~n", [M])
    end.

handle_msg(Dp, #ofp_echo_request{data=Data}, Xid) ->
    send_msg(Dp, #ofp_echo_reply{data=Data}, Xid);
handle_msg(Dp, #ofp_packet_in{match=Match, data=Data}, _Xid) ->
    <<IntInPort:32>> = match_field(Match, in_port),
    InPort = decode_enum(port_no, IntInPort),
    packet_in(Dp, InPort, Data);
handle_msg(_Dp, _M, _Xid) ->
    ok.

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
    lager:debug("msg sent ~w~n", [Msg]),
    {ok, BinMsg} = of_protocol:encode(Msg),
    case aloha_socket:send(Dp#datapath.sock, BinMsg) of
        ok -> ok;
        {error, Reason} -> lager:info("send error ~p", [Reason])
    end.

decode_enum(Enum, Value) ->
    EnumMod = list_to_atom("ofp_v" ++ integer_to_list(?OFP_VERSION)++ "_enum"),
    ofp_utils:get_enum_name(EnumMod, Enum, Value).

packet_in(Dp, InPort, Packet) ->
    lager:info("packet_in ~w ~w~n", [InPort, Packet]),
    NicPid = get_nic(Dp, InPort),
    gen_server:cast(NicPid, {set_backend, {Dp, InPort}}),  % XXX
    gen_server:cast(NicPid, {packet, Packet}).

packet_out(Dp, Port, BinPacket) ->
    lager:info("packet_out ~w ~w~n", [Port, BinPacket]),
    send_msg(Dp, #ofp_packet_out{actions=[#ofp_action_output{port=Port}],
                                 in_port=controller, data=BinPacket}).

get_nic(_Dp, InPort) ->
    % XXX
    ets:lookup_element(nic_table, InPort, 2).

get_xid() ->
    % XXX
    Xid = case get(xid) of
        undefined -> 1;
        N -> N
    end,
    put(xid, Xid + 1),
    Xid.

