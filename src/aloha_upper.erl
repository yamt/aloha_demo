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

% sample app logic

-module(aloha_upper).
-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([acceptor/2]).

-behaviour(gen_server).

-record(state, {sock}).

acceptor(LSock, Opts) ->
    {ok, Sock} = aloha_socket:accept(LSock),
    {ok, Pid} = gen_server:start(?MODULE, [{sock, Sock}|Opts], []),
    ok = aloha_socket:controlling_process(Sock, Pid),
    acceptor(LSock, Opts).

start(Opts) ->
    Mod = proplists:get_value(mod, Opts, aloha_socket),
    {ok, LSock} = Mod:listen(9999, [binary, {packet, raw}, {reuseaddr, true},
                                    {nodelay, true}, {active, false}]),
    proc_lib:spawn(?MODULE, acceptor, [LSock, Opts]).

init(Opts) ->
    Sock = proplists:get_value(sock, Opts),
    lager:info("child for ~p", [Sock]),
    ok = aloha_socket:send(Sock, <<"ALOHA!", 16#a>>),
    {ok, PeerName} = aloha_socket:peername(Sock),
    {ok, SockName} = aloha_socket:sockname(Sock),
    lager:info("peer ~p sock ~p", [PeerName, SockName]),
    ok = aloha_socket:send(Sock, io_lib:format("HELLO ~p~n", [PeerName])),
    ok = aloha_socket:send(Sock, io_lib:format("THIS IS ~p~n", [SockName])),
    ok = aloha_socket:setopts(Sock, [{active, once}]),
    %spawn(fun() -> loop(Sock) end),
    State = #state{sock = Sock},
    {ok, State}.

loop(Sock) ->
    case aloha_socket:recv(Sock, 0, 1000) of
        {ok, Data} -> upper(Sock, Data), loop(Sock);
        {error, timeout} -> lager:info("recv ~p timeout", [Sock]), loop(Sock);
        Error -> lager:info("recv ~p error ~p", [Sock, Error])
    end.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, #state{sock = Sock} = State) ->
    lager:info("handle_info: RECV len ~p", [byte_size(Data)]),
    upper(Sock, Data),
    ok = aloha_socket:setopts(Sock, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Sock}, #state{sock = Sock} = State) ->
    lager:info("handle_info: CLOSED ~w", [Sock]),
    ok = aloha_socket:close(Sock),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:info("handle_info: unknown ~w", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

upper(Sock, Data) ->
    Data2 = list_to_binary(string:to_upper(binary_to_list(Data))),
    ok = aloha_socket:send(Sock, Data2).
