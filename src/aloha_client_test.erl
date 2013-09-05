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

-module(aloha_client_test).
-export([start/0]).

start() ->
    Pid = proc_lib:spawn(fun main/0),
    lager:info("~s spawned ~p", [?MODULE, Pid]).

main() ->
    HwAddr = <<16#0003478ca1b3:48>>,  % taken from my unused machine
    IPAddr = <<127,0,0,1>>,
    IPv6Addr = <<1:128>>,
    {ok, Nic} = aloha_nic_loopback:create(aloha_demo, HwAddr),
    ok = gen_server:call(Nic, {setopts, [{ip, IPAddr}, {ipv6, IPv6Addr}]}),
    {ok, Opts} = gen_server:call(Nic, getopts),
    Addr = proplists:get_value(addr, Opts),
    Mtu = proplists:get_value(mtu, Opts),
    Ip = proplists:get_value(ip, Opts),
    Backend = proplists:get_value(backend, Opts),
    {ok, Sock} = aloha_tcp:connect(aloha_demo, <<127,0,0,1>>, 9999, Addr,
                                   Backend, [{ip, Ip}, {mtu, Mtu}]),
    ok = aloha_socket:setopts(Sock, [{active, true}]),
    ok = aloha_socket:send(Sock, <<"hello!">>),
    loop(Sock).

loop(Sock) ->
    receive
        {tcp, Sock, Data} ->
            lager:info("test client recv ~p", [Data]),
            loop(Sock);
        {tcp_closed, Sock} ->
            lager:info("test client got tcp_closed", [])
    end.
