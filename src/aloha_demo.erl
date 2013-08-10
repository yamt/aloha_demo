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

-module(aloha_demo).
-export([start/0]).

start() ->
    lager:start(),
    lager:info("start~n", []),

    % configuration
    ets:new(nic_table, [set, named_table]),  % XXX should not be here
    HwAddr = <<16#0003478ca1b3:48>>,  % taken from my unused machine
    IPAddr = <<192,0,2,1>>,
    {ok, Pid} = gen_server:start(aloha_nic, [{addr, HwAddr}], []),
    gen_server:cast(Pid, {set_prop, {ip_addr, IPAddr}}),
    register_nic(local, Pid),

    aloha_tcp:start(),
    aloha_upper:start([]),
%    aloha_upper:start([{mod, gen_tcp}]),

%    application:start(sasl),
    application:start(ranch),
    application:start(crypto),
    application:start(cowboy),
%    aloha_httpd:start(aloha_httpd, ranch_tcp, 8080),
    aloha_httpd:start(aloha_httpd_aloha, aloha_ranch, 8080),

    aloha_ofc:start().

register_nic(InPort, Pid) ->
    % XXX
    ets:insert(nic_table, {InPort, Pid}).
