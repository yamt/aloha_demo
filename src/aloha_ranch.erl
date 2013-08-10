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

-module(aloha_ranch).
-behaviour(ranch_transport).

-export([name/0, messages/0]).
-export([listen/1, accept/2]).
-export([controlling_process/2, setopts/2, send/2, recv/3, close/1,
         peername/1, sockname/1]).
-export([sendfile/2, connect/3]).

name() -> tcp.
messages() -> {tcp, tcp_closed}.

listen(Opts) ->
    lager:debug("aloha_ranch listen ~p", [Opts]),
    Port = proplists:get_value(port, Opts),
    aloha_socket:listen(Port, [{active, false}, {packet, raw},
                               {reuseaddr, true}, {nodelay, true}|Opts]).

accept(Sock, _Timeout) ->
    lager:debug("aloha_ranch accept ~p ~p", [Sock, _Timeout]),
    aloha_socket:accept(Sock).  % XXX timeout

connect(_Host, _Port, _Opts) ->
    {error, unimplemented}.

recv(Sock, Len, Timeout) ->
    lager:debug("~p aloha_ranch recv ~p ~p ~p", [self(), Sock, Len, Timeout]),
    aloha_socket:recv(Sock, Len, Timeout).

send(Sock, Data) ->
    aloha_socket:send(Sock, Data).

sendfile(_Sock, _Filename) ->
    {error, unimplemented}.

setopts(Sock, Opts) ->
    lager:debug("aloha_ranch setopts ~p ~p", [Sock, Opts]),
    aloha_socket:setopts(Sock, Opts).

controlling_process(Sock, Pid) ->
    aloha_socket:controlling_process(Sock, Pid).

peername(Sock) ->
    aloha_socket:peername(Sock).

sockname(Sock) ->
    aloha_socket:sockname(Sock).

close(Sock) ->
    aloha_socket:close(Sock).
