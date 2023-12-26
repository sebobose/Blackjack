
-module(player).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-export([primjer/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_server:cast(player, start),
    {ok, []}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, State) ->
    timer:sleep(1000),
    io:format("~nDobrodosli u erlang-blackjack, upisite svoje ime: ~n"),
    Name = io:get_line(""),
    FormattedName = string:trim(Name),
    net_kernel:start([list_to_atom(FormattedName), shortnames]),
    io:format("Dobrodosli ~p!~n", [FormattedName]),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("dealer cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("dealer info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% poziva se u shellu sa dealer:primjer(Msg).
primjer(Msg) ->
    io:format("Primjer poruka: ~p", [Msg]).