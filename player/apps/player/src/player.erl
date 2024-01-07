
-module(player).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-export([poruka/1,message_dealer/1]).
-define(DEALER, 'dealer@localhost').

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
    net_adm:ping(dealer@localhost),
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

    %primjer/1: Ova funkcija prima jedan argument Msg, za koji se očekuje da je poruka koju želite poslati player gen_serveru. Koristi gen_server:cast/2 za asinkrono slanje poruke player gen_serveru.
poruka(Msg) ->
    gen_server:cast(player, Msg),
    string:concat("Primjer poruka: ", Msg).

%message_dealer/1: Ova funkcija prima jedan argument Msg, za koji se očekuje da je poruka koju želite poslati dealer gen_serveru. Koristi rpc:call/4 za sinkrono slanje poruke dealer gen_serveru na čvoru specificiranom s ?DEALER makro. Poruka se šalje pozivanjem funkcije primjer/1 na dealer gen_serveru s Msg kao argumentom. Povratna vrijednost rpc:call/4 je povratna vrijednost funkcije primjer/1 na dealer gen_serveru, što je string
message_dealer(Msg)->
    rpc:call(?DEALER, dealer, poruka, [Msg]).


