
-module(dealer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-export([poruka/1,message_all/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_server:cast(dealer, start),
    {ok, []}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, State) ->
    timer:sleep(1000),
    io:format("~nDobrodosli u erlang-blackjack, vi dijelite karte! ~n"),
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

% poziva se u dealer shellu sa dealer:primjer(Msg).
poruka(Msg) ->
    % ova cast funkcija asinkrono poziva funkciju na 26. liniji
    gen_server:cast(dealer, Msg),
    string:concat("Primjer poruka: ", Msg).

%Funkcija message_all/1 koristi se za slanje poruke svim igračima (player nodes) u sustavu. Evo kako to funkcionira: Prima jedan argument Msg, koji je poruka koju želite poslati svim igračima. Koristi rpc:multicall/4 za istodobno slanje poruke svim vidljivim čvorovima (nodes). nodes() je funkcija koja vraća listu svih vidljivih čvorova. player je modul na kojem želite pozvati funkciju. primjer je funkcija koju želite pozvati na svakom čvoru. [Msg] je lista argumenata koju želite proslijediti funkciji primjer/1. Dakle, message_all(Msg) će pozvati player:primjer(Msg) na svakom vidljivom čvoru u sustavu.
message_all(Msg)->
    rpc:multicall(nodes(), player,poruka,[Msg]).
