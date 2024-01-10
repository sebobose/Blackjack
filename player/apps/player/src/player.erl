-module(player).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-export([poruka/1, message_dealer/1, generate_card/0, calculate_sum/1]).

-define(DEALER, 'dealer@localhost').

calculate_sum(Cards) ->
    calculate_sum(Cards, 0, 0).

calculate_sum([], Sum, Aces) when Aces > 0, Sum + 10 + Aces =< 21 ->
    % If there are Aces, adjust the sum to get the best poss
    {Sum + 10 + Aces, soft};
calculate_sum([], Sum, Aces) ->
    {Sum + Aces, hard};
calculate_sum([Card|Rest], Sum, Aces) when Card =:= 1 ->
    % Handle Ace (value 1)
    calculate_sum(Rest, Sum, Aces + 1);
calculate_sum([Card|Rest], Sum, Aces) when Card >= 2, Card =< 10 ->
    % Handle cards 2 through 10
    calculate_sum(Rest, Sum + Card, Aces);
calculate_sum([Card|Rest], Sum, Aces) when Card > 10 ->
    % Handle face cards (Jack, Queen, King)
    calculate_sum(Rest, Sum + 10, Aces).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_server:cast(player, start),
    {ok, []}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, _) ->
    timer:sleep(1000),
    io:format("~nDobrodosli u erlang-blackjack, upisite svoje ime: ~n"),
    Name = io:get_line(""),
    FormattedName = string:trim(Name),
    io:format("~nUpisite svoj balans: ~n"),
    MoneyStr = io:get_line(""),
    {ok, [Money], _} = io_lib:fread("~d", MoneyStr),
    io:format("~nUpisite svoj ulog: ~n"),
    StakeStr = io:get_line(""),
    {ok, [Stake], _} = io_lib:fread("~d", StakeStr),
    net_kernel:start([list_to_atom(FormattedName), shortnames]),
    io:format("Dobrodosli ~p!~nBalans: ~p~nUlof: ~p~n", [FormattedName, Money, Stake]),
    net_adm:ping(?DEALER),
    UpdatedState = {[], Stake, Money},
    {noreply, UpdatedState};

handle_cast(hit, State) when element(2,State) =< 0 ->
    io:format("Morate staviti ulog.~n"),
    {noreply, State};

handle_cast(hit, State) when element(2,State) > element(3,State) ->
    io:format("Nedovoljno novaca na računu, smanjite ulog.~n"),
    io:format("Balans: ~p~n", [element(3,State)]),
    {noreply, State};


handle_cast(hit, State) ->
    Card = generate_card(),
    UpdatedState = {[Card | element(1,State)], element(2,State), element(3,State)},
    io:format("Dobili ste kartu: ~p~n", [Card]),
    io:format("Trenutne karte: ~p~n", [element(1,UpdatedState)]),
    io:format("Suma: ~p~n", [calculate_sum(element(1,UpdatedState))]),
    case {element(2, calculate_sum(element(1,UpdatedState))), element(1, calculate_sum(element(1,UpdatedState))) >= 21} of
        {hard, true} ->
            gen_server:cast(?MODULE, stand),
            {noreply, UpdatedState};
        _ ->
            {noreply, UpdatedState}
    end;

handle_cast(stand, State) when element(1,State) == [] ->
    io:format("Ne možete ostati ako nemate karata.~n"),
    {noreply, State};

handle_cast(stand, State) ->
    io:format("Ostajete s kartama: ~p~n", [element(1,State)]),
    io:format("Suma: ~p~n", [element(1,calculate_sum(element(1,State)))]),
    io:format("Balans: ~p~n", [element(3, State) - element(2, State)]),
    % Here you can implement further logic, such as sending the player's hand to the dealer.
    {noreply, {[], element(2, State), element(3, State) - element(2, State)}};


handle_cast(Msg, State) ->
    io:format("player cast: ~p~n", [Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("player info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

poruka(Msg) ->
    gen_server:cast(player, Msg),
    string:concat("Primjer poruka: ", Msg).

message_dealer(Msg) ->
    rpc:call(?DEALER, dealer, poruka, [Msg]).

% koristi se za testiranje.
generate_card() ->
    Numbers = lists:flatten(lists:duplicate(4, lists:seq(1, 9) ++ [10, 10, 10, 10])),
    N = rand:uniform(length(Numbers)),
    lists:nth(N, Numbers).
