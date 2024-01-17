-module(player).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-export([poruka/1, message_dealer/1, generate_card/0, calculate_sum/1, send_dealers_count/1]).

-define(DEALER, 'dealer@localhost').

% hand --> karte koje sam povukao
% stake --> ulog
% money --> balans
% error --> error poruka
-record(state, {hand = [], stake = 0, money = 0, error = "none", dealer=0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_server:cast(player, start),
    {ok, []}.

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

% -----------------------------------------------------------------------------------------
% ------------------------------------ CARD OPERATIONS ------------------------------------
% -----------------------------------------------------------------------------------------

calculate_sum(Cards) ->
    calculate_sum(Cards, 0, 0).

calculate_sum([], Sum, Aces) when Aces > 0, Sum + 10 + Aces =< 21 ->
    {Sum + 10 + Aces, soft};
calculate_sum([], Sum, Aces) ->
    {Sum + Aces, hard};
calculate_sum([Card | Rest], Sum, Aces) when Card =:= 1 ->
    calculate_sum(Rest, Sum, Aces + 1);
calculate_sum([Card | Rest], Sum, Aces) when Card >= 2, Card =< 10 ->
    calculate_sum(Rest, Sum + Card, Aces);
calculate_sum([Card | Rest], Sum, Aces) when Card > 10 ->
    calculate_sum(Rest, Sum + 10, Aces).

% -----------------------------------------------------------------------------------------
% ----------------------------------- LOOP FOR INPUT --------------------------------------
% -----------------------------------------------------------------------------------------

% Helper function for the while loop
loop_while_quit(State) ->
    case io:get_line("INPUT:   ") of
        "quit\n" ->
            {noreply, State};
        Input ->
            case State#state.error of
                "wait" ->
                    io:format("Wait for dealers card"),
                    loop_while_quit(State);
                _ ->
                    NewState = element(2, handle_user_input(Input, State)),
                    case NewState#state.error of
                        "bust" ->
                            io:format("BUST!~n"),
                            handle_user_input("stand\n", NewState),
                            loop_while_quit(NewState#state{
                                hand = [],
                                money = NewState#state.money - NewState#state.stake,
                                error = "wait"
                            });
                        "none" ->
                            loop_while_quit(NewState);
                        "stake" ->
                            io:format("Morate staviti ulog.~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "money" ->
                            io:format("Nedovoljno novaca na računu(~p), smanjite ulog(~p).~n", [
                                NewState#state.money, NewState#state.stake
                            ]),
                            loop_while_quit(NewState#state{error = "none"});
                        "card" ->
                            io:format("Ne možete ostati ako nemate karata.~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "starterror" ->
                            io:format("Igra je već započeta.~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "hiterror" ->
                            io:format("Morate započeti igru.~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        _ ->
                            io:format("Error: ~p~n", [NewState#state.error]),
                            loop_while_quit(NewState#state{error = "none"})
                    end
            end
    end.

% Handle user input (hit, stand, or other actions)
handle_user_input("hit\n", State) ->
    handle_cast(hit, State);
handle_user_input("stand\n", State) ->
    handle_cast(stand, State);
handle_user_input("start\n", State) ->
    handle_cast(startgame, State);
handle_user_input("stake\n", State) ->
    io:format("Promijenite svoj ulog: "),
    StakeStr = io:get_line(""),
    {ok, [Stake], _} = io_lib:fread("~d", StakeStr),
    {noreply, State#state{stake = Stake}};
handle_user_input("help\n", State) ->
    io:format(
        "   HELP:~n\n"
        "   --------------------------------\n"
        "   start --- početak igre~n\n"
        "   hit   --- vuci kartu~n\n"
        "   stand --- dosta~n\n"
        "   stake --- promjena uloga~n\n"
        "   quit  --- izlaz~n\n"
    ),
    {noreply, State};
handle_user_input(_, State) ->
    io:format("Invalid input.~n"),
    {noreply, State}.

% -----------------------------------------------------------------------------------------
% ------------------------------------- CALL HANDLERS -------------------------------------
% -----------------------------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% -----------------------------------------------------------------------------------------
% ------------------------------------- CAST HANDLERS -------------------------------------
% -----------------------------------------------------------------------------------------

handle_cast(start, _) ->
    timer:sleep(1000),
    io:format("~nDobrodosli u erlang-blackjack, upisite svoje ime: ~n"),
    Name = io:get_line(""),
    FormattedName = string:trim(Name),
    io:format("Upisite svoj balans: "),
    MoneyStr = io:get_line(""),
    {ok, [Money], _} = io_lib:fread("~d", MoneyStr),
    io:format("Upisite svoj ulog: "),
    StakeStr = io:get_line(""),
    {ok, [Stake], _} = io_lib:fread("~d", StakeStr),
    net_kernel:start([list_to_atom(FormattedName), shortnames]),
    io:format("Dobrodosli ~p!~nBalans: ~p~nUlog: ~p~n", [FormattedName, Money, Stake]),
    io:format("Za pomoc u igri napisite help!~n~n"),
    net_adm:ping(?DEALER),
    UpdatedState = #state{hand = [], stake = Stake, money = Money, error = "none", dealer=0},
    loop_while_quit(UpdatedState),
    {noreply, UpdatedState};
handle_cast(startgame, State) when State#state.stake =< 0 ->
    {noreply, State#state{error = "stake"}};
handle_cast(startgame, State) when State#state.stake > State#state.money ->
    {noreply, State#state{error = "money"}};
handle_cast(startgame, State) when length(State#state.hand) > 0 ->
    {noreply, State#state{error = "starterror"}};
handle_cast(startgame, State) ->
    Resp = rpc:call(?DEALER, dealer, ready, []),
    Hand = [element(1, Resp), element(2, Resp)],
    DealersCard = element(3, Resp),
    io:format("Dobili ste karte: ~p~n", [Hand]),
    io:format("Suma: ~p~n", [calculate_sum(Hand)]),
    io:format("Dealerova karta: ~p~n", [DealersCard]),
    {noreply, State#state{hand = Hand, dealer = DealersCard}};
handle_cast(hit, State) when length(State#state.hand) == 0 ->
    UpdatedState = State#state{error = "hiterror"},
    {noreply, UpdatedState};
handle_cast(hit, State) ->
    Card = rpc:call(?DEALER, dealer, hit, []),
    UpdatedState = State#state{hand = [Card | State#state.hand]},
    io:format("Dobili ste kartu: ~p~n", [Card]),
    io:format("Trenutne karte: ~p~n", [UpdatedState#state.hand]),
    io:format("Suma: ~p~n", [calculate_sum(UpdatedState#state.hand)]),
    case
        {
            element(2, calculate_sum(UpdatedState#state.hand)),
            element(1, calculate_sum(UpdatedState#state.hand)) > 21
        }
    of
        {hard, true} ->
            {noreply, State#state{hand = [Card | State#state.hand], error = "bust"}};
        _ ->
            {noreply, UpdatedState}
    end;
handle_cast(stand, State) when State#state.hand == [] ->
    UpdatedState = State#state{error = "card"},
    {noreply, UpdatedState};
handle_cast(stand, State) ->
    Sum = element(1, calculate_sum(State#state.hand)),
    io:format("Ostajete s kartama: ~p~n", [State#state.hand]),
    io:format("Suma: ~p~n", [Sum]),
    UpdatedState=State#state{error = "wait"},
    if
        Sum == 21, length(State#state.hand)==2  ->
            io:format("blackjack"),
            UpdatedState=UpdatedState#state{hand=[100]}
    end,
    rpc:call(?DEALER, dealer, stand, []),
    io:format("Cekamo dealerove karte..."),
    {noreply, UpdatedState};
handle_cast(Msg, State) ->
    io:format("Dealer got count: ~p~n", [Msg]),
    Count=element(1,calculate_sum(State#state.hand)),
    UpdatedState=State#state{hand=[], error="none"},
    if
        Count==100 ->
            io:format("BLACKJACK!!!.\n"),
            UpdatedState=UpdatedState#state{money=State#state.money+State#state.stake*1.5};
        Count > 21 ->
            io:format("Bust.\n"),
            UpdatedState=UpdatedState#state{money=State#state.money-State#state.stake};
        Msg == 'blackjack' ->
            io:format("Dealer won.\n"),
            UpdatedState=UpdatedState#state{money=State#state.money-State#state.stake};
        Msg == 'bust' ->
            io:format("YOU WON!\n"),
            UpdatedState=UpdatedState#state{money=State#state.money+State#state.stake};
        Count > Msg ->
            io:format("Dealer won.\n"),
            UpdatedState=UpdatedState#state{money=State#state.money-State#state.stake};
        Count < Msg ->
            io:format("YOU WON!\n"),
            UpdatedState=UpdatedState#state{money=State#state.money+State#state.stake};
        true ->
            io:format("Push.\n")
    end,
    {noreply, UpdatedState}.

% -----------------------------------------------------------------------------------------
% ------------------------------- FUNKCIJE KOJE ZOVE PLAYER -------------------------------
% -----------------------------------------------------------------------------------------

% koristi se za testiranje.
generate_card() ->
    Numbers = lists:flatten(lists:duplicate(4, lists:seq(1, 13))),
    N = rand:uniform(length(Numbers)),
    lists:nth(N, Numbers).

% -----------------------------------------------------------------------------------------
% ------------------------------- FUNKCIJE KOJE ZOVE DEALER -------------------------------
% -----------------------------------------------------------------------------------------

send_dealers_count(Card) ->
    gen_server:cast(player, Card).
