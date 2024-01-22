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
-export([poruka/1, generate_card/0, calculate_sum/1, send_dealers_count/1, end_game/1]).

% hand --> karte koje sam povukao
% stake --> ulog
% money --> balans
% error --> error poruka
-record(state, {hand = [], stake = 0, money = 0, error = "none", dealer = 0, dealerHost}).

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

% message_dealer(Msg) ->
%     rpc:call(?DEALER, dealer, poruka, [Msg]).

% -----------------------------------------------------------------------------------------
% ------------------------------------ CARD OPERATIONS ------------------------------------
% -----------------------------------------------------------------------------------------

card_value(Card) ->
    case Card of
        "A" -> 1;
        "10" -> 10;
        "J" -> 10;
        "Q" -> 10;
        "K" -> 10;
        "2" -> 2;
        "3" -> 3;
        "4" -> 4;
        "5" -> 5;
        "6" -> 6;
        "7" -> 7;
        "8" -> 8;
        "9" -> 9;
        _ -> Card
    end.

% Function to apply transformation to all elements of the list
transform_list(List) ->
    transform_list(List, []).

transform_list([], Acc) ->
    % Base case: The list is empty, return the accumulated result
    lists:reverse(Acc);
transform_list([Head | Tail], Acc) ->
    % Recursive case: Process the head of the list and continue with the tail
    Transformed = card_value(Head),
    transform_list(Tail, [Transformed | Acc]).

calculate_sum(Cards) ->
    Cards2= transform_list(Cards),
    calculate_sum(Cards2, 0, 0).

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
    case State#state.error of
        "dealers_count" ->
            io:format("~nDEALEROVA SUMA:  ~p~n", [State#state.dealer]),
            Count = element(1, calculate_sum(State#state.hand)),
            io:format("~nVAŠA SUMA:       ~p~n", [Count]),
            if
                Count == 21 andalso  length(State#state.hand)==2 ->
                    io:format("~nBLACKJACK!!!.\n"),
                    UpdatedState = State#state{
                        hand = [],
                        error = "none",
                        dealer = 0,
                        money = State#state.money + State#state.stake * 1.5
                    };
                Count > 21 ->
                    io:format("~nBUST.\n"),
                    UpdatedState = State#state{
                        hand = [],
                        error = "none",
                        dealer = 0,
                        money = State#state.money - State#state.stake
                    };
                State#state.dealer == 'blackjack' ->
                    io:format("~nDEALER POBJEĐUJE.\n"),
                    UpdatedState = State#state{
                        hand = [],
                        error = "none",
                        dealer = 0,
                        money = State#state.money - State#state.stake
                    };
                State#state.dealer == 'bust' ->
                    io:format("~nPOBJEDA!\n"),
                    UpdatedState = State#state{
                        hand = [],
                        error = "none",
                        dealer = 0,
                        money = State#state.money + State#state.stake
                    };
                Count < State#state.dealer ->
                    io:format("~nDEALER POBJEĐUJE.\n"),
                    UpdatedState = State#state{
                        hand = [],
                        error = "none",
                        dealer = 0,
                        money = State#state.money - State#state.stake
                    };
                Count > State#state.dealer ->
                    io:format("~nPOBJEDA!\n"),
                    UpdatedState = State#state{
                        hand = [],
                        error = "none",
                        dealer = 0,
                        money = State#state.money + State#state.stake
                    };
                true ->
                    io:format("~nPUSH.\n"),
                    UpdatedState = State#state{hand = [], error = "none", dealer = 0}
            end,
            io:format("\nBALANS:   ~p\nULOG:     ~p\n\n", [UpdatedState#state.money, UpdatedState#state.stake]),
            gen_server:cast(player, {change, UpdatedState}),
            loop_while_quit(UpdatedState);
        "wait" ->
            io:format("~nČEKAMO DEALEROVE KARTE ...~n~n"),
            {noreply, State};
        _ ->
            case io:get_line("INPUT:   ") of
                "quit\n" ->
                    {noreply, State};
                Input ->
                    NewState = element(2, handle_user_input(Input, State)),
                    case NewState#state.error of
                        "bust" ->
                            io:format("~nBUST!~n~n"),
                            handle_user_input("stand\n", NewState),
                            loop_while_quit(NewState#state{
                                hand = [],
                                money = NewState#state.money - NewState#state.stake,
                                error = "wait"
                            });
                        "none" ->
                            loop_while_quit(NewState);
                        "stake" ->
                            io:format("~nMorate staviti ulog.~n~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "money" ->
                            io:format("~nNedovoljno novaca na računu(~p), smanjite ulog(~p).~n~n", [
                                NewState#state.money, NewState#state.stake
                            ]),
                            loop_while_quit(NewState#state{error = "none"});
                        "card" ->
                            io:format("~nNe možete ostati ako nemate karata.~n~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "starterror" ->
                            io:format("~nIgra je već započeta.~n~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "hiterror" ->
                            io:format("~nMorate započeti igru.~n~n"),
                            loop_while_quit(NewState#state{error = "none"});
                        "wait" ->
                            loop_while_quit(NewState#state{error = "wait"});
                        _ ->
                            io:format("~nError: ~p~n~n", [NewState#state.error]),
                            loop_while_quit(NewState#state{error = "none"})
                    end
            end
    end.

% Handle user input (hit, stand, or other actions)
handle_user_input("hit\n", State) ->
    handle_func(hit, State);
handle_user_input("stand\n", State) ->
    handle_func(stand, State);
handle_user_input("start\n", State) ->
    handle_func(startgame, State);
handle_user_input("stake\n", State) ->
    io:format("~nPromijenite svoj ulog: "),
    StakeStr = io:get_line(""),
    {ok, [Stake], _} = io_lib:fread("~d", StakeStr),
    gen_server:cast(player, {change, State#state{stake = Stake}}),
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
    io:format("~nInvalid input.~n"),
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
handle_func(startgame, State) when State#state.stake =< 0 ->
    gen_server:cast(player, {change, State#state{stake = "stake"}}),
    {noreply, State#state{error = "stake"}};
handle_func(startgame, State) when State#state.stake > State#state.money ->
    gen_server:cast(player, {change, State#state{stake = "money"}}),
    {noreply, State#state{error = "money"}};
handle_func(startgame, State) when length(State#state.hand) > 0 ->
    gen_server:cast(player, {change, State#state{stake = "starterror"}}),
    {noreply, State#state{error = "starterror"}};
handle_func(startgame, State) ->
    Resp = rpc:call(State#state.dealerHost, dealer, ready, []),
    Hand = [element(1, Resp), element(2, Resp)],
    DealersCard = element(3, Resp),
    io:format("~nKARTE:           ~p~n", [Hand]),
    io:format("~nSUMA:            ~p~n", [calculate_sum(Hand)]),
    io:format("~nDEALEROVA KARTA: ~p~n~n", [DealersCard]),
    gen_server:cast(player, {change, State#state{hand = Hand, dealer = DealersCard}}),
    {noreply, State#state{hand = Hand, dealer = DealersCard}};
handle_func(hit, State) when length(State#state.hand) == 0 ->
    UpdatedState = State#state{error = "hiterror"},
    gen_server:cast(player, {change, UpdatedState}),
    {noreply, UpdatedState};
handle_func(hit, State) ->
    Card = rpc:call(State#state.dealerHost, dealer, hit, []),
    UpdatedState = State#state{hand = [Card | State#state.hand]},
    io:format("NOVA KARTA:   ~p~n", [Card]),
    io:format("KARTE:        ~p~n", [UpdatedState#state.hand]),
    io:format("SUMA:         ~p~n~n", [calculate_sum(UpdatedState#state.hand)]),
    case
        {
            element(2, calculate_sum(UpdatedState#state.hand)),
            element(1, calculate_sum(UpdatedState#state.hand)) > 21
        }
    of
        {hard, true} ->
            gen_server:cast(
                player, {change, State#state{hand = [Card | State#state.hand], error = "bust"}}
            ),
            {noreply, State#state{hand = [Card | State#state.hand], error = "bust"}};
        _ ->
            gen_server:cast(player, {change, UpdatedState}),
            {noreply, UpdatedState}
    end;
handle_func(stand, State) when State#state.hand == [] ->
    UpdatedState = State#state{error = "card"},
    gen_server:cast(player, {change, UpdatedState}),
    {noreply, UpdatedState};
handle_func(stand, State) ->
    Sum = element(1, calculate_sum(State#state.hand)),
    io:format("~nSTAND!~n~nKARTE:  ~p~n", [State#state.hand]),
    io:format("SUMA:   ~p~n", [Sum]),
    UpdatedState = State#state{error = "wait"},
    gen_server:cast(player, {change, UpdatedState}),
    rpc:call(State#state.dealerHost, dealer, stand, []),
    {noreply, UpdatedState}.

handle_cast(start, _) ->
    timer:sleep(1000),
    io:format("Povezivanje s dealerom ...~n"),
    {ok, Socket} = gen_udp:open(12345, [binary, {active, false}]),
    {ok, {_, _, DealerBinary}} = gen_udp:recv(Socket, 0),
    gen_udp:close(Socket),
    Dealer = list_to_atom(binary_to_list(DealerBinary)),
    net_adm:ping(Dealer),
    io:format("Povezano!~n"),
    MoneyStr = io:get_line("Upisite svoj balans: "),
    {ok, [Money], _} = io_lib:fread("~d", MoneyStr),
    StakeStr = io:get_line("Upisite svoj ulog: "),
    {ok, [Stake], _} = io_lib:fread("~d", StakeStr),
    io:format("Dobrodosli!~nBalans: ~p~nUlog:   ~p~n", [Money, Stake]),
    io:format("Za pomoc u igri napisite help!~n~n"),
    UpdatedState = #state{
        hand = [], stake = Stake, money = Money, error = "none", dealer = 0, dealerHost = Dealer
    },
    loop_while_quit(UpdatedState),
    {noreply, UpdatedState};
handle_cast({change, Delta}, _State) ->
    {noreply, Delta};
handle_cast(Msg, State) ->
    case State#state.error of 
        "wait" ->
            UpdatedState = State#state{dealer = Msg, error = "dealers_count"},
            gen_server:cast(player, {change, UpdatedState}),
            loop_while_quit(UpdatedState);
        _ ->
            UpdatedState = State
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

end_game(Msg) ->
    gen_server:cast(?MODULE, Msg).
