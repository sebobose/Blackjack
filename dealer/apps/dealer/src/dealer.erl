
-module(dealer).
-behaviour(gen_server).

-import(string, [concat/2, sub_string/3]).
-import(lists, [nth/2, find_index/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).
-export([primjer/1, generate_cards/0, number_of_cards/1, game_simulator/0, remove_card/2, pick_card/1, request_card/0]).

-record(player_hand, {bet = 0, sum = 0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    InitialState = #player_hand{},
    gen_server:cast(dealer, start, InitialState),
    {ok, []}.



handle_cast(start, State) ->
    timer:sleep(1000),
    io:format("~nDobrodosli u erlang-blackjack, upisite svoje ime: ~n"),
    Name = io:get_line(""),
    FormattedName = string:trim(Name),
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
    gen_server:cast(dealer, Msg),
    io:format("Primjer poruka: ~p", [Msg]).

% funkcija za generiranje špila karata
generate_cards() ->
    Card_numbers = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"],
    Hearts = [concat(N, " Hearts") || N <- Card_numbers],
    Spades = [concat(N, " Spades") || N <- Card_numbers],
    Diamonds = [concat(N, " Diamonds") || N <- Card_numbers],
    Clubs = [concat(N, " Clubs") || N <- Card_numbers],
    Deck = Hearts ++ Spades ++ Diamonds ++ Clubs,
    Deck.

% funkcija koja vraća broj preostalih karata
number_of_cards(X) ->
    Length = length(X),
    Length.

% izvlacenje karte i uklanjanje iste iz spila
pick_card(Cards) ->
    Index = rand:uniform(number_of_cards(Cards)),
    Card = lists:nth(Index, Cards),
    Card.

remove_card(X, Cards) ->
    NewCards = [N || N <- Cards, N /= X],
    NewCards.

card_value(Card) ->
    CardValue = string:sub_string(Card, 1, 1),
    case CardValue of
        "A" -> 11;
        "1" -> 10;
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
        "9" -> 9
    end.



% simulacija igre i poziva funkcija
game_simulator() ->
    Cards = generate_cards(),
    io:format("Karte su generirane! ~n"),

    io:format("Broj karata u spilu: ~p~n", [number_of_cards(Cards)]),
    io:format("~n --- Dealer vuce 1. kartu --- ~n"),
    Card1 = pick_card(Cards),
    NewCards1 = remove_card(Card1, Cards),
    io:format("Izvucena je karta: ~p~n", [Card1]),
    CardValue1 = card_value(Card1),
    io:format("Vrijednost ruke: ~p~n", [CardValue1]),
    io:format("Broj karata u spilu: ~p~n", [number_of_cards(NewCards1)]),

    io:format("~n --- Dealer vuce 2. kartu --- ~n"),
    Card2 = pick_card(Cards),
    NewCards2 = remove_card(Card2, Cards),
    io:format("Izvucena je karta: ~p~n", [Card2]),
    CardValue2 = card_value(Card2) + CardValue1,
    io:format("Vrijednost ruke: ~p~n", [CardValue2]),
    io:format("Broj karata u spilu: ~p~n", [number_of_cards(NewCards2)]),

    if CardValue2 < 16 -> io:format("~n --- Dealer vuce jos jednu kartu --- ~n")
    ; CardValue2 < 21 -> io:format("~n --- Provjere --- ~n")
    ; CardValue2 >= 21 -> io:format("~n --- Dealer bust --- ~n")
    end.

handle_call({draw_card_request}, _From, State) ->
    Cards = generate_cards(),
    io:format("Broj karata u spilu: ~p~n", [number_of_cards(Cards)]),
    io:format("~n --- Player vuce kartu --- ~n"),
    Card = pick_card(Cards),
    %NewCards = remove_card(Card, Cards), ---> ovo sredi u pravom spilu
    io:format("Izvucena je karta: ~p~n", [Card]),
    CardValue = card_value(Card),
    NewSum = State#player_hand.sum + CardValue,
    NewState = State#player_hand{sum = NewSum},
    {reply, Card, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% funkcija gdje player trazi kartu od dealera
% prvo se poziva gen_server:call
% handle_call
% funkcija vraca novce playeru, 0 ili dobitak
request_card() ->
    NewState = gen_server:call(?MODULE, {draw_card_request}),
    Sum = NewState#player_hand.sum,
    Bet = NewState#player_hand.bet,
    if
        Sum > 21 -> 0;
        Sum == 21 -> 1.5 * Bet
    end.




