
-module(dealer).
-behaviour(gen_server).

-import(string, [concat/2, sub_string/3]).
-import(lists, [nth/2, find_index/2, delete/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).
-export([primjer/1, generate_cards/0, number_of_cards/1, pick_card/1, request_card/0]).

% my_hand -> zbroj karata u dealerovoj ruci
% deck -> spil karata
% player_hands -> zbroj karata u ruci svakog igraca
% num_players -> broj igraca koji ujedno sluzi i kao id
% -> pri svakom spajanju igraca, igrac dobije trenutni broj igraca kao svoj ID i broj igraca se poveca za jedan
-record(game_info, {my_hand = 0.0, deck = generate_cards()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Deck = generate_cards(),
    InitialState = #game_info{deck = Deck},
    {ok, InitialState}.

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

% player s brojem ID zeli vuci kartu
handle_call({draw_card_request}, _From, State) ->
    % izvlacenje karte i pripremanje novog spila bez iste
    Cards = State#game_info.deck,
    Card = pick_card(Cards),
    NewCards = delete(Card, Cards),

    % dodavanje vrijednosti u ukupnu sumu igraca
    CardValue = card_value(Card),

    % azuriraj stanje
    NewState = State#game_info{deck = NewCards},
    {reply, CardValue, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% funkcija gdje player trazi kartu od dealera
% prvo se poziva gen_server:call
% handle_call
% funkcija vraca novce playeru, 0 ili dobitak
request_card() ->
    {reply, DrawnCard, UpdatedState} = gen_server:call(?MODULE, {draw_card_request}),
    DrawnCard.

% ako je suma blackjack -> napravi atom blackjack i vrati to