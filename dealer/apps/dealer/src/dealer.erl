
-module(dealer).
-behaviour(gen_server).

-import(string, [concat/2, sub_string/3]).
-import(lists, [nth/2, find_index/2, delete/2, foreach/2]).
-import(timer, [sleep/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).
-export([primjer/1, generate_cards/0, pick_card/1]).
-export([stand/0, bust/0, hit/0, ready/0]).

% my_hand -> zbroj karata u dealerovoj ruci
% deck -> spil karata
% players_no -> ukupan broj igraca
% players_ready -> broj igraca koji su zvali 'stand'
% out -> broj igraca koji su bustali ili dobili blackjack
-record(game_info, {my_hand = 0.0, blackjack = 0, deck = generate_cards(), players_ready = 0, out = 0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% inicijalizacija decka
% generira se 6 standardnih deckova od 52 karte i izvuci prvu dealerovu kartu
init([]) ->
    Deck = generate_deck(),

    % izvuci kartu i ukloni je iz decka
    Card = pick_card(Deck),
    CardValue = card_value(Card),
    NewCards = delete(Card, Deck),

    % ako je karta as, stavi flag blackjack na 1
    if
        CardValue == 11 -> Blackjack = 1;
        CardValue < 11  -> Blackjack = 0
    end,

    InitialState = #game_info{deck = NewCards, my_hand = CardValue, blackjack = Blackjack},
    {ok, InitialState}.



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

% -----------------------------------------------------------------------------------------
% ------------------------------------ CARD OPERATIONS ------------------------------------
% -----------------------------------------------------------------------------------------

% funkcija za generiranje decka karata
% return - standardni deck od 52 karte
generate_cards() ->
    Card_numbers = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"],
    Hearts = [concat(N, " Hearts") || N <- Card_numbers],
    Spades = [concat(N, " Spades") || N <- Card_numbers],
    Diamonds = [concat(N, " Diamonds") || N <- Card_numbers],
    Clubs = [concat(N, " Clubs") || N <- Card_numbers],
    Deck = Hearts ++ Spades ++ Diamonds ++ Clubs,
    Deck.

% generiranje decka od 6 standardnih deckova
% return - deck za igru
generate_deck() ->
    Deck1 = generate_cards(),
    Deck2 = generate_cards(),
    Deck3 = generate_cards(),
    Deck4 = generate_cards(),
    Deck5 = generate_cards(),
    Deck6 = generate_cards(),
    Deck = Deck1 ++ Deck2 ++ Deck3 ++ Deck4 ++ Deck5 ++ Deck6,
    Deck.

% funkcija za izvlacenje nasumicne karte
% return - izvucena karta
pick_card(Cards) ->
    Index = rand:uniform(length(Cards)),
    Card = lists:nth(Index, Cards),
    Card.

% racuna vrijednost pruzene karte
% return - brojcana vrijednost karte
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

% -----------------------------------------------------------------------------------------
% ------------------------------------- CAST HANDLERS -------------------------------------
% -----------------------------------------------------------------------------------------

% svi playeri su spremni za dalje
handle_cast({all_players_ready}, State) ->
    % sacekaj tri sekunde
    sleep(3000),

    %dealer izvlaci drugu kartu
    Deck = State#game_info.deck,
    Card = pick_card(Deck),
    CardValue = card_value(Card),
    NewCards = delete(Card, Deck),

    % provjeri dealerovu kartu
    Blackjack1 = State#game_info.blackjack,
    if
        CardValue == 11 -> Blackjack2 = Blackjack1 + 1;
        CardValue < 11  -> Blackjack2 = Blackjack1
    end,

    % preracunaj ako je dealer izvukao asa
    if
        Blackjack2 == 2 -> HandValue = 12;
        Blackjack2 == 1 -> HandValue = CardValue + State#game_info.my_hand
    end,


    if
        % ako dealer ima vise od 21 u ruci
        HandValue > 21 ->
            lists:foreach(
                fun(ShellPid) ->
                    gen_server:cast(ShellPid, 'bust')
                end,
                State
            ),
            NewCards2 = NewCards;
        % ako dealer ima 21 u ruci
        % -> pozovi funkciju dealer_blackjack
        HandValue == 21 ->
            lists:foreach(
                fun(ShellPid) ->
                    gen_server:cast(ShellPid, 'blackjack')
                end,
                State
            ),
            NewCards2 = NewCards;
        % ako dealer ima 17 ili vise u ruci
        HandValue >= 17 ->
            lists:foreach(
                fun(ShellPid) ->
                    gen_server:cast(ShellPid, HandValue)
                end,
                State
            ),
            NewCards2 = NewCards;
        % ako dealer ima manje od 17 u ruci
        HandValue < 17 ->
            Card2 = pick_card(NewCards),
            NewCards2 = delete(Card2, NewCards),
            CardValue2 = card_value(Card2),
            if
                CardValue2 == 11 -> Blackjack3 = Blackjack2 + 1;
                CardValue2 < 11  -> Blackjack3 = Blackjack2
            end,
            if
                Blackjack3 == 1 -> HandValue2 = 13;
                Blackjack3 == 2 ->
                    HandValue2 = HandValue + 1;
                Blackjack3 == 1 ->
                    if
                        HandValue + CardValue2 > 21 -> HandValue2 = HandValue + 1;
                        true -> HandValue2 = HandValue + 11
                    end
            end,
            lists:foreach(
                fun(ShellPid) ->
                    gen_server:cast(ShellPid, HandValue)
                end,
                State
            )
    end,

    % popravi state
    NewState = NewState = State#game_info{deck = NewCards2, players_ready = 0, my_hand = 0, blackjack = 0, out = 0},
    {noreply, NewState};


handle_cast(Msg, State) ->
    io:format("dealer cast: ~p~n", [Msg]),
    {noreply, State}.

% -----------------------------------------------------------------------------------------
% ------------------------------------- CALL HANDLERS -------------------------------------
% -----------------------------------------------------------------------------------------

% zahtjev za izvlacenje karte
handle_call({hit_request}, _From, State) ->
    % provjera ima li karata u decku - ako nema, generiraj novi deck
    Length = length(State#game_info.deck),
    if
        Length > 52  -> Cards = State#game_info.deck;
        Length =< 52 -> Cards = generate_deck()
    end,

    % izvuci kartu i ukloni je iz decka
    Card = pick_card(Cards),
    NewCards = delete(Card, Cards),

    % azuriraj stanje
    NewState = State#game_info{deck = NewCards},
    {reply, Card, NewState};

% player trazi prve dvije karte i spreman je za rundu
handle_call({player_ready}, _From, State) ->
    % izvuci dvije karte
    Card1 = hit(),
    Card2 = hit(),
    % pripremi return tuple s igracevim kartama i dealerovom kartom
    DealerHand = State#game_info.my_hand,
    ReturnInfo = {Card1, Card2, DealerHand},

    % provjeri ima li player blackjack
    PlayersNo = nodes(),
    if
        Card1 + Card2 == 21 ->
            Out = State#game_info.out + 1,
            Ready = State#game_info.players_ready;
        Card1 + Card2 < 21  ->
            Out = State#game_info.out,
            Ready = State#game_info.players_ready + 1
    end,

    % provjeri jesu li svi spremni -> ako jesu izvlacenje druge karte
    PlayersNotReady = PlayersNo - Out - Ready,
    if
        PlayersNotReady =< 0 -> gen_server:cast(?MODULE, {all_players_ready})
    end,

    % azuriraj stanje
    NewState = State#game_info{out = Out, players_ready = Ready},
    {reply, ReturnInfo, NewState};

% player javlja dealeru da ne zeli vise karata
handle_call({stand_request}, _From, State) ->
    PlayersNo = nodes(),
    Out = State#game_info.out,
    Ready = State#game_info.players_ready + 1,

    % provjeri jesu li svi spremni -> ako jesu izvlacenje druge karte
    PlayersNotReady = PlayersNo - Out - Ready,
    if
        PlayersNotReady =< 0 -> gen_server:cast(?MODULE, {all_players_ready})
    end,

    % azuriraj stanje
    NewState = State#game_info{players_ready = Ready},
    {reply, NewState};

% player javlja dealeru da je bustao
handle_call({bust_request}, _From, State) ->
    PlayersNo = nodes(),
    Out = State#game_info.out + 1,
    Ready = State#game_info.players_ready,

    % provjeri jesu li svi spremni -> ako jesu izvlacenje druge karte
    PlayersNotReady = PlayersNo - Out - Ready,
    if
        PlayersNotReady =< 0 -> gen_server:cast(?MODULE, {all_players_ready})
    end,

    % azuriraj stanje
    NewState = State#game_info{out = Out},
    {reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


% -----------------------------------------------------------------------------------------
% ------------------------------- FUNKCIJE KOJE ZOVE PLAYER -------------------------------
% -----------------------------------------------------------------------------------------

% player trazi kartu od dealera
% return - vrijednost karte
hit() ->
    Card = gen_server:call(?MODULE, {hit_request}),
    CardValue = card_value(Card),
    CardValue.

% player javlja dealeru da ne zeli vise karata
stand() ->
    gen_server:call(?MODULE, {stand_request}).

% player javlja dealeru da je bustao
bust() ->
    gen_server:call(?MODULE, {bust_request}).

% player na početku igre javlja da je spreman
% return value -> tuple oblika {vrijednost_prve_karte, vrijednost_druge_karte, dealerova_karta}
ready() ->
    InitialCards = gen_server:call(?MODULE, {player_ready}),
    InitialCards.

