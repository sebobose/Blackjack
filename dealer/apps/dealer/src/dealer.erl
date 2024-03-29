
-module(dealer).
-behaviour(gen_server).

-import(string, [concat/2, sub_string/3]).
-import(lists, [nth/2, find_index/2, delete/2, foreach/2]).
-import(timer, [sleep/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).
-export([primjer/1, generate_cards/0, pick_card/1, test_draw_while/0]).
-export([stand/0, bust/0, hit/0, ready/0]).

% my_hand -> zbroj karata u dealerovoj ruci
% deck -> spil karata
% players_no -> ukupan broj igraca
% players_ready -> broj igraca koji su zvali 'stand'
% out -> broj igraca koji su bustali ili dobili blackjack
-record(game_info, {my_hand = 0.0, blackjack = 0, deck = generate_cards(), players_ready = 0, out = 0, socket}).

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
        CardValue >= 11 -> Blackjack = 1;
        CardValue < 11  -> Blackjack = 0
    end,
    gen_server:cast(?MODULE, start),
    InitialState = #game_info{deck = NewCards, my_hand = CardValue, blackjack = Blackjack},
    {ok, InitialState}.

handle_info({send_udp_message, Host}, State = #game_info{socket = Socket}) ->
    ok = gen_udp:send(Socket, {255,255,255,255}, 12345, Host),
    {noreply, State};

handle_info(Info, State) ->
    io:format("dealer info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #game_info{socket = Socket}) ->
    ok = gen_udp:close(Socket),
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
        "9" -> 9;
        true -> ignore
    end.

card_value_for_player(Card) ->
    CardValue = string:sub_string(Card, 1, 1),
    case CardValue of
        "A" -> 1;
        "1" -> 10;
        "J" -> "J";
        "Q" -> "Q";
        "K" -> "K";
        "2" -> 2;
        "3" -> 3;
        "4" -> 4;
        "5" -> 5;
        "6" -> 6;
        "7" -> 7;
        "8" -> 8;
        "9" -> 9;
        true -> ignore
    end.

% -----------------------------------------------------------------------------------------
% ------------------------------------- CAST HANDLERS -------------------------------------
% -----------------------------------------------------------------------------------------

handle_cast(start, State) ->
    timer:sleep(1000),
    Dealer = string:concat("dealer@", net_adm:localhost()),
    timer:send_interval(5000, {send_udp_message, Dealer}),
    {ok, Socket} = gen_udp:open(0, [{broadcast, true}]),
    {noreply, State#game_info{socket = Socket}};

% svi playeri su spremni za dalje
handle_cast({all_players_ready}, State) ->
    % sacekaj tri sekunde
    sleep(1000),

    %dealer izvlaci drugu kartu
    Deck = State#game_info.deck,
    Card = pick_card(Deck),
    CardValue = card_value(Card),
    NewCards = delete(Card, Deck),

    % provjeri dealerovu kartu
    Blackjack1 = State#game_info.blackjack,
    if
        CardValue >= 11 -> Blackjack2 = Blackjack1 + 1;
        CardValue < 11  -> Blackjack2 = Blackjack1
    end,

    % preracunaj ako je dealer izvukao asa
    if
        Blackjack2 == 2 -> HandValue = 12;
        Blackjack2 == 1 -> HandValue = CardValue + State#game_info.my_hand;
        true -> HandValue = State#game_info.my_hand
    end,

    if
    % ako dealer ima vise od 21 u ruci
        HandValue > 21 ->
            message_all('bust'),
            NewCards2 = NewCards;
    % ako dealer ima 21 u ruci
    % -> pozovi funkciju dealer_blackjack
        HandValue == 21 ->
            message_all('blackjack'),
            NewCards2 = NewCards;
    % ako dealer ima 17 ili vise u ruci
        HandValue >= 17 ->
            message_all(HandValue),
            NewCards2 = NewCards;
    % ako dealer ima manje od 17 u ruci
        HandValue < 17 ->
            %Card2 = pick_card(NewCards),
            %NewCards2 = delete(Card2, NewCards),
            %CardValue2 = card_value(Card2),
            %if
            %    CardValue2 >= 11 -> Blackjack3 = Blackjack2 + 1;
            %    CardValue2 < 11  -> Blackjack3 = Blackjack2
            %end,
            %if
            %    Blackjack3 == 1 -> HandValue2 = 13;
            %    Blackjack3 == 2 ->
            %        HandValue2 = HandValue + 1;
            %    Blackjack3 == 3 ->
            %        if
            %            HandValue + CardValue2 > 21 -> HandValue2 = HandValue + 1;
            %            true -> HandValue2 = HandValue + 11
            %        end
            %end,
            {HandValue2, NewCards2} = draw_while(HandValue, 17, NewCards),
            if
                HandValue2 > 21 -> message_all('bust');
                true -> message_all(HandValue2)
            end

    end,
    % popravi state

    % izvuci kartu i ukloni je iz decka
    CardNewRound = pick_card(NewCards2),
    CardValueNewRound = card_value(CardNewRound),
    CardsNewRound = delete(CardNewRound, NewCards2),

    % ako je karta as, stavi flag blackjack na 1
    if
        CardValueNewRound >= 11 -> BlackjackNewRound = 1;
        CardValueNewRound < 11  -> BlackjackNewRound = 0
    end,
    
    NewState = State#game_info{deck = CardsNewRound, players_ready = 0, my_hand = CardValueNewRound, blackjack = BlackjackNewRound, out = 0},
    {noreply, NewState};


handle_cast(Msg, State) ->
    io:format("dealer cast: ~p~n", [Msg]),
    {noreply, State}.

message_all(X)->
    rpc:multicall(nodes(), player, end_game, [X]).

% -----------------------------------------------------------------------------------------
% --------------------------------------- INTERNAL ----------------------------------------
% -----------------------------------------------------------------------------------------

test_draw_while() ->
    Cards = generate_deck(),
    {Sum, NewCards} = draw_while(0, 17, Cards),
    {Sum, length(NewCards)}.

draw_while(CurrentSum, MinSum, Cards) ->
    if
        CurrentSum >= MinSum-> Sum = CurrentSum, NewCards = Cards;
        true ->
            % izvuci kartu i ukloni je iz decka
            Card = pick_card(Cards),
            TempNewCards = delete(Card, Cards),
            CardValue = card_value(Card),
            if
                CardValue == 11 andalso CurrentSum + 11 > 21 -> TempSum = CurrentSum + 1;
                true -> TempSum = CurrentSum + CardValue
            end,
            {Sum, NewCards} = draw_while(TempSum, MinSum, TempNewCards)
    end,
    {Sum, NewCards}.


%reduce_value(CardValue) ->
%    if
%        CardValue == 11 -> CardValueReduced = 1;
%        true -> CardValueReduced = CardValue
%    end,
%    CardValueReduced.

draw_two(Deck) ->
    Length = length(Deck),
    if
        Length > 53  -> Cards = Deck;
        Length =< 53 -> Cards = generate_deck()
    end,

    % izvuci kartu i ukloni je iz decka
    Card1 = pick_card(Cards),
    NewCards1 = delete(Card1, Cards),

    % izvuci kartu i ukloni je iz decka
    Card2 = pick_card(NewCards1),
    NewCards2 = delete(Card2, NewCards1),
    NewCards2,

    CardValueReduced1 = card_value_for_player(Card1),
    CardValueReduced2 = card_value_for_player(Card2),
    {CardValueReduced1, CardValueReduced2, NewCards2}.

% -----------------------------------------------------------------------------------------
% ------------------------------------- CALL HANDLERS -------------------------------------
% -----------------------------------------------------------------------------------------

% zahtjev za izvlacenje karte
handle_call({hit_request}, _From, State) ->
    Length = length(State#game_info.deck),
    if
        Length > 100  -> Cards = State#game_info.deck;
        Length =< 100 -> Cards = generate_deck()
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
    {Card1, Card2, NewCards} = draw_two(State#game_info.deck),

    % pripremi return tuple s igracevim kartama i dealerovom kartom
    DealerHand = State#game_info.my_hand,
    ReturnInfo = {Card1, Card2, DealerHand},

    % provjeri ima li player blackjack
    PlayersNo = length(nodes()),
    if
        Card1 + Card2 >= 21 ->
            Out = State#game_info.out + 1,
            Ready = State#game_info.players_ready;
        Card1 + Card2 < 21  ->
            Out = State#game_info.out,
            Ready = State#game_info.players_ready + 1;
        true -> Out = 0, Ready = 0
    end,

    % provjeri jesu li svi spremni -> ako jesu izvlacenje druge karte
    PlayersNotReady = PlayersNo - Out - Ready,
    if
        PlayersNotReady =< 0 -> gen_server:cast(?MODULE, {all_players_ready});
        true -> ignore
    end,

    % azuriraj stanje
    NewState = State#game_info{out = Out, players_ready = Ready, deck = NewCards, my_hand = DealerHand},

    {reply, ReturnInfo, NewState};

% player javlja dealeru da ne zeli vise karata
handle_call({stand_request}, _From, State) ->
    PlayersNo = length(nodes()),
    Out = State#game_info.out,
    Ready = State#game_info.players_ready + 1,

    % provjeri jesu li svi spremni -> ako jesu izvlacenje druge karte
    PlayersNotReady = PlayersNo - Out - Ready,
    if
        PlayersNotReady =< 0 -> gen_server:cast(?MODULE, {all_players_ready});
        true -> ignore
    end,

    % azuriraj stanje
    NewState = State#game_info{players_ready = Ready},
    {reply, ok, NewState};

% player javlja dealeru da je bustao
handle_call({bust_request}, _From, State) ->
    PlayersNo = length(nodes()),
    Out = State#game_info.out + 1,
    Ready = State#game_info.players_ready,

    % provjeri jesu li svi spremni -> ako jesu izvlacenje druge karte
    PlayersNotReady = PlayersNo - Out - Ready,
    if
        PlayersNotReady =< 0 -> gen_server:cast(?MODULE, {all_players_ready});
        true -> ignore
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
    CardValue = card_value_for_player(Card),
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