Blackjack
=====

Distributed Erlang OTP application

Setup
=====

**Vjerojatno sam propustio neke korake jer sam se zezo s ovim par dana, ako nesto ne funkcionira javite se, iako ce vam vjerojatno gpt znat bolje i brze objasnit :)**

Instaliraj rebar3 - ja sam pito gpt-a i reko mi je da preko chocolateya to instaliram
    
    Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

    choco install rebar3
    
Instaliraj 25.2.3 verziju erlanga s njihove stranice (u novijim verzijama su promijenili funkcionalnost gen_servera i ne radi upisivanje imena pri startu aplikacije)

Isprobavanje koda
=====
appname je player ili dealer

    (putanja-do-Blackjack-direktorija)> rebar3 release -n appname

**Odi na _build/default/rel/player/bin i otvori (appname).cmd i promijeni 51. liniju tako da obrises w da ostane set werl="%bindir%\erl.exe"**

    prije promjene: set werl="%bindir%\werl.exe"
    poslije promjene: set werl="%bindir%\erl.exe"

Ovo radimo jer nam rebar3 builda release za nas operacijski sustav i zato zeli pokrenut werl ali input/output funkcije ne rade kako treba sa werl-om pa cemo koristiti defaultni erlang shell.

Prvo pokretanje:
otvoriti shell kao administrator

    (putanja-do-Blackjack-direktorija)> cd .\_build\default\rel\player\bin

    ...\bin> (appname).cmd install
    ...\bin> (appname).cmd start
    ...\bin> (appname).cmd console

Ostala pokretanja:
bilo kako otvoriti shell

    (putanja-do-Blackjack-direktorija)> cd .\_build\default\rel\player\bin
    
    ...\bin> (appname).cmd console

Otvorit ce se erlang shell i pokrenut (appname)_app.erl koji ce pokrenut (appname).erl koji radi kao gen_server

**Ne mozes imat otvorena oba shella za playera i dealera**

Zasad sam dodao upisivanje imena da vidim kako funkcionira input od korisnika i funkciju primjer koja se moze pokrenut iz shella. Dok se ne napravi spajanje izmedu playera i dealera testiraj kod da zoves funkcije iz shella na taj nacin.

