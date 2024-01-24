Blackjack
=====

Distributed Erlang OTP application

Setup
=====

Instalirati rebar3 - dolje napisano kako u windows powershellu
    
    Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

    choco install rebar3
    
Instalirati erlang, preporučamo verziju 25.2.3 jer se ta koristila za testiranje

Isprobavanje aplikacije
=====
appname je player ili dealer

    (putanja-do-appname-direktorija)> rebar3 release

**Otići na _build/default/rel/appname/bin otvoriti appname.cmd i promijeniti 51. liniju tako da obrišete w da ostane set werl="%bindir%\erl.exe"**

    prije promjene: set werl="%bindir%\werl.exe"
    poslije promjene: set werl="%bindir%\erl.exe"

Ovo radimo jer nam rebar3 builda release za naš operacijski sustav i zato želi pokrenuti werl ali input/output funkcije ne rade kako treba sa werl-om pa ćemo koristiti defaultni erlang shell.

Prvo pokretanje:
**otvoriti shell kao administrator**

    (putanja-do-Blackjack-direktorija)> cd .\_build\default\rel\appname\bin

    ...\bin> ./appname.cmd install
    ...\bin> ./appname.cmd start
    ...\bin> ./appname.cmd console

Ostala pokretanja:
bilo kako otvoriti shell

    (putanja-do-Blackjack-direktorija)> cd .\_build\default\rel\appname\bin
    
    ...\bin> appname.cmd console

Otvoriti će se erlang shell sa pokrenutom aplikacijom. Prati upute za igru i sretno!
