
# Domácí úkol 3 -- Multiplayer

Hlavním úkolem ve třetím domácím úkolu je procvičit si konkurentní programování
a synchronizační primitiva, např.
[Chan](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-Chan.html)
a
[MVar](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html).

Run-time Haskellu obsahuje vlastní implementaci pollování, díky tomu není
potřeba řešit tradiční problémy s blokujícím čtením a zápisem do socketu, a
konkurentní programování (tj. vícevláknové pomocí jednoho skutečného vlákna
operačního systému) jde implementovat jednoduše pomocí `forkIO`. "Odlehčená"
vlákna, která `forkIO` vyrábí, váš program prakticky nic nestojí a můžete s
nimi poměrně jednoduše programovat všelijaké síťové aplikace, ve kterých byste
v jiných jazycích buď museli explicitně spouštět víc (drahých) vláken
operačního systému, nebo pollování museli implementovat ručně.

## Předběžnosti

K dispozici máte [jednoduchou implementaci serveru "hry"
GameOfLife](golserver.hs). Server umožnuje víc hráčům přes internet editovat a
testovat jedinou "hrací plochu" s jedním buněčným automatem. Velikost plochy je
pro účely domácího úkolu vždycky přesně 20×20.

Komunikační protokol serveru je jednoduchý: Klienti se připojují pomocí
obyčejného TCP spojení na dohodnutém portu, posílají serveru jednořádkové
textové příkazy, a přijímají jednořádkové textové updaty stavu hrací plochy.

Server umí posílat následující tři typy zpráv:

- `error` klientovi říká, že poslal nezpracovatelnou zprávu
- `wrong coords` klientovi říká, že poslal požadavek na neexistující pozici na
  hrací ploše
- `board [...]` klientovi sděluje, že stav hrací plochy na serveru se změnil.
  Ve zprávě je následně zakódovaná celá hrací plocha, jako řetězec délky 400
  (20×20), který obsahuje buď tečky (`.`) reprezentující prázdné místo, nebo
  křížky (písmeno `x`) reprezentující živé buňky. Hrací plocha je zakódovaná
  postupně po řádcích.

Klient serveru může poslat následující jednořádkové příkazy:
- `step`, tím serveru říká, aby provedl 1 krok simulace buněčného automatu
- `flip x y` serveru říká, aby změnil obsah hrací plochy v x-tém sloupci a y-tém
  řádku (tj. chybějící buňku vyrobil, případně existující buňku smazal)
- `quit`, po kterém server ukončí spojení
- `poll`, po kterém server explicitně pošle klientovi současný stav hrací
  plochy. (Není nutné pollovat pořád, server aktivně posílá updaty všem
  klientům při jakékoliv změně stavu.)

Komunikaci se serverem si můžete vyzkoušet ručně pomocí `telnet`u nebo netcatu
(`nc`), zhruba takhle:

```
~ $ telnet localhost 10042
Trying ::1...
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
board ................................................................................................................................................................................................................................................................................................................................................................................................................
flip 1 0
board .x..............................................................................................................................................................................................................................................................................................................................................................................................................
flip 2 1
board .x....................x.........................................................................................................................................................................................................................................................................................................................................................................................
flip 2 2
board .x....................x...................x.....................................................................................................................................................................................................................................................................................................................................................................
flip 1 2
board .x....................x..................xx.....................................................................................................................................................................................................................................................................................................................................................................
flip 0 2
board .x....................x.................xxx.....................................................................................................................................................................................................................................................................................................................................................................
step
board ....................x.x..................xx..................x..................................................................................................................................................................................................................................................................................................................................................
step
board ......................x.................x.x..................xx.................................................................................................................................................................................................................................................................................................................................................
step
board .....................x....................xx.................xx.................................................................................................................................................................................................................................................................................................................................................
step
board ......................x....................x.................xxx................................................................................................................................................................................................................................................................................................................................................
quit
Connection closed by foreign host.
 ~ $
```

V příkladu klient nakreslil standardní
[glider](https://en.wikipedia.org/wiki/Glider_(Conway's_Life)) do horního
levého rohu hrací plochy a nechal ho udělat 4 kroky. (Tím se glider posunul o 1
políčko dolů a 1 doprava.)

Můžete si vyzkoušet i [pomocný testovací program](sendcmd.hs), který resolvuje
adresu serveru, připojí se k ní, pošle jedíný příkaz (`flip 0 0`) a na výstup
vypíše jediný přijatý řádek. (Program se odpojuje celkem hrubě zavřením
socketu, ne příkazem `quit`. Server proto bude pravděpodobně vypisovat
odpovídající stížnosti o neočekávaném odpojení klienta.)

## Část první

Vezměte si implementaci hrací plochy z robodungeonu (úkolu 1), zjednodušte ji
na Game of Life (tj. políčka můžete reprezentovat poměrně triviálně binární
hodnotou), připojte ji na server, uživatelovi zobrazujte dění na serveru a
nechte ho nějakým rozumným způsobem posílat příkazy "flip" a "step".

Doporučený postup implementace je následující:
- Implementujte přijímací a vysílací část jako samostatné "lehké" thready
  (spuštěné pomocí `forkIO`). Ke připojení na zbytek programu použijte vhodné
  komunikační prostředky (reference, kanály, zamykací proměnné), které
  "zobrazovací" části programu dovolí fungovat nezávisle (asynchronně) na
  komunikaci se serverem. Konkrétně:
  - Pokud jste použili Gloss, jakákoliv blokující operace spuštěná v hlavním
    threadu způsobí zamrznutí celého kreslícího cyklu (což asi nechcete).
  - Pokud jste použili Brick, musíte navíc Brickovému frontendu občas říct, že
    se má probudit a překreslit (narozdíl od Glossu to nedělá pořád).
    Konkrétně, Brick občas jen čeká na vstup a nevšíma si ničeho, co by se
    mohlo dít na síti nebo jiných komunikačních kanálech. To se dá vyřešit
    vyrobením vlastního typu eventu, který Bricku můžete poslat např. v
    případě, že ze serveru přijde nějaká nová informace; ideálně podle [tohohle
    tutorialu](https://samtay.github.io/posts/introduction-to-brick) --
    hledejte sekci "Variable speed".
- Hrací plochu sami nijak nemodifikujte (ani v reakci na uživatelský vstup) --
  prostě jen zobrazujte verzi, kterou vám naposledy poslal server. Jediná věc,
  kterou potřebujete navíc, je "kurzor". Pozici kurzoru serveru nijak
  neoznamujete (obecně je naprosto nezávislá na čemkoliv co si server zrovna
  myslí).
- V případě nějaké uživatelské akce (uživatel může udělat prakticky jen "flip"
  nebo "step") prostě pošlete odpovídající zprávu vláknu, které zajišťuje
  komunikaci se serverem. Zprávu nechte dojít na server, časem vám přijde
  odpověď která obsahuje nový stav plochy (s potenciálními dalšími updaty od
  ostatních klientů). Váš program by na odpověď neměl čekat, a uživateli by měl
  dovolit provádět další akce -- představte si, že server laguje, a máte
  nakreslit [tohle](https://www.conwaylife.com/wiki/Sidecar_gun).
- Různá možná selhání sítě (např. ztrátu spojení) nebo extrémní latenci nijak
  ošetřovat nemusíte.


### Detaily

- Nejspíš budete chtít použít nějakou kombinaci síťování, haskellových
  "lehkých" vláken a konkurentních programovacích primitiv. Pomůcky pro
  komunikaci se sítí najdete v `Network.Socket`, vlákna v `Control.Concurrent`
  a komunikační prostředky v `Control.Concurrent.MVar`, případně v
  `Control.Concurrent.Chan` nebo v nějaké alternativě z balíku `stm`. Je vhodné
  se inspirovat kódem serveru.
- Adresu a port serveru (defaultní je `10042`) přečtěte z příkazového řádku.
  Program by měl podporovat na pozici nezávislé argumenty `-a <ip_adresa>` a
  `-p <port>`, a stěžovat si (nebo se odmítnout spustit) při nedostatečně
  specifikovaných parametrech, nebo výskytu neinterpretovatelných parametrů.

Navíc si vyberte jeden ze dvou následujících volitelných úkolů (můžete
samozřejmě zvládnout i oba dva).

## Část druhá (varianta A)

Na naparsování parametrů z příkazové řádky použijte knihovnu
`optparse-applicative`. Pro oba parametry implementujte i "dlouhou" verzi
(např. `--address`), a v případě chybně zadaných parametrů (nebo když si
uživatel řekne o `--help`) vypište nápovědu.

## Část druhá (varianta B)

Modifikace lokálního naprogramujte velmi hezky a supermoderně, pomocí monády
`State` a optiky (tj. `Control.Lens` nebo `Lens.Micro.*`). Konkrétně zkuste
příchozí eventy zpracovávat např. takhle:

```hs
handleCursorRight = runState $ do
  cursorX += 1
  cursorX . filtered (>19) .= 19
```

(Kód zvětší hodnotu v cursorX o 1, a výsledek zarovná na 19. Podobně jde napsat
např. `cursorX . filtered (<19) += 1`.)

## Bonus

Svůj zdrojový kód okomentujte pomocí haddockových docstringů (začínají svislítkem,
tj. např. `--| ... ` nebo `{-| ...` se vysází do dokumentace, podobně jako
`/** ...` při používání Doxygenu). Zařiďte aby dokumentace byla sestavitelná pomocí
`cabal haddock`. Zároveň nechte pěkně vysázet zdrojový kód jako HTML (přidejte
parametr `--haddock-hyperlink-source`).

## Hinty

- Vaše aplikace _jen zobrazuje stav serveru_; logiku Game Of Life implementovat
  nemusíte, a serveru můžete úplně věřit.
- Pokud je tohle první síťová aplikace kterou programujete, API (odvozené od
  Berkeleyovských socketů) vás může poněkud šokovat. Většinu síťového kódu
  naštěstí stačí zkopírovat z dodaných příkladů. Hlavní rozdíl oproti serveru
  je ten, že nebudete používat "naslouchací" kombo `bind`-`listen`-`accept`,
  ale použijete jen jedinou funkci
  [`connect`](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:connect)
  která zařídí vytvoření TCP spojení s adresou serveru.
- IP adresu z příkazového řádku neparsujte ručně. (Pokud se ji
  rozhodnete parsovat ručně, vzpomeňte si na IPv6!). Místo toho chcete použít
  [`getAddrInfo`](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:getAddrInfo).
- V některých situacích chcete použít stavový výpočet a IO najednou (např. v
  event-handlerech některé akce mění pozice kurzoru, a jiné akce odesílají
  zprávu serveru). V takovém případě se dost hodí transformery. Extra příklad
  kombinace `State` + `IO` je zde:

```hs
import Control.Monad.State
    -- Monádový "návratový" typ ("a")--.
           -- Vnitřní monáda -------.  |
                -- Typ stavu ---.   |  |
		--              |   |  |
stateAndIOComputation :: StateT Int IO ()
stateAndIOComputation = do
   i <- get    --stavová akce
   lift $ putStrLn "hello!"   --IO akce
   put (i+5)
   lift $ putStrLn $ "the state was: " ++ show i

--runStateT :: StateT s m  a -> s -> m  (a, s)  -- polymorfní verze
--runStateT :: StateT s IO a -> s -> IO (a, s)  -- specializovaná verze

main = do
  (result, finalState) <- runStateT stateAndIOComputation 123
  print result
  print finalState
```

Program vypíše tohle:

```
hello!
the state was: 123
()
128
```

## Odevzdávání

Program sbalíkujte tradičně cabalem do balíku `u3-Prijmeni`, a archiv s
balíkem nahrajte do odpovídajícího políčka v SISu.
