# Haggle

(Tento súbor je v Markdown formáte. Pre renderovanie do HTML použite napríklad program marked.)

Haggle je vnorený doménovo špecifický jazyk pre všeobecné problémy prehľadávania stavových priestorov. Jeho cieľom je zjednodušiť kombinovanie rozličných prehľadávacích algoritmov a skúšanie rozličných heuristík pre daný problém.

## Kompilácia a spúšťanie

Zdrojový kód jazyka Haggle pozostáva z troch modulov, ktoré tvoria jeho jadro - Haggle, Path a Expandable - a z modulov, ktoré implementujú konkrétne prehľadávacie algoritmy. Podporované prehľadávacie algoritmy sú:

* prehľadávanie s výberom najlepšieho (s heuristikou)

* prehľadávanie do šírky (s ľubovoľnou hĺbkou vrátane nekonečnej)

Haggle potrebuje GHC 7. Novšie verzie pravdepodobne nebudú fungovať.

Najjednoduchší spôsob, ako používať Haggle, je stiahnuť si jeho zdrojové súbory a dať adresár so zdrojovými súbormi ako -i parameter prekladaču, keď kompilujete alebo spúšťate Haggle súbory.

## Pridávanie nových prehľadávacích algoritmov

Implementujte váš prehľadávací algoritmus v Haskelli ako nový modul. Exportovaná funkcia, ktorá vykonáva samotné prehľadávanie, musí mať návratový typ `State (Backlog a) [[a]]`, ak je a inštanciou typovej triedy Expandable.

Potom stačí len importovať modul z Haggle súborov a zavolať prehľadávaciu funkciu.

## Testovanie algoritmov na nových problémoch

Navrhnite dátový typ reprezentujúci problém a urobte tento typ inštanciou typovej triedy Expandable.

Táto typová trieda obsahuje nasledovné funkcie:

```
-- predikát "detekuj úspešný koncový vrchol"
stopSuccess :: a -> Bool
-- predikát "detekuj neúspešný koncový vrchol"
stopFail    :: [a] -> Bool
-- vráť ohodnotenie cesty
rank        :: (a -> Int) -> [a] -> Int
-- vráť nasledujúce stavy k zadanému stavu
generateNbs :: a -> [a]
-- odstráň zo zoznamu potenciálnych kandidátov tých,
-- ktorých sme už preskúmali
prune       :: Result a -> [a] -> Result a
```

Dátový typ Result je typ pre výsledok expanzie jedného vrcholu a vyzerá nasledovne:

```
data Result a = Fail | Success a | Sons [a]
```

Teraz môžete používať podporované prehľadávanie algoritmy a ich kombinácie na hľadanie riešenia tohto problému.

## Formát súboru pre Haggle

Súbory pre Haggle sú obyčajné zdrojáky v Haskelli. Môžete v nich používať funkcie tohto jazyka, prípadne si pridať vlastné, kdekoľvek to prekladač dovoľuje.

Súbor pre jazyk Haggle vyzerá nasledovne:

```
import Haggle
<import used search algorithms>
<import other modules>`

main = < result selector > $ do
                                <search algorithm 1> <params>
                                ...
                                <search algorithms n> <params>
```

Algoritmus pre prehľadávanie s výberom najlepšieho sa zavolá použitím:

`befs < heuristická funkcia >`

Algoritmus pre prehľadávanie do šírky sa zavolá použitím:

`bfs < limit; Inf znamená neobmedzene >`

Haggle súbor spustíte (napríklad) pomocou:

`runghc -i<path to Haggle> hagglefile.hs`

## Príklady

V adresári examples je niekoľko príkladov využitia jazyka Haggle pre hľadanie riešení Lloydovej 15 rozličnými technikami.

Skript runExamples.sh skompiluje a spustí všetky príklady.

## Testy

V adresári tests sa nachádzajú unit testy. Všetky používajú framework HUnit.

Modul TestBeFS2x2 hľadá riešenia 2x2 verzie Lloyd 15 s použitím prehľadávania s výberom najlepšieho.

Modul TestBeFS4x4 hľadá riešenia 4x4 verzie Lloyd 15 s použitím prehľadávania s výberom najlepšieho.

Modul TestBFS2x2 hľadá riešenia 2x2 verzie Lloyd 15 s použitím prehľadávania do šírky.

Modul TestBFS4x4 hľadá riešenia 4x4 verzie Lloyd 15 s použitím prehľadávania do širky.

Modul TestFixtures obsahuje pomocné funkcie pre heuristiky a testovacie dáta.

Modul TestLloyd15 testuje reprezentáciu Lloyd 15.

Skript runTests.sh skompiluje a spustí všetky testy.
