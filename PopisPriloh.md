# Popis príloh

(Tento súbor je v Markdown formáte. Pre renderovanie do HTML použite napríklad program marked.)

Cieľom tohto súboru je popísať adresárovú štruktúru prílohy k diplomovej práci. Užívateľská dokumentácia sa nachádza v súbore README.md.

Aktuálny adresár obsahuje nasledovné súbory a podadresáre:

## algorithms

Tento adresár obsahuje moduly s implementáciami prehľadávacích algoritmov pre jazyk Haggle.

### BeFS.hs

Prehľadávanie s výberom najlepšieho.

### BFS.hs

Prehľadávanie do šírky.

### InfInt.hs

Rozšírenie dátového typu Int o kladné nekonečno - pomocný modul k BFS.

## core

Tento adresár obsahuje zdrojové kódy jadra jazyka Haggle.

### Expandable.hs

Typová trieda Expandable, ktorej inštanciou musí byť uzol prehľadávacieho stromu.

### Haggle.hs

Hlavný modul, importovaný zo súborov, kde chceme používať Haggle.

### Path.hs

Pomocný modul - reprezentácia cesty v prehľadávacom strome.

## examples

Tento adresár obsahuje príklady použitia jazyka Haggle.

### BeFS2x2

Príklady hľadania riešenia 2x2 verzie Lloyd 15 s použitím prehľadávania s výberom najlepšieho.

### BeFS4x4

Príklady hľadania riešenia 4x4 verzie Lloyd 15 s použitím prehľadávania s výberom najlepšieho.

### BFS2x2

Príklady hľadania riešenia 2x2 verzie Lloyd 15 s použitím prehľadávania do šírky.

### BFS4x4

Príklady hľadania riešenia 4x4 verzie Lloyd 15 s použitím prehľadávania do širky.

### combined

Zložitejšie príklady ukazujúce kombinovanie prehľadávacích algoritmov.

## lloyd15

Vzorová reprezentácia problému vo formáte očakávanom jazykom Haggle. Používame problém Lloyd 15.

### Lloyd15.hs

Modul so samotnou reprezentáciou.

### Success2x2.hs, Success4x4.hs

Pomocné moduly s definíciami cieľového uzla pre varianty Lloyd 15 2x2 a 4x4.

## README.md

Užívateľská dokumentácia.

## runExamples.sh

Shell skript, ktorý skompiluje a spustí príklady v adresári examples. (Čiže vlastne integračné testy.)

## runTests.sh

Shell skript, ktorý skompiluje a spustí unit testy v adresári tests.

## tests

Unit testy.

### TestBeFS2x2.hs

Modul TestBeFS2x2 hľadá riešenia 2x2 verzie Lloyd 15 s použitím prehľadávania s výberom najlepšieho.

### TestBeFS4x4.hs

Modul TestBeFS4x4 hľadá riešenia 4x4 verzie Lloyd 15 s použitím prehľadávania s výberom najlepšieho.

### TestBFS2x2.hs

Modul TestBFS2x2 hľadá riešenia 2x2 verzie Lloyd 15 s použitím prehľadávania do šírky.

### TestBFS4x4.hs

Modul TestBFS4x4 hľadá riešenia 4x4 verzie Lloyd 15 s použitím prehľadávania do širky.

### TestFixtures.hs

Modul TestFixtures obsahuje pomocné funkcie pre heuristiky a testovacie dáta.

### TestLloyd15.hs

Modul TestLloyd15 testuje reprezentáciu Lloyd 15.
