# Analiza podatkov s programom R, 2015/16

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2015/16.

ANALIZA PROSILCEV ZA AZIL V DRŽAVAH EU

V zadnjih mesecih je v središču dogajanja begunska kriza. Zaradi vojne v Siriji je svoje domove zapustilo več milijonov ljudi, veliko se jih je po želji za boljšim življenjem odpravilo v Evropo, destinacija večine pa je "obljubjena" Nemčija.

Odločil sem se, da bom analiziral prosilce za azil v najbolj obleganih evropskih državah - Nemčiji, Španiji, Švedski, Franciji, Veliki Britaniji, Nizozemski, Švici, Italiji, Belgiji ter Madžarski. Zanima me predvsem poreklo migrantov, torej iz kje prihajajo, kje so zaprosili za azil in koliko odstotkov prošenj je bilo v zgoraj navedenih evropskih državah odobrenih.

Analiza podatkov bo pogljobljena do te mere, da bom denimo ugotovil koliko Sirijcev je za azil zaprosilo v Nemčiji, koliko je bilo odobrenih prošenj ali zavrjenih, ter odstotek uspeha prošnje. Kot zgoraj zapisano bom naredil za vse države, ki sem jih omenil v zgornjem razdelku.

Podatke bom črpal iz Eurostata, kjer je v tem trenutku zabeleženo število prosilcev za azil v posamezni evropski državi do meseca avgusta tega leta. Podatki so v XLS, CSV ter HTML obliki.


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Spletni vmesnik

Spletni vmesnik se nahaja v datotekah v mapi `shiny/`. Poženemo ga tako, da v
RStudiu odpremo datoteko `server.R` ali `ui.R` ter kliknemo na gumb *Run App*.
Alternativno ga lahko poženemo tudi tako, da poženemo program `shiny.r`.

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `httr` - za pobiranje spletnih strani
* `XML` - za branje spletnih strani
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
