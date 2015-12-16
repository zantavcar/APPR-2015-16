# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function() {
  return(read.table("podatki/druzine.csv", sep = ";", as.is = TRUE,
                      row.names = 1,
                      col.names = c("obcina", "en", "dva", "tri", "stiri"),
                      fileEncoding = "Windows-1250"))
}

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine()

obcine <- uvozi.obcine()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

#######################################################################

#2. FAZA

#Funkcija, ki uvozi podatke iz mape podatki
#1.tabela - prošnje azilantov v letu 2015
uvozi.Prosnje2015 <- function() {
Prosnje2015 <- read.table("podatki/prosnje_azil_2015.csv",sep=";",
               skip=8,nrows=32,
               na.strings=":",
               as.is=TRUE,
               row.names= NULL,
               fileEncoding="UTF-8")

#Precistil bom nepotreben stolpec s podatki iz leta 2014
Prosnje2015<-Prosnje2015[-2]
#Poimenoval bom stolpce in vrstice
colnames(Prosnje2015) <- c("Države",paste0("M",1:11))
rownames(Prosnje2015) <- Prosnje2015[,1]
Prosnje2015 <- Prosnje2015[-1]

#Logični vektorji za vsa tri četrtletja - regularni izrazi

prvo.cetrtletje <- grepl("^M[1-3]",colnames(Prosnje2015))
drugo.cetrtletje <- grepl("^M[4-6]",colnames(Prosnje2015))
tretje.cetrtletje <- grepl("^M[7-9]",colnames(Prosnje2015))

#Na konec bom dodal tri stolpce, in sicer za vsako  četrtletje
#število prošenj za azil za vsako državo, to bo pomembno v 
#nadaljevanju, ko bom primerjal z istimi obdobji lani
Q1 <- apply(Prosnje2015[,prvo.cetrtletje],1, 
            function (x) sum(x,na.rm=TRUE))
Q2 <- apply(Prosnje2015[,drugo.cetrtletje],1, 
                  function (x) sum(x,na.rm=TRUE))
Q3 <- apply(Prosnje2015[,tretje.cetrtletje],1, 
                  function (x) sum(x,na.rm=TRUE))
Prosnje2015$Q1 <- Q1
Prosnje2015$Q2 <- Q2
Prosnje2015$Q3 <- Q3


return(Prosnje2015)
}
cat("Uvažam podatke prosilcev za azil v letu 2015...\n")
Prosnje2015<-uvozi.Prosnje2015()


#2. tabela - migracijski tokovi v 
#preteklih dveh desetletjih z namenom primerjave današnje migracije,
#in tiste na Balkanu po letu 1990
uvozi.Tokovi <- function() {
  Tokovi <- read.table("podatki/migrantski_tokovi.csv",sep=",",
                       na.strings="..",
                       as.is=TRUE,
                       skip=1,
                       nrows = 9,
                       row.names=NULL,
                       fileEncoding = "UTF-8")
#Poimenovanje vrstic in stolpcev, dodal sem dva stolpca
#koliko migracij je bilo iz 10 izbranih držav v zadnjih 25
#letih
Tokovi<-Tokovi[c(-1,-2,-4)]
colnames(Tokovi) = c("Države",1991:2015)
rownames(Tokovi)<-Tokovi[1:9,1]
Tokovi<-Tokovi[c(-1)]
migracije_skupaj <- apply(Tokovi[,2:26],1,function (x) sum(x,na.rm=TRUE))
Tokovi$migracije_skupaj<-migracije_skupaj
migracije_povprecje<-apply(Tokovi[,2:26],1,function (x) mean(x,na.rm=TRUE))
Tokovi$migracije_povprecje<-migracije_povprecje
return(Tokovi)

}

cat("Uvažam podatke migracijskih tokov...\n")
Tokovi<-uvozi.Tokovi()

#3. tabela - Število prebivalcev držav EU. To tabelo uvažam z
#namenom kasnejše analize, ko bom izrazil število prošenj za azil
#na milijon prebivalcev države

uvozi.Prebivalci <- function () {
  return(read.csv2("podatki/Prebivalstvo_EU.csv",
                   fileEncoding = "UTF-8",
                   skip=7,
                   nrows=50,
                   na.strings=":"
                   ))
 } 

cat("Uvažam podatke števila prebivalcev Evrope...\n")
Prebivalci<-uvozi.Prebivalci()
  

#4. tabela - uvoz rezultatov prošenj za azil v letu 2014. Opis, kaj določene
#odločitve pomenijo bo v zaključnem poročilu. Namen te tabele je, da bom kasneje
#vstavil isto tabelo iz leta 2015 ter primerjal rezultate.

library(rvest)
library(dplyr)
library(XML)
library(RCurl)
naslov <- "
http://en.wikipedia.org/wiki/Brazil_national_football_team
"

naslovdata <- getURL(naslov)
data<-readHTMLTable(naslovdata,stringsAsFactors=FALSE)

#5. tabela - poglobljena analiza prosilcev za azil 
#county of origin, starost in spol

#DOKONČAJ, EUROSTAT TRENUTNO NE DELUJE
