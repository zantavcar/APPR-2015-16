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

#LOGIČNI VEKTORJI, KI PREPOZNAJO ZELJENO CETRTLETJE

#Funkcija, ki uvozi podatke iz mape podatki
#1.tabela - prošnje azilantov v letu 2015
uvozi.Prosnje2015 <- function() {
Prosnje2015 <- read.table("prosnje_azil_2015.csv",sep=";",
               skip=8,nrows=32,
               na.strings=":",
               as.is=TRUE,
               row.names= NULL, fileEncoding="UTF-8")

#Precistil bom nepotreben stolpec s podatki iz leta 2014
Prosnje2015<-Prosnje2015[-2]
#Poimenoval bom stolpce
colnames(Prosnje2015) <- c("Države",paste0("M",1:11))
#Ustvaril bom logični vektor, ki prepozna, za katero
#četrtletje gre
prvo.cetrtletje <- c(2:4)
drugo.cetrtletje <- c(5:8)
tretje.cettletje <- c(9:12)
#Dodam stolpce, ki za vsako državo posebej preštejejo
#prošnje za azil za vsako četrtletje posebej
Prosnje2015$prvo_cetrtletje <- apply(Prosnje2015[prvo.cetrtletje],1,
                                     function (x) sum(x,na.rm=TRUE))


return(Prosnje2015)
}
cat("Uvažam podatke...\n")

