
#######################################################################

#2. FAZA

#Funkcija, ki uvozi podatke iz mape podatki
#1.tabela - prošnje azilantov v letu 2015
uvoz.Prosnje2015 <- function() {
Prosnje2015 <- read.table("prosnje_azil_2015.csv",sep=";",
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
#izračunal bom prirast prošenj za azil v tretjem četrtletju 2015,
#če za bazo vzamem drugo četrtletje
prirast <- round(((Q3/Q2)-1)*100,2)
#naredil bom faktor
Prosnje2015$SpremembaQ2.Q3 <- paste0(prirast, " %")

#Delež prošenj za posamezno EU državo v Q3
delez <- round((Q3/sum(Q3)),3)*100
urejene.velikosti <- c("manj kot 1 %","1-5 %",
                       "5-10 %","več kot 10 %")
kategorija <- rep("",length(delez))
kategorija[delez<1] <- "manj kot 1 %"
kategorija[delez>1 & delez<5] <- "1-5 %"
kategorija[delez>5 & delez<10] <- "5-10 %"
kategorija[delez>10] <- "več kot 10 %"
kategorija <- factor(kategorija, levels=urejene.velikosti,
                     ordered=TRUE)
Prosnje2015$Delez<-kategorija
return(Prosnje2015)
}
cat("Uvažam podatke prosilcev za azil v letu 2015...\n")
Prosnje2015<-uvozi.Prosnje2015()


#2. tabela - migracijski tokovi v 
#preteklih dveh desetletjih z namenom primerjave današnje migracije,
#in tiste na Balkanu po letu 1990
uvozi.Tokovi <- function() {
  Tokovi <- read.table("migrantski_tokovi.csv",sep=",",
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


  

#4. tabela - uvoz rezultatov prošenj za azil v letu 2014. Opis, kaj določene
#odločitve pomenijo bo v zaključnem poročilu. Namen te tabele je, da bom kasneje
#vstavil isto tabelo iz leta 2015 ter primerjal rezultate.
uvoz.Odlocitve2015 <- function() {
  Odlocitve2015 <- read.csv2("Odlocitve2015.csv",
                             na.strings=":",
                             fileEncoding = "UTF-8",
                             sep = ",")
  Odlocitve2015 <- Odlocitve2015[c(-3,-4,-5,-7)]
  EU <- grepl("^Euro",Odlocitve2015[,"GEO"])
  Odlocitve2015 <- Odlocitve2015[EU,]
  cetrtletja <- c("2015Q1","2015Q2","2015Q3")
  #Logični vektorji za odločitve o azilantih
  Positive <- grepl("Total positive decisions",Odlocitve2015[,"DECISION"])
  Geneva <- grepl("^Geneva",Odlocitve2015[,"DECISION"])
  Humanitarian <- grepl("^Human",Odlocitve2015[,"DECISION"])
  Negative <- grepl("^Reje",Odlocitve2015[,"DECISION"])
  Total <- grepl("Total",Odlocitve2015[,"DECISION"])
  #nova tabela -> odlocitve po cetrtletjih v EU
  positive <- Odlocitve2015[Positive,]["Value"]
  geneva <- Odlocitve2015[Geneva,]["Value"]
  humanitarian <- Odlocitve2015[Humanitarian,]["Value"]
  negative <- Odlocitve2015[Negative,]["Value"]
  total <- Odlocitve2015[Total,]["Value"]
  
  Odlocitve_EU <- data.frame(cetrtletja,positive,geneva,humanitarian,negative,total)
  colnames(Odlocitve_EU) <- c("Q","Pozitivne","Geneva status","Humanitarian",
                              "Negativne","Skupaj")
  Odlocitve_EU <- Odlocitve_EU[c(4:6),]
  
  Odlocitve2015 <- uvozi.Odlocitve2015()
  cat("Uvažam podatke o odločitvah...\n")
}
#5. tabela - poglobljena tabela prosilcev za azil (SPOL)

uvoz.Spol <- function() {
  Spol <- read.csv2("Spol.csv",
                    na.strings=":",
                    fileEncoding = "UTF-8",
                    sep = ",")
  #LOGIČNI VEKTORJI - SPOL AZILANTOV
  
  Moski <- grepl("Male",Spol[,"SEX"])
  Zenske <- grepl("Female",Spol[,"SEX"])
  Spol <- Spol[c(-5,-6,-7)]
  
  Spol <- uvoz.Spol()
  cat("Uvažam podatke o spolu azilantov...\n")

}
#6. tabela - poglobljena analiza prosilcev za azil (STAROST)
uvoz.Starost <- function () {
  Starost <- read.csv2("Starost.csv",
                       na.strings=":",
                       fileEncoding = "UTF-8",
                       sep = ",",
                       nrows = 9325)
  Starost <- Starost[c(-4,-6,-7)]
  rownames(Starost)<- NULL
  
  #Ureditev tabele - starejši na vrh
  Starost <- uvoz.Starost()
  cat("Uvažam podatke starosti azilantov...\n")
}
 #7. tabela - pogljobljena analiza prosilcev za azil (ORIGIN) 
 uvoz.Origin <- function () {
   Origin <- read.csv2("Origin.csv",
                       na.strings = ":",
                       fileEncoding="UTF-8",
                       sep=",",
                       nrows=2613)
   Origin <- Origin[c(-4,-5,-6,-7,-8)]
   rownames(Origin) <- NULL
#za pet držav bom napravil tabelo prihodov migrantov v letošnjem letu po četrtletjih
   države<-c("Albanija","Srbija","Kosovo",
             "Eritreja","Afganistan","Irak",
             "Sirija")
   #logične vektorje za četrtletja že imamo, naredimo jih še za države
   prihod.Albanija <- grepl("^Alb",Origin[,"CITIZEN"])
   prihod.Kosovo <- grepl("^Kos",Origin[,"CITIZEN"])
   prihod.Srbija <- grepl("^Ser",Origin[,"CITIZEN"])
   prihod.Afganistan <- grepl("^Afg",Origin[,"CITIZEN"])
   prihod.Sirija <- grepl("^Syr",Origin[,"CITIZEN"])
   prihod.Eritreja <- grepl("^Eri",Origin[,"CITIZEN"])
   prihod.Irak <- grepl("^Ira",Origin[,"CITIZEN"])
   
   prihodV.EU <- grepl("^Euro",Origin[,"GEO"])
   Origin <- Origin[prihodV.EU,]
   rownames(Origin)<-NULL
   #nova tabela
   meseci <- paste0("2015M",1:11)
   albanija <- Origin[prihod.Albanija,]["Value"]
   albanija <- albanija[!is.na(albanija)]
   kosovo <- Origin[prihod.Kosovo,]["Value"]
   kosovo <- kosovo[!is.na(kosovo)]
   srbija <- Origin[prihod.Srbija,]["Value"]
   srbija <-srbija[!is.na(srbija)]
   sirija <- Origin[prihod.Sirija,]["Value"]
   sirija <- sirija[!is.na(sirija)]
   afganistan <- Origin[prihod.Afganistan,]["Value"]
   afganistan <- afganistan[!is.na(afganistan)]
   irak <- Origin[prihod.Irak,]["Value"]
   irak <- irak[!is.na(irak)]
   eritreja <- Origin[prihod.Eritreja,]["Value"]
   eritreja <- eritreja[!is.na(eritreja)]
   Origin <- data.frame(meseci,sirija,irak,afganistan,eritreja,
                        kosovo,albanija,srbija)
   Origin <- uvoz.Origin()
   cat("Uvažam podatke o državljanstvu azilantov...\n")
                            }



