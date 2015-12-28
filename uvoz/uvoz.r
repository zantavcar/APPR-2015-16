
#2. FAZA

#1.tabela - prošnje azilantov v letu 2015

uvoz.Prosnje2015 <- function() {
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
  
  #Na konec bom dodal tri stolpce, in sicer za vsako četrtletje
  #število prošenj za azil za vsako državo, to bo pomembno v 
  #nadaljevanju, ko bom primerjal relativno spremembo glede na 
  #prejšnje četrtletje
  
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
  
  prirastQ2_Q3 <- round(((Q3/Q2)-1)*100,2)
  prirastQ1_Q2 <- round(((Q2/Q1)-1)*100,2)
  
  Prosnje2015$ChangeQ1.Q2 <- paste0(prirastQ1_Q2," %")
  Prosnje2015$ChangeQ2.Q3 <- paste0(prirastQ2_Q3, " %")
  
  
  #Delež prošenj za posamezno EU državo v Q3
  
  delez <- round((Q3/sum(Q3)),3)*100
  urejene.velikosti <- c("<1 %","1-5 %",
                         "5-10 %",">10 %")
  kategorija <- rep("",length(delez))
  kategorija[delez<1] <- "<1 %"
  kategorija[delez>1 & delez<5] <- "1-5 %"
  kategorija[delez>5 & delez<10] <- "5-10 %"
  kategorija[delez>10] <- ">10 %"
  kategorija <- factor(kategorija, levels=urejene.velikosti,
                       ordered=TRUE)
  Prosnje2015$Delez<-kategorija
  
  drzave <-rownames(Prosnje2015)
  library("rvest")
  url <- "https://en.wikipedia.org/wiki/Area_and_population_of_European_countries"
  population <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[3]/div[3]/div[4]/table[2]') %>%
    html_table()
  Prebivalstvo <- do.call(rbind.data.frame, population)
  rownames(Prebivalstvo)<-Prebivalstvo[,1]
  Prebivalstvo <-Prebivalstvo[drzave,]
  Prebivalstvo[,"Population"] <- gsub(",","",Prebivalstvo[,"Population"])
  st.prebivalcev <- as.numeric(Prebivalstvo[,"Population"])
  cetrtletja <- c("Q1","Q2","Q3")
  st.azilantov <- as.numeric(apply(Prosnje2015[,cetrtletja],
                                   1, function (x) sum(x,na.rm=TRUE)))
  azilantov.na.milijon <- round((1000000*st.azilantov)/st.prebivalcev)
  Prosnje2015$ApM <- azilantov.na.milijon
  ObjavaProsnje2015 <- head(Prosnje2015[,seq(-1,-11,-1)])
  return(ObjavaProsnje2015)
}

ObjavaProsnje2015<-uvoz.Prosnje2015()
message("Uvažam podatke prosilcev za azil v letu 2015...\n")
#2. tabela - uvoz rezultatov prošenj za azil v letu 2015. Opis, kaj določene
#odločitve pomenijo bo v zaključnem poročilu. Namen te tabele je, primerjanje
#odločitev skozi čas
uvoz.Odlocitve2015 <- function() {
  Odlocitve2015 <- read.csv2("podatki/Odlocitve2015.csv",
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
  Total <- grepl("^Total$",Odlocitve2015[,"DECISION"])
  #nova tabela -> odlocitve po cetrtletjih v EU
  positive <- Odlocitve2015[Positive,"Value"]
  geneva <- Odlocitve2015[Geneva,"Value"]
  humanitarian <- Odlocitve2015[Humanitarian,"Value"]
  negative <- Odlocitve2015[Negative,"Value"]
  total <- Odlocitve2015[Total,"Value"]
  
  Odlocitve2015<- data.frame(cetrtletja,positive,geneva,humanitarian,negative,total)
}

Odlocitve2015 <- uvoz.Odlocitve2015()
message("Uvažam podatke o odločitvah...\n")

#3. tabela - Število prebivalcev držav EU. To tabelo uvažam z
#namenom kasnejše analize, ko bom izrazil število prošenj za azil
#na milijon prebivalcev države

#4. tabela - poglobljena tabela prosilcev za azil (SPOL)

uvoz.Spol <- function() {
  Spol <- read.csv2("podatki/Spol.csv",
                    na.strings=":",
                    fileEncoding = "UTF-8",
                    sep = ",")
  
  prihodV.EU <- grepl("^Euro",Spol[,"GEO"])
  Spol <- Spol[prihodV.EU,]
  #LOGIČNI VEKTORJI - SPOL AZILANTOV
  
  Moski <- grepl("Male",Spol[,"SEX"])
  Zenske <- grepl("Female",Spol[,"SEX"])
  
  M <-Spol[Moski,"Value"]
  Ž <-Spol[Zenske,"Value"]
  meseci <- paste0("2015M",1:11)
  Spol <- data.frame(meseci,M,Ž)
}
Spol <- uvoz.Spol()
message("Uvažam podatke o spolu azilantov...\n")

#5. tabela - poglobljena analiza prosilcev za azil (STAROST)
uvoz.Starost <- function () {
  Starost <- read.csv2("podatki/Starost.csv",
                       na.strings=":",
                       fileEncoding = "UTF-8",
                       sep = ",")
  prihodV.EU <- grepl("^Euro",Starost[,"GEO"])
  Starost <- Starost[prihodV.EU,]
  
  #Logicni vektorji (STAROST)
  pod18 <- grepl("^Less",Starost[,"AGE"])
  odrasli_mlajsi <-grepl("^From 18",Starost[,"AGE"])
  odrasli_starejsi <- grepl("^From 35",Starost[,"AGE"])
  nad65 <- grepl("^65",Starost[,"AGE"])
  
  meseci <- paste0("2015M",1:11)
  Mladoletni <- Starost[pod18,"Value"]
  Od_18_do_34 <- Starost[odrasli_mlajsi,"Value"]
  Od_35_do_64 <- Starost[odrasli_starejsi,"Value"]
  Nad_65 <- Starost[nad65,"Value"]
  Starost <- data.frame(meseci,Mladoletni,Od_18_do_34,
                        Od_35_do_64,Nad_65)
}
Starost <- uvoz.Starost()
message("Uvažam podatke starosti azilantov...\n")

 #6. tabela - pogljobljena analiza prosilcev za azil (ORIGIN) 
 uvoz.Origin <- function () {
   Origin <- read.csv2("podatki/Origin.csv",
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
   meseci <- factor(paste0("2015M",1:11),levels=paste0("2015M",1:11),ordered = TRUE)
   albanija <- Origin[prihod.Albanija,]["Value"]
   albanija <- as.numeric(gsub(" ","",albanija[!is.na(albanija)]))
   kosovo <- Origin[prihod.Kosovo,]["Value"]
   kosovo <- as.numeric(gsub(" ","",kosovo[!is.na(kosovo)]))
   srbija <- Origin[prihod.Srbija,]["Value"]
   srbija <-as.numeric(gsub(" ","",srbija[!is.na(srbija)]))
   sirija <- Origin[prihod.Sirija,]["Value"]
   sirija <- as.numeric(gsub(" ","",sirija[!is.na(sirija)]))
   afganistan <- Origin[prihod.Afganistan,]["Value"]
   afganistan <- as.numeric(gsub(" ","",afganistan[!is.na(afganistan)]))
   irak <- Origin[prihod.Irak,]["Value"]
   irak <- as.numeric(gsub(" ","",irak[!is.na(irak)]))
   eritreja <- Origin[prihod.Eritreja,]["Value"]
   eritreja <- as.numeric(gsub(" ","",eritreja[!is.na(eritreja)]))
   Origin <- data.frame(meseci,sirija,irak,afganistan,eritreja,
                        kosovo,albanija,srbija)
   colnames(Origin)<-c("Meseci","Sirija","Irak","Afganistan","Eritreja","Kosovo","Albanija","Srbija")
   return(Origin)
 }
 Origin <- uvoz.Origin()
 message("Uvažam podatke o državljanstvu azilantov...\n")

#GRAFI
 library(ggplot2)


