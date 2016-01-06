
#2. FAZA

#1.tabela - proönje azilantov v letu 2015

uvoz.Prosnje2015 <- function() {
  Prosnje2015 <- read.csv2("podatki/Prosnje2015.csv",sep=",",
                           na.strings=":",
                           as.is=TRUE,
                           fileEncoding="UTF-8")
  
  #Precistil bom nepotreben stolpec s podatki iz leta 2014
  tidy_Prosnje2015 <- Prosnje2015[,c(-3,-4,-5,-7)]
  return(tidy_Prosnje2015)
}

tidy_Prosnje2015<-uvoz.Prosnje2015()
message("Uvažam podatke prosilcev za azil v letu 2015...\n")

#2. tabela - uvoz rezultatov proöenj za azil v letu 2015. Opis, kaj doloËene
#odloËitve pomenijo bo v zakljuËnem poroËilu. Namen te tabele je, primerjanje
#odloËitev skozi Ëas
uvoz.Odlocitve2015 <- function() {
  Odlocitve2015 <- read.csv2("podatki/Odlocitve2015.csv",
                             na.strings=":",
                             fileEncoding = "UTF-8",
                             sep = ",")
  tidy_Odlocitve2015 <- Odlocitve2015[c(-3,-4,-5,-7)]
  tidy_Odlocitve2015[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                                     tidy_Odlocitve2015[,"GEO"],fixed=TRUE)
  tidy_Odlocitve2015[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                                     tidy_Odlocitve2015[,"GEO"],fixed=TRUE)
  EU <- grepl("^Euro",Odlocitve2015[,"GEO"])
  GER <- grepl("^Ger",Odlocitve2015[,"GEO"])
  objavaEU_odlocitve2015 <- tidy_Odlocitve2015[EU,]
  objavaGER_odlocitve2015 <- tidy_Odlocitve2015[GER,]
  cetrtletja <- c("2015Q1","2015Q2","2015Q3")
  #LogiËni vektorji za odloËitve o azilantih
  Positive <- grepl("Total positive decisions",Odlocitve2015[,"DECISION"])
  Geneva <- grepl("^Geneva",Odlocitve2015[,"DECISION"])
  Humanitarian <- grepl("^Human",Odlocitve2015[,"DECISION"])
  Negative <- grepl("^Reje",Odlocitve2015[,"DECISION"])
  Total <- grepl("^Total$",Odlocitve2015[,"DECISION"])
  #nova tabela -> odlocitve po cetrtletjih v EU ter v NemËiji
  positive <- objavaEU_odlocitve2015[Positive,"Value"]
  geneva <- objavaEU_odlocitve2015[Geneva,"Value"]
  humanitarian <- objavaEU_odlocitve2015[Humanitarian,"Value"]
  negative <- objavaEU_odlocitve2015[Negative,"Value"]
  total <- objavaEU_odlocitve2015[Total,"Value"]
  return(tidy_Odlocitve2015)
}

tidy_Odlocitve2015 <- uvoz.Odlocitve2015()
message("Uvažam podatke o odločitvah...\n")

#4. tabela - poglobljena tabela prosilcev za azil (SPOL)

uvoz.Spol <- function() {
  Spol <- read.csv2("podatki/Spol.csv",
                    na.strings=":",
                    fileEncoding = "UTF-8",
                    sep = ",")
  
  prihodV.EU <- grepl("^Euro",Spol[,"GEO"])
  #LOGI»NI VEKTORJI - SPOL AZILANTOV
  
  Moski <- grepl("Male",Spol[,"SEX"])
  Zenske <- grepl("Female",Spol[,"SEX"])
  tidy_Spol <- Spol[,c(-3,-5,-6,-7)]
  tidy_Spol[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                            tidy_Spol[,"GEO"],fixed=TRUE)
  tidy_Spol[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                            tidy_Spol[,"GEO"],fixed=TRUE)
  return(tidy_Spol)
}
tidy_Spol <- uvoz.Spol()
message("Uvažam podatke o spolu azilantov...\n")

#5. tabela - poglobljena analiza prosilcev za azil (STAROST)
uvoz.Starost <- function () {
  Starost <- read.csv2("podatki/Starost.csv",
                       na.strings=":",
                       fileEncoding = "UTF-8",
                       sep = ",")
  prihodV.EU <- grepl("^Euro",Starost[,"GEO"])
  tidy_Starost <- Starost[,c(-3,-4,-6,-7)]
  tidy_Starost[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                               tidy_Starost[,"GEO"],fixed=TRUE)
  tidy_Starost[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                               tidy_Starost[,"GEO"],fixed=TRUE)
  tidy_Starost <- tidy_Starost[c(1:1485),]
  #Logicni vektorji (STAROST)
  pod18 <- grepl("^Less",tidy_Starost[,"AGE"])
  odrasli_mlajsi <-grepl("^From 18",tidy_Starost[,"AGE"])
  odrasli_starejsi <- grepl("^From 35",tidy_Starost[,"AGE"])
  nad65 <- grepl("^65",tidy_Starost[,"AGE"])
  return(tidy_Starost)
}
tidy_Starost <- uvoz.Starost()
message("Uvažam podatke starosti azilantov...\n")

#6. tabela - pogljobljena analiza prosilcev za azil (ORIGIN) 
uvoz.Origin <- function () {
  Origin <- read.csv2("podatki/Origin.csv",
                      na.strings = ":",
                      fileEncoding="UTF-8",
                      sep=",",
                      nrows=2613)
  tidy_Origin <- Origin[c(-4,-5,-6,-7,-8)]
  rownames(Origin) <- NULL
  tidy_Origin[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                              tidy_Origin[,"GEO"],fixed=TRUE)
  tidy_Origin[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                              tidy_Origin[,"GEO"],fixed=TRUE)
  tidy_Origin[,"CITIZEN"] <-gsub("Kosovo (under United Nations Security Council Resolution 1244/99)",
                                 "Kosovo",tidy_Origin[,"CITIZEN"],fixed=TRUE)
  tidy_Origin <- tidy_Origin[c(1:2142),]
  rownames(tidy_Origin) <- NULL
  #logiËni vektorji za drûave
  prihod.Albanija <- grepl("^Alb",tidy_Origin[,"CITIZEN"])
  prihod.Kosovo <- grepl("^Kos",tidy_Origin[,"CITIZEN"])
  prihod.Srbija <- grepl("^Ser",tidy_Origin[,"CITIZEN"])
  prihod.Afganistan <- grepl("^Afg",tidy_Origin[,"CITIZEN"])
  prihod.Sirija <- grepl("^Syr",tidy_Origin[,"CITIZEN"])
  prihod.Eritreja <- grepl("^Eri",tidy_Origin[,"CITIZEN"])
  prihod.Irak <- grepl("^Ira",tidy_Origin[,"CITIZEN"])
  
  prihodV.EU <- grepl("^Euro",Origin[,"GEO"])
  tidy_OriginEU <- Origin[prihodV.EU,]
  #nova tabela
  meseci <- factor(paste0("2015M",1:11),levels=paste0("2015M",1:9),ordered = TRUE)
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
  return(tidy_Origin)
}
tidy_Origin <- uvoz.Origin()
message("Uvažam podatke o drûavljanstvu azilantov...\n")




