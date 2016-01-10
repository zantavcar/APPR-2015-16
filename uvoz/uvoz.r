
#2. FAZA

#1.tabela - prošnje azilantov v letu 2015
uvoz.Prosnje2015 <- function() {
  Prosnje2015 <- read.csv2("podatki/Prosnje2015.csv",sep=",",
                           na.strings=":",
                           as.is=TRUE,
                           fileEncoding="UTF-8")
  
  tidy_Prosnje2015 <- Prosnje2015[,c(-3,-4,-5,-7)]
  tidy_Prosnje2015[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                                     tidy_Prosnje2015[,"GEO"],fixed=TRUE)
  tidy_Prosnje2015[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                                     tidy_Prosnje2015[,"GEO"],fixed=TRUE)
  tidy_Prosnje2015[,"Value"] <-as.numeric(gsub(" ","",tidy_Prosnje2015[,"Value"],fixed=TRUE))
  return(tidy_Prosnje2015)
}

tidy_Prosnje2015<-uvoz.Prosnje2015()
message("Uvažam podatke prosilcev za azil v letu 2015...\n")

#2. tabela - uvoz rezultatov prošenj za azil v letu 2015. 
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
  tidy_Odlocitve2015[,"Value"] <-as.numeric(gsub(" ","",tidy_Odlocitve2015[,"Value"],fixed=TRUE))
  return(tidy_Odlocitve2015)
}

tidy_Odlocitve2015 <- uvoz.Odlocitve2015()
message("Uvažam podatke o odločitvah...\n")

#3. tabela - poglobljena tabela prosilcev za azil (SPOL)

uvoz.Spol <- function() {
  Spol <- read.csv2("podatki/Spol.csv",
                    na.strings=":",
                    fileEncoding = "UTF-8",
                    sep = ",")
  #LOGIČNI VEKTORJI - SPOL AZILANTOV
  
  Moski <- grepl("Male",Spol[,"SEX"])
  Zenske <- grepl("Female",Spol[,"SEX"])
  tidy_Spol <- Spol[,c(-3,-5,-6,-7)]
  
  tidy_Spol[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                            tidy_Spol[,"GEO"],fixed=TRUE)
  tidy_Spol[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                            tidy_Spol[,"GEO"],fixed=TRUE)
  tidy_Spol[,"Value"] <-as.numeric(gsub(" ","",tidy_Spol[,"Value"],fixed=TRUE))
  return(tidy_Spol)
}
tidy_Spol <- uvoz.Spol()
message("Uvažam podatke o spolu azilantov...\n")

#4. tabela - poglobljena analiza prosilcev za azil (STAROST)
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
  tidy_Starost[,"Value"] <-as.numeric(gsub(" ","",tidy_Starost[,"Value"],fixed=TRUE))
  
  #Logicni vektorji (STAROST)
  pod18 <- grepl("^Less",tidy_Starost[,"AGE"])
  odrasli_mlajsi <-grepl("^From 18",tidy_Starost[,"AGE"])
  odrasli_starejsi <- grepl("^From 35",tidy_Starost[,"AGE"])
  nad65 <- grepl("^65",tidy_Starost[,"AGE"])
  return(tidy_Starost)
}
tidy_Starost <- uvoz.Starost()
message("Uvažam podatke starosti azilantov...\n")

#5. tabela - pogljobljena analiza prosilcev za azil (ORIGIN) 
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
  tidy_Origin[,"Value"] <-as.numeric(gsub(" ","",tidy_Origin[,"Value"],fixed=TRUE))
  rownames(tidy_Origin) <- NULL
  #logični vektorji za drûave
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
message("Uvažam podatke o državljanstvu azilantov...\n")

#ZA OBJAVO
objava_Prosnje <- head(tidy_Prosnje2015)
objava_Starost <- head(tidy_Starost)
objava_Odlovitve <- tidy_Odlocitve2015[c(1:5),]
#UVOZ TABELE O PREBIVALSTVU V DRŽAVAH 
require("rvest")
url <- "https://en.wikipedia.org/wiki/Area_and_population_of_European_countries"
population <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[3]/div[3]/div[4]/table[2]') %>%
  html_table()
prebivalstvo.EU <- do.call(rbind.data.frame, population)
prebivalstvo.EU <- prebivalstvo.EU[,c(-2,-3)]
colnames(prebivalstvo.EU) <- c("drzava","prebivalstvo")
prebivalstvo.EU[,"prebivalstvo"] <- as.numeric(gsub(',',"",prebivalstvo.EU[,"prebivalstvo"],fixed = FALSE))

#NOVA TABELA ZA PREDSTAVITEV - VIZUALIZACIJA

tidy_Prosnje2015 <- tidy_Prosnje2015 %>%
  mutate(MONTH = TIME %>% strapplyc("M([0-9]+)") %>% as.numeric())

Q1 <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant", MONTH <= 3) %>%
  group_by(GEO) %>% summarise(applicants = sum(Value))

Q2 <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant", (MONTH <=6 & MONTH >3)) %>%
  group_by(GEO) %>% summarise(applicants = sum(Value))

Q3 <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant", (MONTH >6 & MONTH <= 9)) %>%
  group_by(GEO) %>% summarise(applicants = sum(Value))

tabela_Prosnje <- inner_join(Q1,Q2,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,Q3,by="GEO")
colnames(tabela_Prosnje) <- c("GEO","Q1","Q2","Q3")
sprememba1 <- round((tabela_Prosnje[,"Q2"]-tabela_Prosnje[,"Q1"])/tabela_Prosnje[,"Q1"],digits=4)*100
sprememba2 <- round ((tabela_Prosnje[,"Q3"]-tabela_Prosnje[,"Q2"])/tabela_Prosnje[,"Q2"],digits=4)*100
sprememba1 <- paste0(sprememba1[,1]," %")
sprememba2 <- paste0(sprememba2[,1]," %")
tabela_Prosnje$"Q1.Q2(%)" <- sprememba1
tabela_Prosnje$"Q2.Q3(%)" <- sprememba2

#TABELA ZA OBJAVO

objava_tabela_Prosnje <- tabela_Prosnje %>% filter(GEO %in% c("European Union","Germany","Hungary",
                                                              "United Kingdom","Austria", "Sweden", 
                                                              "Italy", "France","Slovenia"))
#PRIPRAVA TABELE ZA UVOZ
total <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant",(MONTH >= 1 & MONTH <=9)) %>%
  group_by(GEO) %>% summarise(applicants = sum(Value))

povprecja <- inner_join(total, prebivalstvo.EU,
                        by = c("GEO" = "drzava")) %>%
  mutate(prosnje.na.milijon = round((1000000*applicants)/prebivalstvo))
#GRAFI
require(dplyr)
require(gsubfn)
require(ggplot2)

graf_prosnje<-ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",
                                         GEO %in% c("Germany","Hungary","United Kingdom",
                                                    "Austria", "Sweden", "Italy", "France")),
             aes(x = TIME, y = Value, group = GEO, color = GEO)) + geom_line()
 
    

