
require(dplyr)
require(gsubfn)
require(ggplot2)
require(maptools)
require(sp)
require(mgcv)
options(scipen=999)
#Uvoz CSV datotek iz Eurostat

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

#2. tabela - poglobljena tabela prosilcev za azil (SPOL)

uvoz.Spol <- function() {
  Spol <- read.csv2("podatki/Spol.csv",
                    na.strings=":",
                    fileEncoding = "UTF-8",
                    sep = ",")

  tidy_Spol <- Spol[,c(-3,-5,-7)]
  
  tidy_Spol[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                            tidy_Spol[,"GEO"],fixed=TRUE)
  tidy_Spol[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                            tidy_Spol[,"GEO"],fixed=TRUE)
  tidy_Spol[,"Value"] <-as.numeric(gsub(" ","",tidy_Spol[,"Value"],fixed=TRUE))
  return(tidy_Spol)
}
tidy_Spol <- uvoz.Spol()
message("Uvažam podatke o spolu azilantov...\n")

#3. tabela - poglobljena analiza prosilcev za azil (STAROST)
uvoz.Starost <- function () {
  Starost <- read.csv2("podatki/Starost.csv",
                       na.strings=":",
                       fileEncoding = "UTF-8",
                       sep = ",")
  tidy_Starost <- Starost[,c(-3,-4,-7)]
  tidy_Starost[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                               tidy_Starost[,"GEO"],fixed=TRUE)
  tidy_Starost[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                               tidy_Starost[,"GEO"],fixed=TRUE)
  tidy_Starost[,"Value"] <-as.numeric(gsub(" ","",tidy_Starost[,"Value"],fixed=TRUE))

  return(tidy_Starost)
}
tidy_Starost <- uvoz.Starost()
message("Uvažam podatke starosti azilantov...\n")

#4. tabela - pogljobljena analiza prosilcev za azil (ORIGIN) 
uvoz.Origin <- function () {
  Origin <- read.csv2("podatki/Origin.csv",
                      na.strings = ":",
                      fileEncoding="UTF-8",
                      sep=",")
  tidy_Origin <- Origin[c(-4,-5,-7)]
  rownames(Origin) <- NULL
  tidy_Origin[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                              tidy_Origin[,"GEO"],fixed=TRUE)
  tidy_Origin[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                              tidy_Origin[,"GEO"],fixed=TRUE)
  tidy_Origin[,"CITIZEN"] <- gsub("European Union (28 countries)","European Union",
                              tidy_Origin[,"CITIZEN"],fixed=TRUE)
  tidy_Origin[,"Value"] <-as.numeric(gsub(" ","",tidy_Origin[,"Value"],fixed=TRUE))

  return(tidy_Origin)
}
tidy_Origin <- uvoz.Origin()
message("Uvažam podatke o državljanstvu azilantov...\n")

#5. tabela - število prosilcev za azil v letu 2014 (za izračun prirasta v letu 2015)

uvoz.Prosnje2014 <- function() {
  Prosnje2014 <- read.csv2("podatki/Prosnje2014.csv",sep=",",
                           na.strings=":",
                           as.is=TRUE,
                           fileEncoding="UTF-8")
  
  tidy_Prosnje2014 <- Prosnje2014[,c(-4,-5,-7)]
  tidy_Prosnje2014[,"GEO"] <- gsub("Germany (until 1990 former territory of the FRG)","Germany",
                                   tidy_Prosnje2014[,"GEO"],fixed=TRUE)
  tidy_Prosnje2014[,"GEO"] <- gsub("European Union (28 countries)","European Union",
                                   tidy_Prosnje2014[,"GEO"],fixed=TRUE)
  tidy_Prosnje2014[,"Value"] <-as.numeric(gsub(" ","",tidy_Prosnje2014[,"Value"],fixed=TRUE))
  return(tidy_Prosnje2014)
}

tidy_Prosnje2014<-uvoz.Prosnje2014()
message("Uvažam podatke prosilcev za azil v letu 2014...\n")
#Uvoz tabele o prebivalstvu v evropskih državah za zemljevid (HTML vir)

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


#ZA OBJAVO - primer v Tidy Data obliki
objava_Prosnje <- tidy_Prosnje2015 %>% filter(GEO %in% c("European Union","Germany",
                                                     "Hungary","Sweden","Slovenia") &
                                            TIME %in% c("2015M01"))


#Pregledna tabela za poročilo
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
Q4 <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant", (MONTH > 9)) %>%
  group_by(GEO) %>% summarise(applicants = sum(Value))

Total <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant") %>%
  group_by(GEO) %>% summarise(applicants = sum(Value,na.rm= TRUE))
total2015 <- as.numeric(unlist(Total[,"applicants"]))


#Formiranje stolpca števila prošenj za azil v 2014 in izračun prirasta
tidy_Prosnje2014 <- tidy_Prosnje2014 %>% filter(CITIZEN=="Total" & ASYL_APP=="Asylum applicant") %>%
                    arrange(GEO)
total2014 <- as.numeric(unlist(tidy_Prosnje2014[,"Value"]))
tidy_Prosnje2014 <- tidy_Prosnje2014[c(-1,-3,-4)]
tidy_Prosnje2014 <- tidy_Prosnje2014 %>% mutate(applicants=round(((total2015-total2014)/total2014)*100,2))
tidy_Prosnje2014 <- tidy_Prosnje2014[c(-2)]

tabela_Prosnje <- inner_join(Q1,Q2,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,Q3,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,Q4,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,Total,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,tidy_Prosnje2014,by="GEO")


colnames(tabela_Prosnje) <- c("GEO","2015Q1","2015Q2","2015Q3","2015Q4","Total","Change on previous year [%]")
#Tabela za poročilo 

objava_tabela_Prosnje <- tabela_Prosnje %>% filter(GEO %in% c("European Union","Germany","Hungary",
                                                              "United Kingdom","Austria", "Sweden", 
                                                              "Italy", "France","Slovenia"))

total <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant") %>%
  group_by(GEO) %>% summarise(applicants = sum(Value,na.rm=TRUE))

objava_tabela_Prosnje <- inner_join(total, prebivalstvo.EU,
                        by = c("GEO" = "drzava")) %>%
  mutate(prosnje.na.milijon = round((1000000*applicants)/prebivalstvo))
#GRAFI - dinamika prošenj

graf_prosnje<-ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",
                                         GEO %in% c("Germany","Hungary","United Kingdom",
                                                    "Austria", "Sweden", "Italy", "France")),
             aes(x = TIME, y = Value, group = GEO, color = GEO))+geom_line()+xlab("")+ylab("")+
             theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

countries <- as.vector(tabela_Prosnje$GEO)

#ZEMLJEVID
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")


pretvori.zemljevid <- function(zemljevid, pogoj = TRUE) {
  fo <- fortify(zemljevid[pogoj,])
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                        "ne_110m_admin_0_countries")
m <- match(svet$name_long, total$GEO)
svet$applicants <- total$applicants[m] 
eu <- pretvori.zemljevid(svet, svet$continent == "Europe")
map1 <- ggplot() +
  geom_polygon(data = eu,
               aes(x = long, y = lat, group = group, fill = applicants)) +
  xlim(-10, 50) + ylim(34, 72) + ggtitle("Number of applicants per million inhabitants") + xlab("")+ylab("")

#ZA POTREBE SHINY APLIKACIJE

#STOLPIČNI - Spol
spol <- tidy_Spol %>%
  filter(ASYL_APP == "Asylum applicant") %>%
  group_by(GEO,SEX) %>% summarise(applicants = sum(Value,na.rm = TRUE))

spol_graf <- ggplot(spol %>% filter(GEO %in% c("Slovenia"))) + 
  aes(x=SEX,y=applicants,fill=GEO,color=GEO)+ 
  geom_bar(stat="identity",position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
  ggtitle(paste0("Sex of asylum seekers in ","Slovenia"))+xlab("")+ylab("")

#STOLPČNI - Starost
starost <- tidy_Starost %>%
  filter(ASYL_APP == "Asylum applicant") %>%
  group_by(GEO,AGE) %>% summarise(applicants = sum(Value,na.rm=TRUE)) 

starost_graf <- ggplot(starost %>% filter(GEO %in% c("Slovenia"))) + 
  aes(x=AGE,y=applicants,fill=GEO,color=GEO)+ 
  geom_bar(stat="identity",position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
  xlab("")+ylab("")+
  ggtitle(paste0("Age of asylum seekers in ","Slovenia"))
                
#STOLPČNI - Origin 
origin <- tidy_Origin %>%
  filter(ASYL_APP == "Asylum applicant") %>%
  group_by(GEO,CITIZEN) %>% summarise(applicants = sum(Value, na.rm=TRUE))
origin_graf <- ggplot(origin %>% filter(GEO %in% c("Slovenia")),
                      aes(x=factor(1), y=applicants, fill=CITIZEN)) +
  geom_bar(stat="identity", width=1) + coord_polar(theta="y") +
  ggtitle(paste0("Origin of asylum seekers in ","Slovenia")) +
  labs(x="",y="")+theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

#PREDIKCIJSKI MODEL - testni model, faktorje prilagajamo v Shiny aplikaciji
faktor1=0
faktor2=0
faktor3=0


napoved_graf <- ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",GEO %in% c("Slovenia")) %>%
                         mutate(applicants=Value+faktor1*Value+faktor2*Value+faktor3*Value),
                       aes(x=MONTH,y=applicants,group=GEO,color=GEO))+xlab("")+ylab("")+geom_smooth(method = "loess")
                       

lin <- lm(data=(tidy_Prosnje2015 %>% filter(GEO %in% c("Germany")) ),MONTH ~ Value)
kv <- lm(data=(tidy_Prosnje2015 %>% filter(GEO %in% c("Germany")) ),MONTH ~ Value+I(Value^2))
mls <- loess(data=(tidy_Prosnje2015 %>% filter(GEO %in% c("Germany"))),MONTH ~ Value)
mgam <- gam(data=(tidy_Prosnje2015 %>% filter(GEO %in% c("Germany"))),MONTH ~ s(Value))
sapply(list(lin, kv, mls, mgam), function(x) sum(x$residuals^2))

#Ugotovitev, da na primeru Nemčije najbolj ustreza uporaba metode loess 