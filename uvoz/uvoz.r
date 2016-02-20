
#2. FAZA

require(dplyr)
require(gsubfn)
require(ggplot2)
require(maptools)
require(sp)


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

  return(tidy_Origin)
}
tidy_Origin <- uvoz.Origin()
message("Uvažam podatke o državljanstvu azilantov...\n")

#ZA OBJAVO
objava_Prosnje <- tidy_Prosnje2015 %>% filter(GEO %in% c("European Union","Germany",
                                                     "Hungary","Sweden","Slovenia") &
                                            TIME %in% c("2015M01"))

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
Q4 <- tidy_Prosnje2015 %>%
  filter(ASYL_APP == "Asylum applicant", (MONTH > 9)) %>%
  group_by(GEO) %>% summarise(applicants = sum(Value))



tabela_Prosnje <- inner_join(Q1,Q2,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,Q3,by="GEO")
tabela_Prosnje <- right_join(tabela_Prosnje,Q4,by="GEO")
colnames(tabela_Prosnje) <- c("GEO","Q1","Q2","Q3","Q4")
sprememba1 <- round((tabela_Prosnje[,"Q2"]-tabela_Prosnje[,"Q1"])/tabela_Prosnje[,"Q1"],digits=4)*100
sprememba2 <- round ((tabela_Prosnje[,"Q3"]-tabela_Prosnje[,"Q2"])/tabela_Prosnje[,"Q2"],digits=4)*100
sprememba3 <- round((tabela_Prosnje[,"Q4"]-tabela_Prosnje[,"Q3"])/tabela_Prosnje[,"Q3"],digits=4)*100
sprememba1 <- paste0(sprememba1[,1]," %")
sprememba2 <- paste0(sprememba2[,1]," %")
sprememba3 <- paste0(sprememba3[,1]," %")

#PRIPRAVA TABELE ZA UVOZ
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
             aes(x = TIME, y = Value, group = GEO, color = GEO))+geom_line()

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
  xlim(-10, 50) + ylim(34, 72) + ggtitle("Prošnje za azil na milijon prebivalstva") 

#TORTNI - Moški, Ženkse
moski <- tidy_Spol %>%
  filter(SEX == "Males") %>%
  group_by(GEO) %>% summarise(Males = sum(Value,na.rm = TRUE))
zenske <- tidy_Spol %>%
  filter(SEX == "Females") %>%
  group_by(GEO) %>% summarise (Females = sum(Value, na.rm = TRUE))
spol <- inner_join(moski,zenske,by = "GEO")

spol_graf <- 
  pie(as.numeric(select(spol %>% filter(GEO %in% c("Slovenia")),c(2,3))),
      label=c(paste0("Males \n",select(spol %>% filter(GEO %in% c("Slovenia")),c(2))),
              paste0("Females \n",select(spol %>% filter(GEO %in% c("Slovenia")),c(3)))),
      col=c("lightblue","pink"),
      main=paste0("Sex of asylum seekers in ","Slovenia"))

#STOLPČNI - Starost
starost <- tidy_Starost %>%
  filter(ASYL_APP == "Asylum applicant") %>%
  group_by(GEO,AGE) %>% summarise(applicants = sum(Value,na.rm=TRUE))

starost_graf <- ggplot(starost %>% filter(GEO %in% c("Germany","Austria"))) + 
  aes(x=AGE,y=applicants,fill=GEO,color=GEO)+ 
  geom_bar(stat="identity",position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
  ggtitle(paste0("Age of asylum seekers in ","Slovenia"))
                
            
#PREDIKCIJSKI MODEL

