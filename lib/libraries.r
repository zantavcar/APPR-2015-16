library(knitr)



# Uvozimo funkcije za delo z datotekami XML.
source("lib/xml.r", encoding = "UTF-8")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

require(dplyr)
require(rvest)
require(gsubfn)
require(reshape2)
require(ggplot2)
require(mgcv)
require(shiny)
