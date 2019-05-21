## skrypt do przygotowania danych
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
current_path <- substr(ccurrent_path, 1, )
## zak³adamy, ¿e wczeœniej zmieniono œcie¿kê 
current_path <- "C:/Studia MGR/Semestr II/MOW/mowpartboostmain/mowpartboost"
setwd(current_path)
energydata = read.csv("datasets/energy/raw/energydata_complete.csv")

#przygotowanie danych o zu¿yciu energii
tset <- energydata[, seq(4,length(energydata)-2)]
tset$year <- as.POSIXlt(energydata[, 1])$year
tset$mon <- as.POSIXlt(energydata[, 1])$mon
tset$wday <- as.POSIXlt(energydata[, 1])$wday
tset$hour <- as.POSIXlt(energydata[, 1])$hour
tset$minutes <- as.POSIXlt(energydata[, 1])$min
tlab1 <- energydata$Appliances
tlab2 <- energydata$lights
#przygotowanie danych o zu¿yciu energii

#przygotowanie danych o oszustwach z kartami kredytowymi
#przygotowanie danych o oszustwach z kartami kredytowymi

#przygotowanie danych o abonentach sieci gsm
#przygotowanie danych o abonentach sieci gsm