rm(list=ls())
source("rootdirpath.R")

#  Wczytanie danych
cfdata = read.csv("datasets/cardfraud/raw/creditcard.csv")
tset <- cfdata

# Stworzenie formuły
targets <- list(Class~.)