rm(list=ls())
source("rootdirpath.R")

cfdata = read.csv("datasets/cardfraud/raw/creditcard.csv")
tlab <- cfdata$Class
tset <- cfdata[1:30]
