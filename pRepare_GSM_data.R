rm(list=ls())
source("rootdirpath.R")

gsmdata = read.delim("datasets/gsm/raw/orange_small_test.csv")
lab = read.delim("datasets/gsm/raw/orange_small_train_appetency.labels.csv", header = FALSE)


tset <- data.frame(1:dim(gsmdata)[1])
tlab <-list(1)
tlab <- lab[1]


j <- 1
# Lista tych atrybutow liczba rekordow "NA" jest mniejsza od 90% wszystkich rekordow
for (i in 1:length(names(gsmdata))) {
  n <- names(gsmdata)[i];
  s <- sum(sapply(gsmdata[n], is.na))/dim(gsmdata[n])[1] #number of NA values/number of observations
  print(paste("Variable ", names(i), as.character(s), "ratio of NA values"))
  if(s<0.20) {tset[j] <- gsmdata[i];j=j+1} # saving variables that have number of missing values less than 20%
}

tset["appetency_labels"] <- read.delim("datasets/gsm/raw/orange_small_train_appetency.labels.csv", header = FALSE)[1]
tset["upselling_labels"] <- read.delim("datasets/gsm/raw/orange_small_train_upselling.labels.csv", header = FALSE)[1]

# przerobienie -1 na 0 w lab, zapisanie tego do tlab - uzywany do obliczen
tset["upselling_labels"] <- (tset["upselling_labels"]+1)/2
tset["appetency_labels"] <- (tset["appetency_labels"]+1)/2

targets <- list(appetency_labels~.-upselling_labels, upselling_labels~.-appetency_labels)