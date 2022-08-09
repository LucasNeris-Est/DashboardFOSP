library(tidyverse)
library(foreign)

rm(list=ls())
setwd("input")

df = read.dbf("pacigeral.dbf")

df = df %>% select(DTDIAG,ULTINFO, ANODIAG, DTULTINFO, 
                   TOPOGRUP,SEXO, TRATAMENTO, RADIO, 
                   CIRURGIA, QUIMIO, IMUNO, TMO, HORMONIO, FAIXAETAR,
                   CATEATEND)

write.dbf(df, "pacigeral_mod.dbf")
