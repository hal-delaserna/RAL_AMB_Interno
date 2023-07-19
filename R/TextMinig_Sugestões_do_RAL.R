# rm(list = ls())

library(tidyverse)
library('tm')

inf_compementares <-
  read.table(
    file = 'D:/Users/humberto.serna/Desktop/D_Lake/DBAMB_InformacoesComplementares.csv',
    header = TRUE,
    sep = ";",
    fill = TRUE, stringsAsFactors = FALSE
  )



sugestoes <-
  read.table(
    file = 'D:/Users/humberto.serna/Desktop/D_Lake/Sugestões_RAL.csv',
    header = TRUE,
    sep = ";",
    quote = "",
    fill = TRUE,
    encoding = "ANSI",
    stringsAsFactors = FALSE)


sugestoes$sugestao <-
  gsub(sugestoes$sugestao,
       pattern = "@",
       replacement = "")



sugestoes$sugestao <-
  stringr::str_squish(sugestoes$sugestao) %>% FUNA_removeAcentos()


release_corpus <- 
  Corpus(VectorSource(sugestoes$sugestao))

tm::TermDocumentMatrix(release_corpus)




