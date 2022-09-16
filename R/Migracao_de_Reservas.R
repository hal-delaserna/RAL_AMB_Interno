# rm(list = ls())

library(tidyverse)
#__________________________________
# Scrip de procura por processos com eventos críticos envolvidos na decisão de migrar, ou não, uma reserva.


# procura todos os anos, CICLO ----

casos_especiais_MIGRACAO <- 
  data.frame("processo" = NA, "Tipo de requerimento" = NA, "Fase atual" = NA, "cpfcnpj" = NA, 
             "titular" = NA, "Munic?pios" = NA, "Subst?ncias" = NA, "Tipos de Uso" = NA, "Situa??o" = NA, 
             "Unidade Protocolizadora" = NA, "Superintend?ncia" = NA, "?rea (ha)" = NA)

for (ano in 2014:2018) {
  eventos <-
    as.list(c('processos_Bloqueio_ART42',
        'ARQUIVAMENTO',
        'processos_Cessao_Total',
        'processos_Cessao_Parcial',
        'processos_desmembramento',
        'processos_INCORPORACAO',
        'processos_RRR'))
  
  caminho <- list()
  for (i in 1:length(eventos)) {
    caminho[i] <-
      paste("./Eventos_", ano, "/", eventos[i], ".xls", sep = "")
  }
  
  
  for (j in 1:length(eventos)) {
    
    eventos[j] <-
      xml2::read_html(x = caminho[[j]], encoding = "ANSI") %>%
      rvest::html_table()}

  
  # N?o migrar
  processos_Bloqueio_ART42 <- data.frame(eventos[1])
  processos_RRR <- data.frame(eventos[7])
  # antes de migrar fazer importa??o RPA (RRR): o sys alertar? sobre exist?ncia pr?via de reserva RRR
  
  # altera??o de t?tular
  processos_Cessao_Total <- data.frame(eventos[3])
  processos_Cessao_Parcial <- data.frame(eventos[4])
  processos_desmembramento <- data.frame(eventos[5])
  processos_INCORPORACAO <- data.frame(eventos[6])
  ARQUIVAMENTO <- data.frame(eventos[2])
  
  df_aux <-
    bind_rows(
      list(
        processos_Bloqueio_ART42 = processos_Bloqueio_ART42,
        ARQUIVAMENTO = ARQUIVAMENTO,
        processos_RRR = processos_RRR,
        processos_Cessao_Parcial = processos_Cessao_Parcial,
        processos_Cessao_Total = processos_Cessao_Total,
        processos_desmembramento = processos_desmembramento,
        processos_INCORPORACAO = processos_INCORPORACAO
      ),
      .id = "evento")
  
  colnames(df_aux)[2:13] <- c("processo", "Tipo.de.requerimento", "Fase.atual", "cpfcnpj", 
                                    "titular", "Munic?pios", "Subst?ncias", "Tipos.de.Uso", "Situa??o", 
                                    "Unidade.Protocolizadora", "Superintend?ncia", "?rea..ha.")
  
  
  df_aux$ano <- ano
  
  casos_especiais_MIGRACAO <- bind_rows(casos_especiais_MIGRACAO, df_aux)
            }

casos_especiais_MIGRACAO <-
  casos_especiais_MIGRACAO[is.na(casos_especiais_MIGRACAO$processo) == FALSE &
                             casos_especiais_MIGRACAO$processo != "Processo", ]

casos_especiais_MIGRACAO$Subst?ncias <-
  casos_especiais_MIGRACAO[, "Subst?ncias"] %>% stringr::str_squish()

#______grava??o

write.table(
  x = casos_especiais_MIGRACAO[,c('processo',"evento","ano")],
  file = paste("./NAO_MIGRAR/casos_especiais_MIGRACAO_TODOS", ".csv", sep = ""),
  sep = ";",
  row.names = FALSE,
  quote = FALSE)

# BUSCA processos_CADUC_ReqLav ----

processos_CADUC_ReqLav <-
  data.frame(rvest::html_table(xml2::read_html(
    paste("./Eventos_", ano, "/", "processos_CADUC_ReqLav", ".xls", sep = ""),
    encoding = "ANSI")))


