# rm(list = ls())
library(tidyverse)
# source("./Funcoes_AMB/Funcoes_de_Formatacao_Estilo.R")

# CARREGANDO Reserva AMB ----

reserva_AMB <-
  read.table(
    "./data/DBAMB_Reserva.csv",
    header = TRUE,
    sep = "\t",    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'UTF-8',
    fill = TRUE,
    quote = "")


colnames(reserva_AMB) <-
  colnames(reserva_AMB) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")


reserva_AMB$Municipio.Mina <-
  reserva_AMB$Municipio.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

reserva_AMB$Substancia.AMB <-
  reserva_AMB$Substancia.AMB %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

reserva_AMB$Substancia.RAL <-
  reserva_AMB$Substancia.RAL %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

reserva_AMB$Minerio <-
  reserva_AMB$Minerio %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") 

reserva_AMB$Nome.Mina <-
  reserva_AMB$Nome.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

reserva_AMB$Municipio.Mina <-
  gsub(reserva_AMB$Municipio.Mina,
       pattern = "sao luiz do paraitinga",
       replacement = "sao luis do paraitinga")


#_____acrescentando percentil de reserva ----
reserva_AMB$id_subs.ano <-
  paste(reserva_AMB$Substancia.AMB, reserva_AMB$Ano.Base.Ral, sep = "_")

reserva_AMB <-
  left_join(
    reserva_AMB,
    select(reserva_AMB, Ano.Base.Ral, id_subs.ano, Massa.Medida) %>%
      group_by(id_subs.ano) %>%
      summarise("percentil_80" = quantile(
        Massa.Medida, probs = 0.8, na.rm = TRUE
      )),by = "id_subs.ano")

 # valores > 80%, pareto = 1
reserva_AMB$pareto <- 0
for (i in 1:nrow(reserva_AMB)) {
  
  if (reserva_AMB$Massa.Medida[i] > reserva_AMB$percentil_80[i]) {
    reserva_AMB$pareto[i] <- 1   
  }} 





# CARREGANDO Produção BRUTA ----

producaoBRUTA <-
  read.table(
    "./data/DBAMB_MovimentacaoProducaoBruta.csv",
    header = TRUE,
    sep = "\t",dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'UTF-8',
    fill = TRUE,
    quote = ""
  )


colnames(producaoBRUTA) <-
  colnames(producaoBRUTA) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")


producaoBRUTA$Municipio.Mina <-
  producaoBRUTA$Municipio.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()


producaoBRUTA$Substancia.AMB <-
  producaoBRUTA$Substancia.AMB %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

producaoBRUTA$Nome.Mina <-
  producaoBRUTA$Nome.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()



#_____acrescentando percentil de produção ----
producaoBRUTA$id_subs.ano <-
  paste(producaoBRUTA$Substancia.AMB, producaoBRUTA$Ano.Base.Ral, sep = "_")

producaoBRUTA <-
  left_join(
    producaoBRUTA,
    select(producaoBRUTA, Ano.Base.Ral, id_subs.ano, Quantidade.Producao.Substancia.com.Ajuste) %>%
      group_by(id_subs.ano) %>%
      summarise("percentil_80" = quantile(
        Quantidade.Producao.Substancia.com.Ajuste, probs = 0.8, na.rm = TRUE
      )),by = "id_subs.ano")

# valores > 80%, pareto = 1
producaoBRUTA$pareto <- 0
for (i in 1:nrow(producaoBRUTA)) {
  
  if (producaoBRUTA$Quantidade.Producao.Substancia.com.Ajuste[i] > producaoBRUTA$percentil_80[i]) {
    producaoBRUTA$pareto[i] <- 1   
  }} 

# Valor Unitário de Venda
producaoBRUTA$preco <-
  round(producaoBRUTA$Valor.Venda.com.Ajuste.por.Substancia / 
          producaoBRUTA$Quantidade.Venda.Substancia.com.Ajuste, digits = 1)




# CARREGANDO Produção BENEFICIADA ----

producaoBENEFICIADA <-
  read.table(
    "./data/DBAMB_MovimentacaoProducaoBeneficiada.csv",
    header = TRUE,
    sep = "\t",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'UTF-8',
    fill = TRUE,quote = ""
  )




colnames(producaoBENEFICIADA) <-
  colnames(producaoBENEFICIADA) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

# minusculas
producaoBENEFICIADA$Municipio.Usina <-
  producaoBENEFICIADA$Municipio.Usina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

producaoBENEFICIADA$Substancia.AMB <-
  producaoBENEFICIADA$Substancia.AMB %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

producaoBENEFICIADA$Nome.Usina <-
  producaoBENEFICIADA$Nome.Usina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

# ID cpf/cnpj- USINA

producaoBENEFICIADA$id_cpfcnpj.usina <- 
  paste(producaoBENEFICIADA$CPF.CNPJ.Titular, producaoBENEFICIADA$Nome.Usina, sep = "_")

# ID cpf/cnpj- municipio

producaoBENEFICIADA$id_cpfcnpj.municipio <- 
  paste(producaoBENEFICIADA$CPF.CNPJ.Titular, producaoBENEFICIADA$Municipio.Usina, sep = "_")


#_____acrescentando percentil de produção ----
producaoBENEFICIADA$id_subs.ano <-
  paste(producaoBENEFICIADA$Substancia.AMB, producaoBENEFICIADA$Ano.Base.Ral, sep = "_")

producaoBENEFICIADA <-
  left_join(
    producaoBENEFICIADA,
    select(producaoBENEFICIADA, Ano.Base.Ral, id_subs.ano, Quantidade.Producao.Substancia.com.Ajuste) %>%
      group_by(id_subs.ano) %>%
      summarise("percentil_80" = quantile(
        Quantidade.Producao.Substancia.com.Ajuste, probs = 0.8, na.rm = TRUE
      )),by = "id_subs.ano")

# valores > 80%, pareto = 1
producaoBENEFICIADA$pareto <- 0
for (i in 1:nrow(producaoBENEFICIADA)) {
  
  if (producaoBENEFICIADA$Quantidade.Producao.Substancia.com.Ajuste[i] > producaoBENEFICIADA$percentil_80[i]) {
    producaoBENEFICIADA$pareto[i] <- 1   
  }} 


# Valor Unitário de Venda
producaoBENEFICIADA$preco <-
  round(producaoBENEFICIADA$Valor.Venda.com.Ajuste.por.Substancia / 
          producaoBENEFICIADA$Quantidade.Venda.Substancia.com.Ajuste, digits = 1)

# excluindo usinas sem produção (inclusive as usinas automáticas criadas redundantemente)

linhas <- list()
for (i in 1:nrow(producaoBENEFICIADA)) {
  if (sum(producaoBENEFICIADA[i, c(
    "Quantidade.Producao.Substancia.com.Ajuste",
    "Contido.Substancia",
    "Quantidade.Venda.Substancia.com.Ajuste",
    "Valor.Venda.com.Ajuste.por.Substancia",
    "Quantidade.Producao.Com.Ajuste",
    "Quantidade.Venda.com.Ajuste",
    "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor"
  )], na.rm = TRUE) == 0) {
    linhas[i] <- i
  }
}
linhas <- do.call(what = 'rbind',args = linhas)


producaoBENEFICIADA <-
  producaoBENEFICIADA[-linhas, ]





# CARREGANDO Produção QUANTIDADE_E_VALOR_DA_PRODUÇÃO_COMERCIALIZADA ----

VPM_QuantidadeValorCOMERCIALIZADO <-
  read.table(
    "D:/Users/humberto.serna/Documents/D_Lake/DBAMB_QuantidadeEhValordaProducaoMineralComercializada.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'latin1',
    fill = TRUE,
    quote = ""
  )


colnames(VPM_QuantidadeValorCOMERCIALIZADO) <-
  colnames(VPM_QuantidadeValorCOMERCIALIZADO) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
colnames(VPM_QuantidadeValorCOMERCIALIZADO) <-
  c("Ano", "CPFCNPJ", "unidade", "Titular", 
    "tipo", "Municipio", "uf", "Substancia.AMB", "Substancia.RAL", "produto", 
    "Quantidade.Producao.Comercializada.Substancia", "Valor.Producao.Comercializada.Substancia.AMB", 
    "Quantidade.Producao.Comercializada.Produto", "Valor.Producao.Comercializada.Produto"
  ) %>% tolower()


VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL <- 
  VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM_QuantidadeValorCOMERCIALIZADOMunicipio.Mina <-
  VPM_QuantidadeValorCOMERCIALIZADOMunicipio.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

VPM_QuantidadeValorCOMERCIALIZADO$titular <-
  VPM_QuantidadeValorCOMERCIALIZADO$titular %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB <-
  VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

VPM_QuantidadeValorCOMERCIALIZADO$produto <-
  VPM_QuantidadeValorCOMERCIALIZADO$produto %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

VPM_QuantidadeValorCOMERCIALIZADO$tipo <-
  VPM_QuantidadeValorCOMERCIALIZADO$tipo %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()





# CARREGANDO consumidores MINA ----

consumidoresMINA <-
  read.table(
    file = "D:/Users/humberto.serna/Documents/D_Lake/DBAMB_PrincipaisCompradoresMina.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'latin1',
    fill = TRUE,
    quote = ""
  )



colnames(consumidoresMINA) <-
  colnames(consumidoresMINA) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") 
colnames(consumidoresMINA) <-
  c(
    "Ano.Base.Ral",
    "titular",
    "cpfcnpj",
    "Nome.Mina",
    "municipio",
    "uf",
    "substancias.amb.Nome.Mina",
    "Minerio",
    "nome.comprador",
    "uso.destinacao",
    "municipio.comprador",
    "uf.comprador",
    "quantidade.compra",
    "valor.compra"
  ) %>% tolower()


consumidoresMINAMunicipio.Mina <-
  consumidoresMINAMunicipio.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

consumidoresMINA$titular <-
  consumidoresMINA$titular %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

consumidoresMINA$substancias.amb.Nome.Mina <-
  consumidoresMINA$substancias.amb.Nome.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

consumidoresMINA$Minerio <-
  consumidoresMINA$Minerio %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

consumidoresMINA$Nome.Mina <-
  consumidoresMINA$Nome.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

consumidoresMINA$preco <-
  round(consumidoresMINA$valor.compra / consumidoresMINA$quantidade.compra,
        digits = 1)


# CARREGANDO consumidores USINA ----

consumidoresUSINA <-
  read.table(
    file = "D:/Users/humberto.serna/Documents/D_Lake/DBAMB_PrincipaisCompradoresUsina.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'latin1',
    fill = TRUE,
    quote = ""
    )

colnames(consumidoresUSINA) <-
  colnames(consumidoresUSINA) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
colnames(consumidoresUSINA) <-
  c(
    "Ano.Base.Ral",
    "titular",
    "cpfcnpj",
    "usina",
    "municipio",
    "uf",
    "produto.beneficiado",
    "substancias.amb.usina",
    "nome.comprador",
    "uso.destinacao",
    "quantidade.compra",
    "valor.compra"
  ) %>% tolower()


consumidoresUSINAMunicipio.Mina <-
  consumidoresUSINAMunicipio.Mina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()


consumidoresUSINA$titular <-
  consumidoresUSINA$titular %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()

consumidoresUSINA$substancias.amb.usina <-
  consumidoresUSINA$substancias.amb.usina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

consumidoresUSINA$usina <-
  consumidoresUSINA$usina %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

consumidoresUSINA$produto.beneficiado <-
  consumidoresUSINA$produto.beneficiado %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower() %>% stringr::str_squish()

consumidoresUSINA$preco <- round(
  consumidoresUSINA$valor.compra / consumidoresUSINA$quantidade.compra,
  digits = 1
)


# CARREGANDO eventos RFP_RRR ----

Eventos_RRR_RFP <-
  read.table(
    file = "D:/Users/humberto.serna/Documents/D_Lake/DBAMB_Reserva_Eventos_RRR_RFP.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'latin1',
    fill = TRUE,
    quote = ""
  )


colnames(Eventos_RRR_RFP) <- 
  c("Ano.Base.Ral", "cpfcnpj", "processo", "Substancia.AMB", "reavaliacao.reserva", 
    "situacao.operacional.processo", "motivo.situacao.processo")

# CARREGANDO Informacoes Complementares ----

InformacoesComplementares <-
  read.table(
    file = "D:/Users/humberto.serna/Documents/D_Lake/DBAMB_InformacoesComplementares.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = 'latin1',
    fill = TRUE,
    quote = ""
  )



colnames(InformacoesComplementares) <- 
  c("Ano.Base.Ral", "processo", "cpfcnpj", "informacoes_Complementares", 
    "data_Motivo_Situacao_Processo", "motivo_Processo",
    "situacao_Operacional_Processo")




InformacoesComplementares <-
  InformacoesComplementares[, c(
    "Ano.Base.Ral","processo","cpfcnpj",
    "data_Motivo_Situacao_Processo",
    "motivo_Processo",
    "situacao_Operacional_Processo",
    "informacoes_Complementares"
  )]




# CARREGANDO GEOCOD ----

source("./R/geocod.R")


# geocod <-
#   read.table(
#     file = "./data/geocod.csv",
#     header = TRUE,
#     sep = ";",
#     dec = ",",
#     stringsAsFactors = FALSE,
#     encoding = 'latin1',
#     fill = TRUE,
#     quote = ""
#   )
# 
# 
# geocod$NM_MUNICIP_STRING <- 
#   geocod$NM_MUNICIP_STRING %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% tolower()
# 
# geocod <- geocod[,c(1,2)]
# colnames(geocod) <- c("municipio", "geocod")



