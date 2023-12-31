# rm(list = ls())
library(tidyverse)
# source("./Funcoes_AMB/Funcoes_de_Formatacao_Estilo.R")

# CARREGANDO Reserva AMB ----

reserva_AMB <-
  read.table("./data/CubosDBAMB_Reserva.csv",
             header = TRUE, sep = "\t",dec = ".",
             fill = TRUE, quote = "",
             fileEncoding = 'UTF-8')
  
  # read.table(
  #   "./data/DBAMB_Reserva.csv",
  #     header = TRUE, sep = "\t",dec = ",",
  #   fill = TRUE, quote = "",
  #   fileEncoding = 'latin1')





 colnames(reserva_AMB) <-
   colnames(reserva_AMB) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")


reserva_AMB$Municipio.Mina <-
  reserva_AMB$Municipio.Mina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()

reserva_AMB$Substancia.AMB <-
  reserva_AMB$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

reserva_AMB$Substancia.Agrupadora <-
  reserva_AMB$Substancia.Agrupadora |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

reserva_AMB$Substancia.RAL <-
  reserva_AMB$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

reserva_AMB$Minerio <-
  reserva_AMB$Minerio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") 

reserva_AMB$Nome.Mina <-
  reserva_AMB$Nome.Mina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()

reserva_AMB$Municipio.Mina <-
  gsub(reserva_AMB$Municipio.Mina,
       pattern = "sao luiz do paraitinga",
       replacement = "sao luis do paraitinga")


#____________________________________ Teor Reserva Medida **************************

reserva_AMB$teor_medido <- NA
Substancias_teor <- c("Ferro","Barita","Prata (Primaria)","Pirocloro","Niquel","Cobre","Diamante (Secundario)","Fosfato","Cassiterita (Primaria)","Ouro (Primario)","Uranio","Ilmenita","Monazita","Rutilo","Zirconita (Secundaria)","Diamante (Primario)","Cassiterita (Secundaria)","Bauxita Refrataria","Manganes","Terras-Raras","Ouro (Secundario)","Potassio","Bauxita Metalurgica","Tungstenio","Gemas (Primaria)","Gemas (Secundaria)","Anatasio","Chumbo","Cromo","Vermiculita e Perlita","Zinco","Grafita","Fluorita","Tantalo (Columbita-Tantalita)-Secundario","Cobalto","Agatas Calcedonia etc..","Zirconita (Primaria)","Molibdenio","Turmalina Industrial","Crisotila","Vanadio","Petalita","Espodumenio","Berilio","Enxofre","Tantalo (Columbita-Tantalita)-Primario","Lepidolita","Zirconio (Oxidos)","Djalmaita","Niobio (Columbita-Tantalita)-Primaria","Geodos de Ametista","Criolita","Niobio (Columbita-Tantalita)-Secundaria","Cadmio","Prata (Secundaria)","Corindon","Paladio","Platina","Bismuto","Nao Informado")

reserva_AMB$teor_medido <- 
  ifelse(reserva_AMB$Substancia.AMB %in% Substancias_teor,
         ifelse(reserva_AMB$Unidade.do.Contido == "kg",
                round(100*(((reserva_AMB$Contido.Medido)/1000)/reserva_AMB$Massa.Medida)),
                round(100*((reserva_AMB$Contido.Medido)/reserva_AMB$Massa.Medida))),
         NA)

#_____acrescentando percentil de reserva ----
reserva_AMB$id_subs.ano <-
  paste(reserva_AMB$Substancia.AMB, reserva_AMB$Ano.Base.Ral, sep = "_")

reserva_AMB <-
  left_join(
    reserva_AMB,
    select(reserva_AMB, Ano.Base.Ral, id_subs.ano, Massa.Medida) |>
      group_by(id_subs.ano) |>
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
    "./data/CubosDBAMB_MovimentacaoProducaoBruta.csv",
    header = TRUE, sep = "\t",dec = ".",
    fill = TRUE, quote = "",
    fileEncoding = 'UTF-8')

colnames(producaoBRUTA) <-
  colnames(producaoBRUTA) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBRUTA$Quantidade.Producao.Com.Ajuste <- 
  as.numeric(producaoBRUTA$Quantidade.Producao.Com.Ajuste)

producaoBRUTA$Quantidade.Venda.com.Ajuste <- 
  as.numeric(producaoBRUTA$Quantidade.Venda.com.Ajuste)

producaoBRUTA$Valor.Venda.com.Ajuste.por.Minerio <- 
  as.numeric(producaoBRUTA$Valor.Venda.com.Ajuste.por.Minerio)

producaoBRUTA$Contido.Substancia <- 
  as.numeric(producaoBRUTA$Contido.Substancia)

producaoBRUTA$Municipio.Mina <-
  producaoBRUTA$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()

producaoBRUTA$Substancia.AMB <-
  producaoBRUTA$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBRUTA$Substancia.Agrupadora <-
  producaoBRUTA$Substancia.Agrupadora |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBRUTA$Substancia.RAL <-
  producaoBRUTA$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBRUTA$Minerio <-
  producaoBRUTA$Minerio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") 

producaoBRUTA$Nome.Mina <-
  producaoBRUTA$Nome.Mina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()



# Teor 

producaoBRUTA$teor_medido <- NA
Substancias_teor <- c("Ferro","Barita","Prata (Primaria)","Pirocloro","Niquel","Cobre","Diamante (Secundario)","Fosfato","Cassiterita (Primaria)","Ouro (Primario)","Uranio","Ilmenita","Monazita","Rutilo","Zirconita (Secundaria)","Diamante (Primario)","Cassiterita (Secundaria)","Bauxita Refrataria","Manganes","Terras-Raras","Ouro (Secundario)","Potassio","Bauxita Metalurgica","Tungstenio","Gemas (Primaria)","Gemas (Secundaria)","Anatasio","Chumbo","Cromo","Vermiculita e Perlita","Zinco","Grafita","Fluorita","Tantalo (Columbita-Tantalita)-Secundario","Cobalto","Agatas Calcedonia etc..","Zirconita (Primaria)","Molibdenio","Turmalina Industrial","Crisotila","Vanadio","Petalita","Espodumenio","Berilio","Enxofre","Tantalo (Columbita-Tantalita)-Primario","Lepidolita","Zirconio (Oxidos)","Djalmaita","Niobio (Columbita-Tantalita)-Primaria","Geodos de Ametista","Criolita","Niobio (Columbita-Tantalita)-Secundaria","Cadmio","Prata (Secundaria)","Corindon","Paladio","Platina","Bismuto","Nao Informado")


producaoBRUTA$teor_medido <- 
  ifelse(producaoBRUTA$Substancia.AMB %in% Substancias_teor,
         ifelse(producaoBRUTA$Unidade.de.Medida.Contido == "kg",
                round(100*(((producaoBRUTA$Contido.Substancia)/1000)/producaoBRUTA$Quantidade.Producao.Com.Ajuste)),
                round(100*((producaoBRUTA$Contido.Substancia)/producaoBRUTA$Quantidade.Producao.Com.Ajuste))),
         NA)



producaoBRUTA$teor_medido <-
  ifelse(
    producaoBRUTA$Substancia.AMB %in% Substancias_teor,
    ifelse(
      producaoBRUTA$Unidade.de.Medida.Contido == "kg" &
        producaoBRUTA$Unidade.Medida.Minerio == 't',
      round(
        100 * (((producaoBRUTA$Contido.Substancia) / 1000) / producaoBRUTA$Quantidade.Producao.Com.Ajuste
        )
      ),
      ifelse(
        producaoBRUTA$Unidade.de.Medida.Contido == "g" &
          producaoBRUTA$Unidade.Medida.Minerio == 'kg',
        round(
          100 * (((producaoBRUTA$Contido.Substancia) / 1000) / producaoBRUTA$Quantidade.Producao.Com.Ajuste
          )
        ),
        round(
          100 * ((producaoBRUTA$Contido.Substancia) / producaoBRUTA$Quantidade.Producao.Com.Ajuste
          )
        )
      )
    ),
    NA)


#_____acrescentando percentil de produção ----
producaoBRUTA$id_subs.ano <-
  paste(producaoBRUTA$Substancia.AMB, producaoBRUTA$Ano.Base.Ral, sep = "_")

producaoBRUTA <-
  left_join(
    producaoBRUTA,
    select(producaoBRUTA, Ano.Base.Ral, id_subs.ano, Quantidade.Producao.Com.Ajuste) |>
      group_by(id_subs.ano) |>
      summarise("percentil_80" = quantile(
        Quantidade.Producao.Com.Ajuste, probs = 0.8, na.rm = TRUE
      )),by = "id_subs.ano")

# valores > 80%, pareto = 1
producaoBRUTA$pareto <- 0
for (i in 1:nrow(producaoBRUTA)) {
  
  if (is.na(producaoBRUTA$Quantidade.Producao.Com.Ajuste[i]) == FALSE) {
  
  if (producaoBRUTA$Quantidade.Producao.Com.Ajuste[i] > producaoBRUTA$percentil_80[i]) {
    producaoBRUTA$pareto[i] <- 1   
  }} }

# Valor Unitário de Venda
producaoBRUTA$preco <-
  round(producaoBRUTA$Valor.Venda.com.Ajuste.por.Minerio / 
          producaoBRUTA$Quantidade.Venda.com.Ajuste, digits = 1)




# CARREGANDO Produção BENEFICIADA ----

producaoBENEFICIADA <-
  read.table(
    "./data/CubosDBAMB_MovimentacaoProducaoBeneficiada.csv",
    header = TRUE, sep = "\t",dec = ".",
    fill = TRUE, quote = "",
    fileEncoding = 'UTF-8')

colnames(producaoBENEFICIADA) <- 
  colnames(producaoBENEFICIADA) |> iconv(from = "UTF-8",  to = "ASCII//TRANSLIT")


# minusculas
producaoBENEFICIADA$Municipio.Usina <-
  producaoBENEFICIADA$Municipio.Usina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()

producaoBENEFICIADA$Substancia.AMB <-
  producaoBENEFICIADA$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBENEFICIADA$Nome.Usina <-
  producaoBENEFICIADA$Nome.Usina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()

producaoBENEFICIADA$Substancia.AMB <-
  producaoBENEFICIADA$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBENEFICIADA$Substancia.Agrupadora <-
  producaoBENEFICIADA$Substancia.Agrupadora |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

producaoBENEFICIADA$Produto.Beneficiado <-
  producaoBENEFICIADA$Produto.Beneficiado |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

# ID cpf/cnpj- USINA

producaoBENEFICIADA$id_cpfcnpj.usina <- 
  paste(producaoBENEFICIADA$CPF.CNPJ.Titular, producaoBENEFICIADA$Nome, sep = "_")

# ID cpf/cnpj- Municipio

producaoBENEFICIADA$id_cpfcnpj.municipio <- 
  paste(producaoBENEFICIADA$CPF.CNPJ.Titular, producaoBENEFICIADA$Municipio.Usina, sep = "_")

# Teor 

producaoBENEFICIADA$teor_medido <- NA
Substancias_teor <- c("Ferro","Barita","Prata (Primaria)","Pirocloro","Niquel","Cobre","Diamante (Secundario)","Fosfato","Cassiterita (Primaria)","Ouro (Primario)","Uranio","Ilmenita","Monazita","Rutilo","Zirconita (Secundaria)","Diamante (Primario)","Cassiterita (Secundaria)","Bauxita Refrataria","Manganes","Terras-Raras","Ouro (Secundario)","Potassio","Bauxita Metalurgica","Tungstenio","Gemas (Primaria)","Gemas (Secundaria)","Anatasio","Chumbo","Cromo","Vermiculita e Perlita","Zinco","Grafita","Fluorita","Tantalo (Columbita-Tantalita)-Secundario","Cobalto","Agatas Calcedonia etc..","Zirconita (Primaria)","Molibdenio","Turmalina Industrial","Crisotila","Vanadio","Petalita","Espodumenio","Berilio","Enxofre","Tantalo (Columbita-Tantalita)-Primario","Lepidolita","Zirconio (Oxidos)","Djalmaita","Niobio (Columbita-Tantalita)-Primaria","Geodos de Ametista","Criolita","Niobio (Columbita-Tantalita)-Secundaria","Cadmio","Prata (Secundaria)","Corindon","Paladio","Platina","Bismuto","Nao Informado")



producaoBENEFICIADA$teor_medido <-
  ifelse(
    producaoBENEFICIADA$Substancia.AMB %in% Substancias_teor,
    ifelse(
      producaoBENEFICIADA$Unidade.de.Medida.Contido == "kg" &
        producaoBENEFICIADA$Unidade.Medida.Produto.Beneficiado == 't',
      round(
        100 * (((producaoBENEFICIADA$Contido.Substancia) / 1000) / producaoBENEFICIADA$Quantidade.Producao.Com.Ajuste
        )
      ),
      ifelse(
        producaoBENEFICIADA$Unidade.de.Medida.Contido == "g" &
          producaoBENEFICIADA$Unidade.Medida.Produto.Beneficiado == 'kg',
        round(
          100 * (((producaoBENEFICIADA$Contido.Substancia) / 1000) / producaoBENEFICIADA$Quantidade.Producao.Com.Ajuste
          )
        ),
        round(
          100 * ((producaoBENEFICIADA$Contido.Substancia) / producaoBENEFICIADA$Quantidade.Producao.Com.Ajuste
          )
        )
      )
    ),
    NA
  )



#_____acrescentando percentil de produção ----
producaoBENEFICIADA$id_subs.ano <-
  paste(producaoBENEFICIADA$Substancia.AMB, producaoBENEFICIADA$Ano.Base.Ral, sep = "_")

producaoBENEFICIADA <-
  left_join(
    producaoBENEFICIADA,
    select(producaoBENEFICIADA, Ano.Base.Ral, id_subs.ano, Quantidade.Producao.Com.Ajuste) |>
      group_by(id_subs.ano) |>
      summarise("percentil_80" = quantile(
        Quantidade.Producao.Com.Ajuste, probs = 0.8, na.rm = TRUE
      )),by = "id_subs.ano")

# valores > 80%, pareto = 1
producaoBENEFICIADA$pareto <- 0
for (i in 1:nrow(producaoBENEFICIADA)) {
  
  if (is.na(producaoBENEFICIADA$Quantidade.Producao.Com.Ajuste[i]) == FALSE) {
  
  if (producaoBENEFICIADA$Quantidade.Producao.Com.Ajuste[i] > producaoBENEFICIADA$percentil_80[i]) {
    producaoBENEFICIADA$pareto[i] <- 1   
  }} }


# Valor Unitário de Venda
producaoBENEFICIADA$preco <-
  round(producaoBENEFICIADA$Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor / 
          producaoBENEFICIADA$Quantidade.Venda.com.Ajuste, digits = 1)

# excluindo usinas sem produção (inclusive as usinas autom?ticas criadas redundantemente)
linhas <- list()
for (i in 1:nrow(producaoBENEFICIADA)) {
  if (sum(
    producaoBENEFICIADA[i, c(
        "Quantidade.Venda.com.Ajuste", 
        "Quantidade.Producao.Com.Ajuste"
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
     "./data/DBAMB_QuantidadeEhValordaProducaoMineralComercializada.csv",
     header = TRUE, sep = "\t",dec = ",",
     fill = TRUE, quote = "",
     fileEncoding = 'latin1', encoding = 'UTF-8')
 
 
 colnames(VPM_QuantidadeValorCOMERCIALIZADO) <-
   colnames(VPM_QuantidadeValorCOMERCIALIZADO) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
 
 
 
  VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL <- 
    VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
 
 VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB <-
   VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
 
 VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado <-
   VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
 
 VPM_QuantidadeValorCOMERCIALIZADO$Municipio <-
   VPM_QuantidadeValorCOMERCIALIZADO$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
 
 VPM_QuantidadeValorCOMERCIALIZADO$Nome <-
   VPM_QuantidadeValorCOMERCIALIZADO$Nome |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()

 
 
 
# # CARREGANDO consumidores MINA ----
# 
# consumidoresMINA <-
#   read.table(
#     file = "./data/DBAMB_PrincipaisCompradoresMina.csv",
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
# 
# colnames(consumidoresMINA) <-
#   colnames(consumidoresMINA) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") 
# colnames(consumidoresMINA) <-
#   c(
#     "ano",
#     "titular",
#     "cpfcnpj",
#     "Nome",
#     "Municipio",
#     "uf",
#     "substancias.amb.Nome",
#     "Minerio",
#     "nome.comprador",
#     "uso.destinacao",
#     "Municipio.comprador",
#     "uf.comprador",
#     "quantidade.compra",
#     "valor.compra"
#   ) |> tolower()
# 
# 
# consumidoresMINA$Municipio <-
#   consumidoresMINA$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()
# 
# consumidoresMINA$titular <-
#   consumidoresMINA$titular |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()
# 
# consumidoresMINA$substancias.amb.Nome <-
#   consumidoresMINA$substancias.amb.Nome |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()
# 
# consumidoresMINA$Minerio <-
#   consumidoresMINA$Minerio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()
# 
# consumidoresMINA$Nome <-
#   consumidoresMINA$Nome |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()
# 
# consumidoresMINA$preco <-
#   round(consumidoresMINA$valor.compra / consumidoresMINA$quantidade.compra,
#         digits = 1)
# 
# 
# # CARREGANDO consumidores USINA ----
# 
# consumidoresUSINA <-
#   read.table(
#     file = "./data/DBAMB_PrincipaisCompradoresUsina.csv",
#     header = TRUE,
#     sep = ";",
#     dec = ",",
#     stringsAsFactors = FALSE,
#     encoding = 'latin1',
#     fill = TRUE,
#     quote = ""
#     )
# 
# colnames(consumidoresUSINA) <-
#   colnames(consumidoresUSINA) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
# colnames(consumidoresUSINA) <-
#   c(
#     "ano",
#     "titular",
#     "cpfcnpj",
#     "usina",
#     "municipio",
#     "uf",
#     "produto.beneficiado",
#     "substancias.amb.usina",
#     "nome.comprador",
#     "uso.destinacao",
#     "quantidade.compra",
#     "valor.compra"
#   ) |> tolower()
# 
# 
# consumidoresUSINA$municipio <-
#   consumidoresUSINA$municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()
# 
# 
# consumidoresUSINA$titular <-
#   consumidoresUSINA$titular |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower()
# 
# consumidoresUSINA$substancias.amb.usina <-
#   consumidoresUSINA$substancias.amb.usina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()
# 
# consumidoresUSINA$usina <-
#   consumidoresUSINA$usina |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()
# 
# consumidoresUSINA$produto.beneficiado <-
#   consumidoresUSINA$produto.beneficiado |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()
# 
# consumidoresUSINA$preco <- round(
#   consumidoresUSINA$valor.compra / consumidoresUSINA$quantidade.compra,
#   digits = 1
# )
# 
# 
# # CARREGANDO eventos RFP_RRR ----
# 
# Eventos_RRR_RFP <-
#   read.table(
#     file = "./data/DBAMB_Reserva_Eventos_RRR_RFP.csv",
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
# colnames(Eventos_RRR_RFP) <- 
#   c("ano", "cpfcnpj", "processo", "Substancia.AMB", "reavaliacao.reserva", 
#     "situacao.operacional.processo", "motivo.situacao.processo")
# 
# # CARREGANDO Informacoes Complementares ----
# 
# InformacoesComplementares <-
#   read.table(
#     file = "./data/DBAMB_InformacoesComplementares.csv",
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
# 
# colnames(InformacoesComplementares) <- 
#   c("ano", "processo", "cpfcnpj", "informacoes_Complementares", 
#     "data_Motivo_Situacao_Processo", "motivo_Processo",
#     "situacao_Operacional_Processo")
# 
# 
# 
# 
# InformacoesComplementares <-
#   InformacoesComplementares[, c(
#     "ano","processo","cpfcnpj",
#     "data_Motivo_Situacao_Processo",
#     "motivo_Processo",
#     "situacao_Operacional_Processo",
#     "informacoes_Complementares"
#   )]

