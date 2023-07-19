# rm(list = ls())


# Subsetting ---- 
ano_0 <- 2011

SubstanciaAgrupadora <- "."


# carregamento 0 ----
QuantidadeEhValordaProducao <-
  read.table(file = "./data/CubosDBAMB_QuantidadeEhValordaProducaoMineralComercializada.csv",
             header = TRUE, sep = ",", quote = "\"", 
             dec = ".", fill = TRUE, row.names = NULL)



# ajuste de colunas numeric
QuantidadeEhValordaProducao$Quantidade.Produção.Comercializada...Produto <- 
  gsub(QuantidadeEhValordaProducao$Quantidade.Produção.Comercializada...Produto, 
       pattern = ",", replacement = "") |> as.numeric()

QuantidadeEhValordaProducao$Quantidade.Produção.Comercializada...Substância <- 
  gsub(QuantidadeEhValordaProducao$Quantidade.Produção.Comercializada...Substância, 
       pattern = ",", replacement = "") |> as.numeric()

QuantidadeEhValordaProducao$Valor.Produção.Comercializada...Produto <- 
  gsub(QuantidadeEhValordaProducao$Valor.Produção.Comercializada...Produto, 
       pattern = ",", replacement = "") |> as.numeric()



# subsetting
QuantidadeEhValordaProducao <- 
  QuantidadeEhValordaProducao[grepl(QuantidadeEhValordaProducao$Substância.AMB, 
                                  pattern = SubstanciaAgrupadora) & 
                              QuantidadeEhValordaProducao$Ano.Base.Ral >= ano_0,]  




# carregamento I ----
df <-
  read.table(file = "./data/CubosDBAMB_QuantidadeEhValordaProducaoMineralComercializada_AGREGADOS_I.csv",
             header = TRUE, sep = ",", quote = "\"", 
             dec = ".", fill = TRUE, row.names = NULL)



# ajuste de colunas numeric
df$Quantidade.Produção.Comercializada...Produto <- 
  gsub(df$Quantidade.Produção.Comercializada...Produto, 
       pattern = ",", replacement = "") |> as.numeric()

df$Quantidade.Produção.Comercializada...Substância <- 
  gsub(df$Quantidade.Produção.Comercializada...Substância, 
       pattern = ",", replacement = "") |> as.numeric()

df$Valor.Produção.Comercializada...Produto <- 
  gsub(df$Valor.Produção.Comercializada...Produto, 
       pattern = ",", replacement = "") |> as.numeric()



# subsetting

df <- 
  df[grepl(df$Substância.AMB, 
           pattern = SubstanciaAgrupadora) & 
       df$Ano.Base.Ral >= ano_0,]  





QuantidadeEhValordaProducao <- 
  rbind(QuantidadeEhValordaProducao, df)

rm(df)




# carregamento II ----
df <-
  read.table(file = "./data/CubosDBAMB_QuantidadeEhValordaProducaoMineralComercializada_AGREGADOS_II.csv",
             header = TRUE, sep = ",", quote = "\"", 
             dec = ".", fill = TRUE, row.names = NULL)



# ajuste de colunas numeric
df$Quantidade.Produção.Comercializada...Produto <- 
  gsub(df$Quantidade.Produção.Comercializada...Produto, 
       pattern = ",", replacement = "") |> as.numeric()

df$Quantidade.Produção.Comercializada...Substância <- 
  gsub(df$Quantidade.Produção.Comercializada...Substância, 
       pattern = ",", replacement = "") |> as.numeric()

df$Valor.Produção.Comercializada...Produto <- 
  gsub(df$Valor.Produção.Comercializada...Produto, 
       pattern = ",", replacement = "") |> as.numeric()



# subsetting

df <- 
  df[grepl(df$Substância.AMB, 
           pattern = SubstanciaAgrupadora) & 
       df$Ano.Base.Ral >= ano_0,]  



QuantidadeEhValordaProducao <- 
  rbind(QuantidadeEhValordaProducao, df)

rm(df)


 saveRDS(QuantidadeEhValordaProducao, 
         file = paste0("./data/ETL_QuantidadeEhValordaProducao_", format(Sys.time(), "%Y%m%d%H%M"), ".RDS"))


 # QuantidadeEhValordaProducao <- 
 # QuantidadeEhValordaProducao[QuantidadeEhValordaProducao$CPF.CNPJ.Titular %in% c(
 #   "17.404.930/0001-03",
 #   "19.398.452/0001-00",
 #   "19.974.518/0001-54",
 #   "12.959.442/0001-49",
 #   "03.334.595/0001-00",
 #   "13.132.317/0001-23",
 #   "70.967.971/0001-90",
 #   "16.532.772/0001-04",
 #   "64.210.875/0001-09",
 #   "01.703.219/0001-10",
 #   "41.716.499/0001-15",
 #   "16.652.460/0001-34",
 #   "17.188.590/0001-20",
 #   "18.820.688/0001-11"
 # ), ]
 # 
