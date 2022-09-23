#  rm(list = ls())

# carregamento ----
source(file = "./R/geocod.R")
source(file = "./R/FUNA_Eventos_RRR_RFP.R", encoding = "UTF-8")
source(file = "./R/Funcoes_Consumidores.R")
source(file = "./R/Funcoes_de_Formatacao_Estilo.R", encoding = "UTF-8")
source(file = "./R/graficos_AMB.R")
source(file = "./R/Funcoes_Producao.R", encoding = "UTF-8")
source(file = "./R/Funcoes_Reserva.R", encoding = "UTF-8")
source(file = "./R/Abatimento_Reserva_Producao.R", encoding = "UTF-8")
#     CORRIGIR FUNÇÕES VPM = Vendas + Consumo Pr?prio + Transfer?ncias (p/ transforma??o, tratamento ou consumo)
source(file = "./R/Funcoes_VPM.R", encoding = "UTF-8")
source(file = "./R/carregamento_Bases_AMB_outras.r", encoding = "UTF-8")
# source(file = "./Rastreamento_de_Inconsistencias/Municipios_inconsistencia.R")
#__________________________________________________________________----

# RESERVA ----

 #_____Reserva visão ---- 
   Nome.Mina <- '.'
   CPF.CNPJ.Titular <- '01.637.895/0106-00'
   Substancia.AMB <- '.'
   Processo <- '.'

   FUNA_visao_RESERVA(Substancia.AMB = Substancia.AMB, Processo = Processo, CPF.CNPJ.Titular = CPF.CNPJ.Titular, Nome.Mina = Nome.Mina)

   FUNA_Tabela_Pareto_SPREAD(Substancia.AMB = Substancia.AMB)[,] %>% FUNA_BARPLOT()
   
   #_____ medida + indicada  + Inferida + lavrável ----
   reserva_groupBY_SUBSTANCIA.AMB(Processo = Processo, Nome.Mina = Nome.Mina, reserva = 'medida')
   reserva_groupBY_SUBSTANCIA.AMB(Processo = Processo, Nome.Mina = Nome.Mina, reserva = 'indicada')
   reserva_groupBY_SUBSTANCIA.AMB(Processo = Processo, Nome.Mina = Nome.Mina, reserva = 'inferida')
   # reserva_groupBY_SUBSTANCIA.AMB(Processo = Processo, Nome.Mina = Nome.Mina, reserva = 'lavravel')
   
   #____________ Abatimento - Produção Reserva --------------------------------------
   
   Processo <- '.'
   Nome.Mina <- 'vau novo 1'
   CPF.CNPJ.Titular <- '00.255.910/0001-15'
   Substancia.AMB <- 'filito'
   
   FUNA_Abatimento_Reserva_Producao(Processo = Processo,
                                    Substancia.AMB = Substancia.AMB,
                                    Nome.Mina = Nome.Mina,
                                    CPF.CNPJ.Titular = CPF.CNPJ.Titular,
                                    ano1 = '2015')
   
   #__________ Processos que est?o produzindo sem abater a Reserva ----
   
   lista_reservas_iguais <-
      filter(
         reserva_AMB[reserva_AMB$Ano.Base.Ral > 2010 &
                        reserva_AMB$Massa.Medida > 10 &
                        !reserva_AMB$Substancia.AMB %in% c('areia', "brita e cascalho", "saibro"),] %>% 
            group_by(Processo, Substancia.AMB) %>% summarise("reservas_iguais" = max(Massa.Medida) - mean(Massa.Medida)),
         reservas_iguais == 0
      )[, c(1, 2)]
   
   lista_reservas_iguais$id <-
      paste(lista_reservas_iguais$Processo,
            lista_reservas_iguais$Substancia.AMB,
            sep = ".")
   
   producaoBRUTA$id <-
      paste(producaoBRUTA$Processo, producaoBRUTA$Substancia.AMB, sep = ".")
   
   reservas_iguais_mas_produzindo <-
      left_join(lista_reservas_iguais, producaoBRUTA[producaoBRUTA$quantidade.producao.ajuste > 0,], by = "id")
   
   reservas_iguais_mas_produzindo[is.na(reservas_iguais_mas_produzindo$quantidade.producao.ajuste) == FALSE, ] %>% View()
   
   

   reserva_AMB[reserva_AMB$Processo %in% reserva_AMB[reserva_AMB$pareto == 1 &
                                                        reserva_AMB$Substancia.AMB == Substancia.AMB, c('Processo')] &
                  reserva_AMB$Substancia.AMB == Substancia.AMB, c('Processo', 'Ano.Base.Ral', 'Massa.Medida')] %>% 
      group_by(Processo, Ano.Base.Ral) %>% summarise("Massa.Medida" = sum(Massa.Medida))
   
   
   #__________ Migra??o de Reservas ----
   #__________carregamento
   setwd('./Migracao_de_Reservas')
   source(file = 'Migracao_de_Reservas.R')
   
   lista <- list(1, 2, 3, 4, 5)
   for (i in 2014:2018) {
      lista[i - 2013]  <- list(
         left_join(
            casos_especiais_MIGRACAO[casos_especiais_MIGRACAO$Ano.Base.Ral == i,
                                     c("Ano.Base.Ral", "Processo", "CPF.CNPJ.Titular", "evento", "Subst?ncias")],
            reserva_AMB[reserva_AMB$Ano.Base.Ral == i,
                        c("Ano.Base.Ral", "Processo", "CPF.CNPJ.Titular",
                          "substancia.ral", 'Substancia.AMB', "pareto")],
            by = "Processo")) }
   
   checar_migracao <-
      do.call('rbind', lista)
   
   #__________ sem brita, cascalho e saibro
   checar <- 
      checar_migracao[!checar_migracao$Substancia.AMB %in% c("areia", "saibro", "brita e cascalho"), ]
   
   #__________ Arquivamento   ----
   checar[checar$evento == "ARQUIVAMENTO",] %>% view()    
   # tudo Brita & Cascalho
   
   #__________ Processos_Cessao_Total ---- 
   # n?o pode constar 2 reservas/titulares para 1 Processo-subst?ncia
   # aperfei?oar: se subst?ncia > 1 , sair? no resultado
   filter(unique(inner_join(
      filter(as.data.frame(sort(
         decreasing = TRUE,
         table(checar[checar$evento == "Processos_Cessao_Total", ]$Processo)
      )), Freq > 1),
      checar[, c("Processo", "pareto")],
      by = c("Var1" = "Processo")
   )), pareto == 1) %>% View()
   
   #__________ Processos_Cessao_Parcial ---- 
   # a soma da reserva-subst?ncia dos titulares em Sigma A(n+1)i == A(n)
   
   checar[checar$evento == "Processos_Cessao_Parcial",] %>% View()
   
   #__________ Processos_Desmembramento ---- 
   
   checar[checar$evento == "Processos_desmembramento",] %>% View()
   
   #__________ Processos_RRR ---- 
   
   checar[checar$evento == "Processos_RRR",] %>% View()
   
   # O RRR foi importado?
   
   
   # 001.546/1940 > RRR de 2014 (apresentado em 2017, n?o fora abatida a Produção)
   # 003.081/1962 > RRR de 2014 
   # 820.061/2001 > argilas comuns; apresentou RRR em 2015
   
   
   
   
# PRODUÇÃO  ------------------------------------------------------------------------

   
     Substancia.AMB <- '.'
     CPF.CNPJ.Titular <- '38.282.487/0001-15'
     Municipio <- '.'
     Nome.Mina <- '.'
     Nome.Usina <- '.'
     Processo <- '.'
     Minerio <- '.'

     
#_____Produção BRUTA visão ----

 FUNA_visao_PRODUCAO_BRUTA(Substancia.AMB = Substancia.AMB, Processo = Processo, CPF.CNPJ.Titular = CPF.CNPJ.Titular, Nome.Mina = Nome.Mina)
 
#_____Produção BENEFICIADA visão ----
 FUNA_visao_PRODUCAO_BENEFICIADA(Substancia.AMB = Substancia.AMB, CPF.CNPJ.Titular = CPF.CNPJ.Titular, Nome.Usina = Nome.Usina)
 
#producaoBENEFICIADA[producaoBENEFICIADA$CPF.CNPJ.Titular=='48.277.495/0001-27' & producaoBENEFICIADA$Ano.Base.Ral == 2016 & producaoBENEFICIADA$Substancia.AMB == 'bauxita metalurgica',] <- rep("NULL",21)
 
 
      # PRODUCAO QUANTIL-WIDE
 # FUNA_PRODUCAO_Quantil_SPREAD(Substancia.AMB = 'filito', producao = 'bruta', GroupBY = 'Nome.Mina') %>% FUNA_BARPLOT()
 
 
 Substancia.AMB <- "Calcario"
 probs <- 0.8
 
 FUNA_PRODUCAO_Quantil_WIDE(Substancia.AMB = Substancia.AMB, probs = probs)

 
#_____CONSUMIDORES  ------------------------------------------------------------------
         #MINA
   consumidoresMINA_busca(CPF.CNPJ.Titular = CPF.CNPJ.Titular, Nome.Mina = Nome.Mina)
   
      consumidoresMINA_Preco_MEDIO(minerio = minerio, CPF.CNPJ.Titular = CPF.CNPJ.Titular, 
                                   municipio = municipio, Nome.Mina = Nome.Mina, mediana = 'true')
      
         #USINA
   consumidoresUSINA_busca(CPF.CNPJ.Titular = CPF.CNPJ.Titular, Substancia.AMB = Substancia.AMB)
   
      consumidoresUSINA_preco_MEDIO(produto = produto, CPF.CNPJ.Titular = CPF.CNPJ.Titular, 
                                    municipio = municipio, usina = usina, mediana = 'true')
   
   #--------------------- outras fun??es  -----------------------------------------
   # lista Processos - pareto
      reserva_AMB[reserva_AMB$pareto == 1 &
                    reserva_AMB$Ano.Base.Ral > 2010 &
                    !reserva_AMB$Substancia.AMB %in% 
                    c('Calcário'),
                  c(c("Processo", "Substancia.AMB"))] %>% 
        unique() %>% arrange(Substancia.AMB) %>% View()
      
   # função busca pareto
      FUNA_Tabela_Pareto_SPREAD <- function(Substancia.AMB = '.') {
        x <-
          spread(
            reserva_AMB[reserva_AMB$Processo %in% 
                          reserva_AMB[reserva_AMB$pareto == 1 &
                                        reserva_AMB$Substancia.AMB == Substancia.AMB, c('Processo')] &
                          reserva_AMB$Substancia.AMB == Substancia.AMB, c('Processo', 'Ano.Base.Ral', 'Massa.Medida')] %>%
              group_by(Processo, Ano.Base.Ral) %>% summarise("Massa.Medida" = sum(Massa.Medida)),
            key = "Ano.Base.Ral",
            value = "Massa.Medida",
            fill = NA
          )
        return(x)
      }
      
#__________ n Processos ----
  
   arrange(left_join(
      summarise(
         group_by(reserva_AMB, Substancia.AMB),
         "Processos_Reserva" = length(unique(Processo))
      ),
      summarise(
         group_by(producaoBRUTA, Substancia.AMB),
         "Processos_ProdBruta" = length(unique(Processo))
      ),
      by = "Substancia.AMB"
   ),
   Processos_Reserva) %>%  clipr::write_clip()
   
# Subst?ncias Lista Pareto (Q de Processos)
#   cobre			0	# 1
#   diatomita			0	# 1
#   fluorita			0	# 1
#   ilmenita			0	# 1
#   manganes			1	# 1
#   mica			0	# 1
#   terras raras			0	# 1
#   calcita			0	# 2
#   cassiterita (primaria)			2	# 2
#   potassio			0	# 2
#   prata (primaria)			0	# 2
#   quartzito ornamental			0	# 2
#   tungstenio			0	# 2
#   leucita e nefelina sienito			1	# 3
#   ornamental (marmores e afins)			0	# 3
#   turmalina industrial			3	# 3
#   ardosia			0	# 4
#   arenito ornamental			6	# 6
   fosfato			            6	 7
#   talco			3	# 7
#   bentonita e argilas descorantes			8	# 8
   ferro			               5	 8
#   outras rochas ornamentais (pedra de talhe, pedra sabao, basalto, etc.)			9	# 11
#   quartzo			2	# 12
   bauxita refrataria			4	 19
   bauxita metalurgica			16	 26
   quartzito industrial			14	# 31       problema guararema
#   feldspato			         9	# 34
#   turfa			               13	# 40
   dolomito			            22	# 43
   argilas plasticas			   15	# 44
   argilas refratarias			19	# 56
#   caulim            			26	# 59    no Processo, ver RRR do 804.239/1968; 012.622/1935 -> manifesto de Nome.Mina com "tur/caulim/feld/mica". Ver reservas no Proc
   filito			            31	# 59
   ornamental (granito, gnaisse e afins) 			24	# 85      820.191/1993 divide-se em: Ornamental(17,64%) ou Brita (82,36%). Checar a publica??o final.
   areia industrial			   33	 108
   saibro			            76	# 153
   calcario			            114	 222
   brita e cascalho			   433	# 799
   argilas comuns			      567	 1193
   areia			               1222	# 2254
   
   
   
   
   
   producaoBENEFICIADA[grepl(producaoBENEFICIADA$usina, pattern = "automatica"), ]$id_CPF.CNPJ.Titular.usina <-
      str_remove(string = producaoBENEFICIADA[grepl(producaoBENEFICIADA$usina, pattern = "automatica"), ]$id_CPF.CNPJ.Titular.usina, pattern = "...........$")
   
   spread(producaoBENEFICIADA[grepl(producaoBENEFICIADA$usina, pattern = "automatica"), 
                              c('Ano.Base.Ral',
                                "id_CPF.CNPJ.Titular.usina",
                                "quantidade.producao.substancia.ajuste")], 
          key = 'Ano.Base.Ral', 
          value = quantidade.producao.substancia.ajuste) %>% View()
   
   
   

   
   
   
   reserva_AMB[reserva_AMB$Substancia.AMB %in% c('fosfato', 'ferro', 'bauxita'),] %>% View()
   
   
   
   
   
   
   Filito: 
      Foi criada usina p/ filito 2015-2018
   
   
   
   