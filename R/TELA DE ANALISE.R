#  rm(list = ls()) 

#               ATEN??O: FAZER NOVO DOWNLOAD DAS RESERVAS. N?O CONSTA AREIA INDUSTRIAL
#          CORRIGIR VPM = Vendas + Consumo Pr?prio + Transfer?ncias (p/ transforma??o, tratamento ou consumo)


# carregamento ----
source(file = "./Funcoes_AMB/FUNA_Eventos_RRR_RFP.R")
source(file = "./Funcoes_AMB/Funcoes_Consumidores.R")
source(file = "./Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R")
source(file = "./Funcoes_de_Formatacao_Estilo/graficos_AMB.R")
source(file = "./Funcoes_AMB/Funcoes_Producao.R")
source(file = "./Funcoes_AMB/Funcoes_Reserva.R")
source(file = "./Funcoes_AMB/Abatimento_Reserva_Producao.R")
source(file = "./Funcoes_AMB/Funcoes_VPM.R")
source(file = "./carregamento_Bases_AMB_outras.r") 
# source(file = "./Rastreamento_de_Inconsistencias/Municipios_inconsistencia.R")
#__________________________________________________________________----

# RESERVA ----


 #_____Reserva vis?o ---- 
   mina <- '.'
   cpfcnpj <- '.'
   subsAMB <- 'fosfato'
   processo <- '.'

   FUNA_visao_RESERVA(subsAMB = subsAMB, processo = processo, cpfcnpj = cpfcnpj, mina = mina)

   FUNA_Tabela_Pareto_SPREAD(subsAMB = "dolomito")[,] %>% FUNA_BARPLOT()
   
   #_____ medida + indicada  + Inferida + lavr?vel ----
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'medida')
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'indicada')
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'inferida')
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'lavravel')
   
   #____________ Abatimento - Produ??o Reserva --------------------------------------
   
   processo <- '.'
   mina <- 'vau novo 1'
   cpfcnpj <- '00.255.910/0001-15'
   subsAMB <- 'filito'
   
   FUNA_Abatimento_Reserva_Producao(processo = processo,
                                    subsAMB = subsAMB,
                                    mina = mina,
                                    cpfcnpj = cpfcnpj,
                                    ano1 = '2015')
   
   #__________ Processos que est?o produzindo sem abater a Reserva ----
   
   lista_reservas_iguais <-
      filter(
         reserva_AMB[reserva_AMB$ano > 2010 &
                        reserva_AMB$massa.medida > 10 &
                        !reserva_AMB$substancia.amb %in% c('areia', "brita e cascalho", "saibro"),] %>% 
            group_by(processo, substancia.amb) %>% summarise("reservas_iguais" = max(massa.medida) - mean(massa.medida)),
         reservas_iguais == 0
      )[, c(1, 2)]
   
   lista_reservas_iguais$id <-
      paste(lista_reservas_iguais$processo,
            lista_reservas_iguais$substancia.amb,
            sep = ".")
   
   producaoBRUTA$id <-
      paste(producaoBRUTA$processo, producaoBRUTA$substancia.amb, sep = ".")
   
   reservas_iguais_mas_produzindo <-
      left_join(lista_reservas_iguais, producaoBRUTA[producaoBRUTA$quantidade.producao.ajuste > 0,], by = "id")
   
   reservas_iguais_mas_produzindo[is.na(reservas_iguais_mas_produzindo$quantidade.producao.ajuste) == FALSE, ] %>% View()
   
   
   
   FUNA_Tabela_Pareto_SPREAD <- function(subsAMB = '.') {
      x <-
         spread(reserva_AMB[reserva_AMB$processo %in% reserva_AMB[reserva_AMB$pareto == 1 &
                                                                     reserva_AMB$substancia.amb == subsAMB, c('processo')] &
                               reserva_AMB$massa.medida > 100 &
                               reserva_AMB$substancia.amb == subsAMB, c('processo', 'ano', 'massa.medida')],
                key = "ano",
                value = "massa.medida",
                fill = NA)
      return(x)}
   
   
   
   
   reserva_AMB[reserva_AMB$processo %in% reserva_AMB[reserva_AMB$pareto == 1 &
                                                        reserva_AMB$substancia.amb == subsAMB, c('processo')] &
                  reserva_AMB$substancia.amb == subsAMB, c('processo', 'ano', 'massa.medida')] %>% 
      group_by(processo, ano) %>% summarise("massa.medida" = sum(massa.medida))
   
   
   #__________ Migra??o de Reservas ----
   #__________carregamento
   setwd('./Migracao_de_Reservas')
   source(file = 'Migracao_de_Reservas.R')
   
   lista <- list(1, 2, 3, 4, 5)
   for (i in 2014:2018) {
      lista[i - 2013]  <- list(
         left_join(
            casos_especiais_MIGRACAO[casos_especiais_MIGRACAO$ano == i,
                                     c("ano", "processo", "cpfcnpj", "evento", "Subst?ncias")],
            reserva_AMB[reserva_AMB$ano == i,
                        c("ano", "processo", "cpfcnpj",
                          "substancia.ral", 'substancia.amb', "pareto")],
            by = "processo")) }
   
   checar_migracao <-
      do.call('rbind', lista)
   
   #__________ sem brita, cascalho e saibro
   checar <- 
      checar_migracao[!checar_migracao$substancia.amb %in% c("areia", "saibro", "brita e cascalho"), ]
   
   #__________ Arquivamento   ----
   checar[checar$evento == "ARQUIVAMENTO",] %>% view()    
   # tudo Brita & Cascalho
   
   #__________ processos_Cessao_Total ---- 
   # n?o pode constar 2 reservas/titulares para 1 processo-subst?ncia
   # aperfei?oar: se subst?ncia > 1 , sair? no resultado
   filter(unique(inner_join(
      filter(as.data.frame(sort(
         decreasing = TRUE,
         table(checar[checar$evento == "processos_Cessao_Total", ]$processo)
      )), Freq > 1),
      checar[, c("processo", "pareto")],
      by = c("Var1" = "processo")
   )), pareto == 1) %>% View()
   
   #__________ processos_Cessao_Parcial ---- 
   # a soma da reserva-subst?ncia dos titulares em Sigma A(n+1)i == A(n)
   
   checar[checar$evento == "processos_Cessao_Parcial",] %>% View()
   
   #__________ processos_Desmembramento ---- 
   
   checar[checar$evento == "processos_desmembramento",] %>% View()
   
   #__________ processos_RRR ---- 
   
   checar[checar$evento == "processos_RRR",] %>% View()
   
   # O RRR foi importado?
   
   
   # 001.546/1940 > RRR de 2014 (apresentado em 2017, n?o fora abatida a produ??o)
   # 003.081/1962 > RRR de 2014 
   # 820.061/2001 > argilas comuns; apresentou RRR em 2015
   
   
   
# PRODU??O  ------------------------------------------------------------------------
 #_____Produ??o BRUTA vis?o ----
   c(subsAMB <- 'talco',            cpfcnpj <- '.',          mina <- '.',    usina <- '.',            
     processo <- '.',               minerio <- '.',      municipio <- '.',          produto <- '.')
   #producaoBENEFICIADA[producaoBENEFICIADA$cpfcnpj=='48.277.495/0001-27' & producaoBENEFICIADA$ano == 2016 & producaoBENEFICIADA$substancia.amb == 'bauxita metalurgica',] <- rep("NULL",21)
   
         # BRUTA
 FUNA_visao_PRODUCAO_BRUTA(subsAMB = subsAMB, processo = processo, cpfcnpj = cpfcnpj, mina = mina)
 
            # BENEFICIADA
 FUNA_visao_PRODUCAO_BENEFICIADA(subsAMB = subsAMB, cpfcnpj = cpfcnpj, usina = usina)
 
                # PRODUCAO QUANTIL-WIDE
 FUNA_PRODUCAO_Quantil_SPREAD(subsAMB = 'filito', producao = 'bruta', GroupBY = 'mina') %>% FUNA_BARPLOT()
 
   # RESERVA   
 FUNA_visao_RESERVA(subsAMB = subsAMB, processo = processo, cpfcnpj = cpfcnpj, mina = mina)

#_____CONSUMIDORES  ------------------------------------------------------------------
         #MINA
   consumidoresMINA_busca(cpfcnpj = cpfcnpj, mina = mina)
   
      consumidoresMINA_Preco_MEDIO(minerio = minerio, cpfcnpj = cpfcnpj, 
                                   municipio = municipio, mina = mina, mediana = 'true')
      
         #USINA
   consumidoresUSINA_busca(cpfcnpj = cpfcnpj, subsAMB = subsAMB)
   
      consumidoresUSINA_preco_MEDIO(produto = produto, cpfcnpj = cpfcnpj, 
                                    municipio = municipio, usina = usina, mediana = 'true')
   
   #--------------------- outras fun??es  -----------------------------------------
   # lista processos - pareto
      reserva_AMB[reserva_AMB$pareto == 1 &
                     reserva_AMB$ano > 2010 &
                     !reserva_AMB$substancia.amb %in% c('areia', "brita e cascalho", "saibro"), 
                  c(c("processo", "substancia.amb"))] %>% unique() %>% arrange(substancia.amb) %>% View()
   
   # fun??o busca pareto
   FUNA_Tabela_Pareto_SPREAD <- function(subsAMB = '.') {
      x <-
         spread(
            reserva_AMB[reserva_AMB$processo %in% reserva_AMB[reserva_AMB$pareto == 1 &
                                                                 reserva_AMB$substancia.amb == subsAMB, c('processo')] &
                           reserva_AMB$substancia.amb == subsAMB, c('processo', 'ano', 'massa.medida')] %>%
               group_by(processo, ano) %>% summarise("massa.medida" = sum(massa.medida)),
            key = "ano",
            value = "massa.medida",
            fill = NA
         )
      return(x)
   }
      
#__________ n processos ----
  
   arrange(left_join(
      summarise(
         group_by(reserva_AMB, substancia.amb),
         "Processos_Reserva" = length(unique(processo))
      ),
      summarise(
         group_by(producaoBRUTA, substancia.amb),
         "Processos_ProdBruta" = length(unique(processo))
      ),
      by = "substancia.amb"
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
#   caulim            			26	# 59    no processo, ver RRR do 804.239/1968; 012.622/1935 -> manifesto de mina com "tur/caulim/feld/mica". Ver reservas no Proc
   filito			            31	# 59
   ornamental (granito, gnaisse e afins) 			24	# 85      820.191/1993 divide-se em: Ornamental(17,64%) ou Brita (82,36%). Checar a publica??o final.
   areia industrial			   33	 108
   saibro			            76	# 153
   calcario			            114	 222
   brita e cascalho			   433	# 799
   argilas comuns			      567	 1193
   areia			               1222	# 2254
   
   
   
   
   
   producaoBENEFICIADA[grepl(producaoBENEFICIADA$usina, pattern = "automatica"), ]$id_cpfcnpj.usina <-
      str_remove(string = producaoBENEFICIADA[grepl(producaoBENEFICIADA$usina, pattern = "automatica"), ]$id_cpfcnpj.usina, pattern = "...........$")
   
   spread(producaoBENEFICIADA[grepl(producaoBENEFICIADA$usina, pattern = "automatica"), 
                              c('ano',
                                "id_cpfcnpj.usina",
                                "quantidade.producao.substancia.ajuste")], 
          key = 'ano', 
          value = quantidade.producao.substancia.ajuste) %>% View()
   
   
   

   
   
   
   reserva_AMB[reserva_AMB$substancia.amb %in% c('fosfato', 'ferro', 'bauxita'),] %>% View()
   
   
   
   
   
   
   Filito: 
      Foi criada usina p/ filito 2015-2018
   
   
   
   