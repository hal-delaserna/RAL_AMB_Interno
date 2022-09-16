rm(list = ls())

library('dplyr') # pipes, select subsetting
library('tidyr') # gather,spread panel datasets
library(sqldf)

install.packages(c('dplyr','tidyr','sqldf'))


## remove acentos
remove_acentos <- function(texto){
  texto <- gsub(pattern = "Á",replacement = "A", texto) %>% gsub(pattern = "Ã",replacement = "A") %>% gsub(pattern = "Â",replacement = "A") %>% gsub(pattern = "É",replacement = "E") %>% gsub(pattern = "Ê",replacement = "E") %>% gsub(pattern = "Í",replacement = "I") %>% gsub(pattern = "Ó",replacement = "O") %>% gsub(pattern = "Õ",replacement = "O") %>% gsub(pattern = "Ô",replacement = "O") %>% gsub(pattern = "Ú",replacement = "U") %>% gsub(pattern = "Ç",replacement = "C") %>%  gsub(pattern = "à",replacement = "a") %>% gsub(pattern = "á",replacement = "a") %>% gsub(pattern = "â",replacement = "a") %>% gsub(pattern = "ã",replacement = "a") %>% gsub(pattern = "é",replacement = "e") %>% gsub(pattern = "ê",replacement = "e") %>% gsub(pattern = "í",replacement = "i") %>% gsub(pattern = "ó",replacement = "o") %>% gsub(pattern = "õ",replacement = "o") %>% gsub(pattern = "ô",replacement = "o") %>% gsub(pattern = "ú",replacement = "u") %>% gsub(pattern = "ç",replacement = "c") %>% gsub(pattern = "'",replacement = " ") %>% gsub(pattern = "-",replacement = " ") %>% gsub(pattern = "  ",replacement = " ")
  return(texto)}

# relatório Geocódigos
geocod <- read.csv.ffdf(file = 'geocod.csv', sep = ';', dec = ',', encoding = 'ANSI')
colnames(geocod) <- c("geocod", "municipio", "regiao_administrativa")

setwd('C:/Users/humberto.serna/Desktop/D_Lake')
#__________________________________________________________________________________

# Relatório movimentacao da producao Bruta ####
areia_movimentacao_producao_bruta <- read.csv('AREIA_SP_mov_pod_bruta.csv',
         stringsAsFactors = FALSE,
         sep = ';', dec = ',', encoding = 'ANSI')
colnames(areia_movimentacao_producao_bruta) <- c("ano", "titular", "processo", "mina", "municipio", "uf", "s.amb", "s.ral", "minerio", "q", "v")
# vetor preco
areia_movimentacao_producao_bruta$preco <-  round(areia_movimentacao_producao_bruta$v/
                                                  areia_movimentacao_producao_bruta$q,2)
# Rm acentos
areia_movimentacao_producao_bruta$municipio <- remove_acentos(areia_movimentacao_producao_bruta$municipio)

# atribuindo RA_adm aos municipios
areia_movimentacao_producao_bruta <- left_join(areia_movimentacao_producao_bruta,geocod, by = 'municipio')


# # Relatório principais_consumidores ####

areia_principais_consumidores <- read.csv('AREIA_SP_principais_consumidores.csv',
                                              stringsAsFactors = FALSE,
                                              sep = ';', dec = ',', encoding = 'ANSI')
colnames(areia_principais_consumidores) <- c("ano", "titular", "municipio", "uf_produtor", "empreendimento", 
                                             "tipo","minerio", "produto", "comprador", "municipio_onsumidor", "uf", "q", "v")
# vetor precos
areia_principais_consumidores$preco <-  round(areia_principais_consumidores$v/
                                                areia_principais_consumidores$q,2)

# Rm acentos
areia_principais_consumidores$municipio <- remove_acentos(areia_principais_consumidores$municipio)
# atribuindo RA aos municipios consumidores
areia_principais_consumidores <- left_join(areia_principais_consumidores,geocod, by = 'municipio')

  
  
  # AVALIAÇÃO E gráficos ####

# boxplot medianas todos os anos, areia, movimentacao_producao_bruta
boxplot(areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2008,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2008,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2009,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2009,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2010,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2010,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2011,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2011,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2012,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2012,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2013,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2013,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2014,][areia_movimentacao_producao_bruta$minerio=="Areia",]$preco,
        areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$minerio=="Areia",]$ano==2014,][areia_movimentacao_producao_bruta$minerio=="Areia",][litoral=="1",]$preco,
        col = c('white','gray','white','gray','white','gray','white','gray','white','gray','white','gray','white','gray'),
        ylim = c(0, 40),na.rm = TRUE
) + title(main = "preco from areia_movimentacao_producao_bruta RA_Santos e Litoral: 2008 à 2014 ")
  

# boxplot medianas todos os anos, areia, movimentacao_producao_bruta
boxplot(areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2008,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2008,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2009,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2009,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2010,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2010,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2011,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2011,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2012,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2012,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2013,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2013,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2014,][areia_principais_consumidores$minerio=="Areia",]$preco,
        areia_principais_consumidores[areia_principais_consumidores[areia_principais_consumidores$minerio=="Areia",]$ano==2014,][areia_principais_consumidores$minerio=="Areia",][litoral=="1",]$preco,
        col = c('white','gray','white','gray','white','gray','white','gray','white','gray','white','gray','white','gray'),
        ylim = c(0, 40),na.rm = TRUE
) + title(main = "preco from areia_principais_consumidores RA_Santos e Litoral: 2008 à 2014 ")



# areia, 2009
plot(density(areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$ano==2009,]$preco,
             na.rm = TRUE),xlim=c(0,100))
plot(density(areia_movimentacao_producao_bruta[areia_movimentacao_producao_bruta$ano==2008,]$preco,
             na.rm = TRUE),xlim=c(0,100))

select(areia_movimentacao_producao_bruta, everything()) %>% 
  filter(minerio == 'Areia' & is.na(preco)==FALSE) %>% 
  View()


# medianas cidades producao bruta
select(areia_movimentacao_producao_bruta, ano, titular, municipio, minerio, q, v, preco, regiao_administrativa) %>% 
  filter(minerio == "Areia") %>% 
  group_by(municipio,ano) %>% 
  summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2)) 
  
# medianas cidades consumidores:   Santos: 35,00 (n=5) 
select(areia_principais_consumidores, ano, titular, municipio, minerio, q, v, preco, regiao_administrativa) %>% 
  filter(minerio == "Areia" & ano == 2009) %>% 
  group_by(municipio) %>%  
  summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2))


# medianas consumidores MINA: Santos: 35,00 (n=5) 
select(areia_principais_consumidores, everything()) %>%
  filter(litoral==1 & tipo=='mina' & minerio == "Areia" & ano == 2009 & municipio=="SANTOS") %>% 
  group_by(municipio) %>% summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2))

  
  

# dada a pequena amostra em cada uma das cidades, usar medianas RA (RA-Santos: 27,00, n=22)

select(areia_principais_consumidores, everything()) %>% 
  filter(minerio == "Areia" & ano == 2009) %>%
  group_by(regiao_administrativa) %>% 
    summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2))

  
# Reunindo relatórios de consumidores ao de produtores
  
consumidores_produtores <-   bind_rows(areia_movimentacao_producao_bruta[,c(1,2,5,9,10,11,12,13,14,15)],
                             areia_principais_consumidores[,c(1,2,10,7,12,13,14,15,16,17)])

# RA-Santos: 27,55, n=26
select(areia_principais_consumidores, everything()) %>% 
  filter(minerio == "Areia" & regiao_administrativa=="RA_Santos" & ano==2009) %>%
  group_by(regiao_administrativa, ano) %>% summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2)) 

  
  
  
  
  
# Usando a mediana de cada empresa que vendeu para RA Santos como unidade amostral

a <- select(areia_principais_consumidores, everything()) %>% 
  filter(minerio == "Areia" & ano==c(2008:2014)) %>% 
  group_by(regiao_administrativa, titular, ano) %>% summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2)) 
colnames(a) <- c("regiao_administrativa","titular", "ano", "preco")

sqldf("select * from areia_principais_consumidores where minerio = 'Areia'")



a <- select(a, everything()) %>% 
  group_by(regiao_administrativa,ano) %>% summarise(round(median(preco, na.rm = TRUE)[median(preco, na.rm = TRUE)>0],2))
colnames(a) <- c("regiao_administrativa", "ano", "preco")


a <- spread(a, key = ano, value = preco) %>% View()


