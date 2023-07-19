
SubsAgr <- producaoBRUTA$Substancia.Agrupadora |> unique() |> sort()

muitas <- # alta ocorrência
  c("Areias Industriais",
    "Argilas",
    "Rochas Ornamentais",
    "Rochas Ornamentais - Outras",
    "Saibro",
    "Ouro")

intermediarias <- 
  c(
    "Ferro",
    "Diamante",
    "Talco e outras Cargas Minerais",
    "Feldspato",
    "Leucita e Nefelina-Sienito",
    "Estanho",
    "Manganes",
    "Dolomito e Magnesita",
    "Nao Informado",
    "Areias Industriais",
    "Caulim",
    "Gipsita",
    "Gemas"
  )


lista <- as.list(NA)
for (i in 1:length(SubsAgr)) {
  
  if (SubsAgr[i] %in% muitas) {
   
   lista[[i]] <- 
    FUNA_PRODUCAO_Quantil_WIDE_Agrupadora(
      Substancia.Agrupadora = SubsAgr[i], probs = 0.9925
      )
  } else if (SubsAgr[i] %in% intermediarias) {
    lista[[i]] <- 
      FUNA_PRODUCAO_Quantil_WIDE_Agrupadora(
        Substancia.Agrupadora = SubsAgr[i], probs = 0.925
      )
  } else {
    
    lista[[i]] <- 
      FUNA_PRODUCAO_Quantil_WIDE_Agrupadora(
        Substancia.Agrupadora = SubsAgr[i], probs = 0.8
      )
  }
  }


P_Bruta <- 
  do.call(what = "rbind", args = lista)




