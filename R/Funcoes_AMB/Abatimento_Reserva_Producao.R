


FUNA_Abatimento_Reserva_Producao <-
  function(processo = ".",
           subsAMB = ".",
           mina = ".",
           cpfcnpj = ".",
           ano1 = '2011') {
    lista <- list()
    lista[[as.integer(ano1)]] <-
      reserva_groupBY_PROCESSO(
        subsAMB = subsAMB,
        cpfcnpj = cpfcnpj,
        processo = processo,
        mina = mina
      )[1, ano1]
    
    for (i in ano1:2017) {
      
      if (as.character(i + 1) %in% colnames(producaoBRUTA_groupBY_PROCESSO(
        subsAMB = subsAMB,
        processo = processo,
        mina = mina,
        cpfcnpj = cpfcnpj)) == FALSE) {
        lista[[i + 1]] <- lista[[i]]
      } else {
        
        lista[[i + 1]] <-
          lista[[i]] - producaoBRUTA_groupBY_PROCESSO(
            subsAMB = subsAMB,
            processo = processo,
            mina = mina,
            cpfcnpj = cpfcnpj
          )[1, as.character(i + 1)]
      }}
    do.call('rbind', lista)
  }

