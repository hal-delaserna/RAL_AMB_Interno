# Funções Consumidores ----

consumidoresMINA_busca <-
  function(cpfcnpj = ".",
           titular = ".",
           ano = ".",
           mina = ".",
           subsAMB = ".",
           municipio = ".",
           nome.comprador = ".",
           municipio.comprador = ".",
           uso.destinacao = ".") {
    x <-
      select(consumidoresMINA[grepl(consumidoresMINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                grepl(consumidoresMINA$titular, pattern = titular) == TRUE &
                                grepl(consumidoresMINA$ano, pattern = ano) == TRUE &
                                grepl(consumidoresMINA$mina, pattern = mina) == TRUE &
                                grepl(consumidoresMINA$substancias.amb.mina, pattern = subsAMB) == TRUE &
                                grepl(consumidoresMINA$municipio, pattern = municipio) == TRUE &
                                grepl(consumidoresMINA$nome.comprador, pattern = nome.comprador) == TRUE &
                                grepl(consumidoresMINA$municipio.comprador, pattern = municipio.comprador) == TRUE &
                                grepl(consumidoresMINA$uso.destinacao, pattern = uso.destinacao) == TRUE,], everything())
    return(x)
  }




consumidoresUSINA_busca <-
  function(cpfcnpj = ".",
           titular = ".",
           ano = ".",
           usina = ".",
           subsAMB = ".",
           municipio = ".",
           nome.comprador = ".",
           uso.destinacao = ".",
           produto = ".") {
    x <-
      select(consumidoresUSINA[grepl(consumidoresUSINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(consumidoresUSINA$titular, pattern = titular) == TRUE &
                                 grepl(consumidoresUSINA$ano, pattern = ano) == TRUE &
                                 grepl(consumidoresUSINA$usina, pattern = usina) == TRUE &
                                 grepl(consumidoresUSINA$substancias.amb.usina, pattern = subsAMB) == TRUE &
                                 grepl(consumidoresUSINA$municipio, pattern = municipio) == TRUE &
                                 grepl(consumidoresUSINA$nome.comprador, pattern = nome.comprador) == TRUE &
                                 grepl(consumidoresUSINA$uso.destinacao, pattern = uso.destinacao) == TRUE &
                                 grepl(consumidoresUSINA$produto.beneficiado, pattern = produto) == TRUE,], everything())
    return(x)
  }





#_____consumidoresUSINA_preco_MEDIO
consumidoresUSINA_preco_MEDIO <-
  function(produto = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           mediana = "false") {
    if (mediana == "false") {
      x <-
        spread(
          select(consumidoresUSINA[grepl(consumidoresUSINA$produto.beneficiado, pattern = produto) == TRUE &
                                     grepl(consumidoresUSINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(consumidoresUSINA$municipio, pattern = municipio) == TRUE &
                                     grepl(consumidoresUSINA$usina, pattern = usina) == TRUE, ], everything()) %>%
            group_by(ano, produto.beneficiado) %>%
            summarise(preco_medio = mean(preco)),
          key = ano,
          value = preco_medio
        )
      return(x)
    
  } else {
    x <-
      spread(
        select(consumidoresUSINA[grepl(consumidoresUSINA$produto.beneficiado, pattern = produto) == TRUE &
                                   grepl(consumidoresUSINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(consumidoresUSINA$municipio, pattern = municipio) == TRUE &
                                   grepl(consumidoresUSINA$usina, pattern = usina) == TRUE, ], everything()) %>%
          group_by(ano, produto.beneficiado) %>%
          summarise(preco_mediano = median(preco)),
        key = ano,
        value = preco_mediano
      )
    return(x)
  } }




#_____consumidoresMINA_Preco_MEDIO
consumidoresMINA_Preco_MEDIO <-
  function(minerio = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           mediana = "false") {
    if (mediana == "false") {
      x <-
        spread(
          select(consumidoresMINA[grepl(consumidoresMINA$minerio, pattern = minerio) == TRUE &
                                     grepl(consumidoresMINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(consumidoresMINA$municipio, pattern = municipio) == TRUE &
                                     grepl(consumidoresMINA$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, minerio) %>%
            summarise(preco_medio = mean(preco)),
          key = ano,
          value = preco_medio
        )
      return(x)
    } else {
      x <-
        spread(
          select(consumidoresMINA[grepl(consumidoresMINA$minerio, pattern = minerio) == TRUE &
                                     grepl(consumidoresMINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(consumidoresMINA$municipio, pattern = municipio) == TRUE &
                                     grepl(consumidoresMINA$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, minerio) %>%
            summarise(preco_mediano = median(preco)),
          key = ano,
          value = preco_mediano
        )
      return(x)
    }
  }
  