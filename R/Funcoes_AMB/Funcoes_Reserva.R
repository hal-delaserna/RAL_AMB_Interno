# Funções RESERVA ----

FUNA_visao_RESERVA <-
  function(processo = '.',
           cpfcnpj = '.',
           mina = '.',
           subsAMB = '.') {
    reserva_groupBY_SUBSTANCIA.AMB(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_MINA(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_PROCESSO(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_MUNICIPIO(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_TITULAR(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    a <-
      paste("reserva ", paste(processo, paste(cpfcnpj, paste(mina, subsAMB)))) # título do gráfico
    reserva_GERAL(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>% as.matrix() %>% barplot(main = a)
    # prodBruta
    producaoBRUTA_groupBY_SUBSTANCIA.AMB(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    #eventos
    if (processo != ".") {
      FUNA_Eventos_RRR_RFP(processo = processo) %>% print()
    }
  }


#_____reserva_GERAL
reserva_GERAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____reserva_groupBY_MINA
reserva_groupBY_MINA <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, mina) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, mina) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, mina) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, mina) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____reserva_groupBY_MUNICIPIO
reserva_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, municipio) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, municipio) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, municipio) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, municipio) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____reserva_groupBY_PROCESSO
reserva_groupBY_PROCESSO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, processo) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, processo) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, processo) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, processo) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____reserva_groupBY_SUBSTANCIA.AMB
reserva_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, substancia.amb) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, substancia.amb) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, substancia.amb) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, substancia.amb) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____reserva_groupBY_TITULAR
reserva_groupBY_TITULAR <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, cpfcnpj) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, cpfcnpj) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, cpfcnpj) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, cpfcnpj) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



FUNA_Tabela_Pareto_SPREAD <-
  function(subsAMB = '.') {
    x <-
      spread(
        reserva_AMB[reserva_AMB$processo %in% reserva_AMB[reserva_AMB$pareto == 1 &
                                                            grepl(x = reserva_AMB$substancia.amb, pattern = subsAMB), c('processo')] &
                      grepl(x = reserva_AMB$substancia.amb, pattern = subsAMB), c('processo', 'ano', 'massa.medida')] %>%
          group_by(processo, ano) %>% summarise("massa.medida" = sum(massa.medida)),
        key = "ano",
        value = "massa.medida",
        fill = NA
      )
   
    return(x)
  }