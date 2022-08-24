# Funções PRODUÇÃO BRUTA ----



FUNA_visao_PRODUCAO_BRUTA <-
  function(processo = '.',
           cpfcnpj = '.',
           mina = '.',
           subsAMB = '.') {
    producaoBRUTA_groupBY_SUBSTANCIA.AMB(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_MINA(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_PROCESSO(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_MUNICIPIO(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_TITULAR(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    a <-
      paste("producaoBRUTA ", paste(processo, paste(cpfcnpj, paste(mina, subsAMB)))) # título do gráfico
    producaoBRUTA_GERAL(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>% as.matrix() %>% barplot(main = a)
    # Reserva
    reserva_groupBY_SUBSTANCIA.AMB(
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


#_____producaoBRUTA_GERAL
producaoBRUTA_GERAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(Ano.Base) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(Ano.Base) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(Ano.Base) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____producaoBRUTA_groupBY_MINA
producaoBRUTA_groupBY_MINA <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(Ano.Base, mina) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(Ano.Base, mina) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(Ano.Base, mina) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, mina) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBRUTA_groupBY_MUNICIPIO
producaoBRUTA_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(Ano.Base, municipio) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(Ano.Base, municipio) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(Ano.Base, municipio) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, municipio) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____producaoBRUTA_groupBY_PROCESSO
producaoBRUTA_groupBY_PROCESSO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(Ano.Base, processo) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(Ano.Base, processo) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(Ano.Base, processo) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, processo) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBRUTA_groupBY_SUBSTANCIA.AMB
producaoBRUTA_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(Ano.Base, Substancia.AMB) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(Ano.Base, Substancia.AMB) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(Ano.Base, Substancia.AMB) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, Substancia.AMB) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBRUTA_groupBY_TITULAR
producaoBRUTA_groupBY_TITULAR <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(Ano.Base, cpfcnpj) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(Ano.Base, cpfcnpj) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(Ano.Base, cpfcnpj) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$Municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, cpfcnpj) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


# Funções PRODUÇÃO BENEFICIADA ----

FUNA_visao_PRODUCAO_BENEFICIADA <-
  function(cpfcnpj = '.',
           usina = '.',
           subsAMB = '.',
           produto = ".",
           municipio = ".") {
    producaoBENEFICIADA_groupBY_SUBSTANCIA.AMB(
      cpfcnpj = cpfcnpj,
      usina = usina,
      municipio = municipio,
      subsAMB = subsAMB,
      #produto = produto
    ) %>%
      print()
    producaoBENEFICIADA_groupBY_USINA(
      cpfcnpj = cpfcnpj,
      subsAMB = subsAMB,
      municipio = municipio,
      usina = usina,
      #produto = produto
    ) %>%
      print()
    #producaoBENEFICIADA_groupBY_PRODUTO(
    #  cpfcnpj = cpfcnpj,
    #  usina = usina,
    #  subsAMB = subsAMB,
    #  municipio = municipio,
    #  produto = produto
    #) %>%
    #  print()
    producaoBENEFICIADA_groupBY_MUNICIPIO(
      cpfcnpj = cpfcnpj,
      usina = usina,
      subsAMB = subsAMB,
      municipio = municipio,
      #produto = produto
    ) %>%
      print()
    producaoBENEFICIADA_groupBY_TITULAR(
      cpfcnpj = cpfcnpj,
      usina = usina,
      subsAMB = subsAMB,
      municipio = municipio,
      #produto = produto
    ) %>%
      print()
    a <-
      paste("producaoBENEFICIADA ", paste(paste(cpfcnpj, paste(usina, subsAMB)))) # título do gráfico
    producaoBENEFICIADA_GERAL(
      cpfcnpj = cpfcnpj,
      usina = usina,
      subsAMB = subsAMB,
      municipio = municipio,
      #produto = produto
    ) %>% as.matrix() %>% barplot(main = a)
    # Reserva
    reserva_groupBY_SUBSTANCIA.AMB(
      municipio = municipio,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      print()
  }


#_____producaoBENEFICIADA_GERAL
producaoBENEFICIADA_GERAL <-
  function(subsAMB = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____producaoBENEFICIADA_groupBY_USINA
producaoBENEFICIADA_groupBY_USINA <-
  function(subsAMB = ".",
           #subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base, Nome.da.Usina) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base, Nome.da.Usina) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base, Nome.da.Usina) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, Nome.da.Usina) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBENEFICIADA_groupBY_MUNICIPIO
producaoBENEFICIADA_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           #subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base, municipio) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base, municipio) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base, municipio) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, municipio) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


##_____producaoBENEFICIADA_groupBY_PRODUTO
#producaoBENEFICIADA_groupBY_PRODUTO <-
#  function(subsAMB = ".",
#           subsRAL = ".",
#           cpfcnpj = ".",
#           municipio = ".",
#           usina = ".",
#           produto = ".",
#           volume = "producao") {
#    if (volume == "producao") {
#      x <-
#        spread(
#          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
#                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
#                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
#                                       grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
#                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#            group_by(Ano.Base, produto.beneficiado) %>%
#            summarise(soma = sum(quantidade.producao.ajuste)),
#          key = ano,
#          value = soma
#        )
#      
#      return(x)
#      
#    } else {
#      if (volume == "venda") {
#        x <-
#          spread(
#            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
#                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
#                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
#                                         grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
#                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#              group_by(Ano.Base, produto.beneficiado) %>%
#              summarise(soma = sum(quantidade.venda.ajuste)),
#            key = ano,
#            value = soma
#          )
#        
#        return(x)
#      } else {
#        if (volume == "producao.substancia") {
#          x <-
#            spread(
#              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
#                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
#                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
#                                           grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
#                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#                group_by(Ano.Base, produto.beneficiado) %>%
#                summarise(soma = sum(
#                  quantidade.producao.substancia.ajuste
#                )),
#              key = ano,
#              value = soma
#            )
#          
#          return(x)
#        } else {
#          if (volume == "venda.substancia") {
#            x <-
#              spread(
#                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
#                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
#                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
#                                             grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
#                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#                  group_by(Ano.Base, produto.beneficiado) %>%
#                  summarise(soma = sum(
#                    quantidade.venda.substancia.ajuste
#                  )),
#                key = ano,
#                value = soma
#              )
#            
#            return(x)
#            
#          }
#        }
#      }
#    }
#  }



#_____producaoBENEFICIADA_groupBY_SUBSTANCIA.AMB
producaoBENEFICIADA_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           #subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base, Substancia.AMB) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base, Substancia.AMB) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base, Substancia.AMB) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, Substancia.AMB) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBENEFICIADA_groupBY_TITULAR
producaoBENEFICIADA_groupBY_TITULAR <-
  function(subsAMB = ".",
           #subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base, cpfcnpj) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base, cpfcnpj) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base, cpfcnpj) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio, pattern = municipio) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base, cpfcnpj) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

# Avaliação de prioridades por critério de Quantis----

FUNA_PRODUCAO_Quantil_SPREAD <-
  function(subsAMB = '.', producao = "bruta", GroupBY = 'usina') {
    if (producao == "bruta") {
      x <-
        spread(
          producaoBRUTA[producaoBRUTA$Processo %in% producaoBRUTA[producaoBRUTA$pareto == 1 &
                                                                    grepl(x = producaoBRUTA$Substancia.AMB, pattern = subsAMB), c('processo')] &
                          grepl(x = producaoBRUTA$Substancia.AMB, pattern = subsAMB), c('processo', 'ano', 'quantidade.producao.ajuste')] %>%
            group_by(processo, ano) %>% summarise("Producao_BRUTA" = sum(quantidade.producao.ajuste)),
          key = "Ano.Base",
          value = "Producao_BRUTA",
          fill = NA) 
      return(x)
    } else if (producao == "beneficiada" & GroupBY == 'municipio') {
        x <-
          spread(
            producaoBENEFICIADA[producaoBENEFICIADA$id_cpfcnpj.municipio %in% producaoBENEFICIADA[producaoBENEFICIADA$pareto == 1 &
                                                                                                    grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB), c('id_cpfcnpj.municipio')] &
                                  grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB), c('id_cpfcnpj.municipio', 'ano', 'quantidade.producao.ajuste')] %>%
              group_by(id_cpfcnpj.municipio, ano) %>% summarise("Producao_BENEFICIADA" = sum(quantidade.producao.ajuste)),
            key = "Ano.Base",
            value = "Producao_BENEFICIADA",
            fill = NA) 
        return(x)
      } else {
        x <-
          spread(
            producaoBENEFICIADA[producaoBENEFICIADA$id_cpfcnpj.usina %in% producaoBENEFICIADA[producaoBENEFICIADA$pareto == 1 &
                                                                                                grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB), c('id_cpfcnpj.usina')] &
                                  grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = subsAMB), c('id_cpfcnpj.usina', 'ano', 'quantidade.producao.ajuste')] %>%
              group_by(id_cpfcnpj.usina, ano) %>% summarise("Producao_BENEFICIADA" = sum(quantidade.producao.ajuste)),
            key = "Ano.Base",
            value = "Producao_BENEFICIADA",
            fill = NA) 
        return(x)
      }
    }     