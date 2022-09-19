# Fun??es PRODU??O BRUTA ----



FUNA_visao_PRODUCAO_BRUTA <-
  function(Processo = '.',
           CPF.CNPJ.Titular = '.',
           Nome.Mina = '.',
           Substancia.AMB = '.') {
    producaoBRUTA_groupBY_SUBSTANCIA.AMB(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_MINA(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_PROCESSO(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_MUNICIPIO(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_TITULAR(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    a <-
      paste("producaoBRUTA ", paste(Processo, paste(CPF.CNPJ.Titular, paste(Nome.Mina, Substancia.AMB)))) # t?tulo do gr?fico
    producaoBRUTA_GERAL(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>% as.matrix() %>% barplot(main = a)
    # Reserva
    reserva_groupBY_SUBSTANCIA.AMB(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    #eventos
    if (Processo != ".") {
      FUNA_Eventos_RRR_RFP(Processo = Processo) %>% print()
    }
  }


#_____producaoBRUTA_GERAL
producaoBRUTA_GERAL <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Nome.Mina) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Nome.Mina) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Nome.Mina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Nome.Mina) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Municipio.Mina) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Municipio.Mina) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Municipio.Mina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Municipio.Mina) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Processo) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Processo) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Processo) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Processo) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Substancia.AMB) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Substancia.AMB) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.AMB) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.AMB) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                 grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                   grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                     grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo) == TRUE &
                                       grepl(producaoBRUTA$Nome.da.Mina, pattern = Nome.Mina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


# Fun??es PRODU??O BENEFICIADA ----

FUNA_visao_PRODUCAO_BENEFICIADA <-
  function(CPF.CNPJ.Titular = '.',
           usina = '.',
           Substancia.AMB = '.',
           produto = ".",
           Municipio.Mina = ".") {
    producaoBENEFICIADA_groupBY_SUBSTANCIA.AMB(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      usina = usina,
      Municipio.Mina = Municipio.Mina,
      Substancia.AMB = Substancia.AMB,
      #produto = produto
    ) %>%
      print()
    producaoBENEFICIADA_groupBY_USINA(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Substancia.AMB = Substancia.AMB,
      Municipio.Mina = Municipio.Mina,
      usina = usina,
      #produto = produto
    ) %>%
      print()
    #producaoBENEFICIADA_groupBY_PRODUTO(
    #  CPF.CNPJ.Titular = CPF.CNPJ.Titular,
    #  usina = usina,
    #  Substancia.AMB = Substancia.AMB,
    #  Municipio.Mina = Municipio.Mina,
    #  produto = produto
    #) %>%
    #  print()
    producaoBENEFICIADA_groupBY_MUNICIPIO(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      usina = usina,
      Substancia.AMB = Substancia.AMB,
      Municipio.Mina = Municipio.Mina,
      #produto = produto
    ) %>%
      print()
    producaoBENEFICIADA_groupBY_TITULAR(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      usina = usina,
      Substancia.AMB = Substancia.AMB,
      Municipio.Mina = Municipio.Mina,
      #produto = produto
    ) %>%
      print()
    a <-
      paste("producaoBENEFICIADA ", paste(paste(CPF.CNPJ.Titular, paste(usina, Substancia.AMB)))) # t?tulo do gr?fico
    producaoBENEFICIADA_GERAL(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      usina = usina,
      Substancia.AMB = Substancia.AMB,
      Municipio.Mina = Municipio.Mina,
      #produto = produto
    ) %>% as.matrix() %>% barplot(main = a)
    # Reserva
    reserva_groupBY_SUBSTANCIA.AMB(
      Municipio.Mina = Municipio.Mina,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB
    ) %>%
      print()
  }


#_____producaoBENEFICIADA_GERAL
producaoBENEFICIADA_GERAL <-
  function(Substancia.AMB = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           usina = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Nome.da.Usina) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Nome.da.Usina) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Nome.da.Usina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Nome.da.Usina) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Municipio.Mina) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Municipio.Mina) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Municipio.Mina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Municipio.Mina) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
#  function(Substancia.AMB = ".",
#           Substancia.RAL = ".",
#           CPF.CNPJ.Titular = ".",
#           Municipio.Mina = ".",
#           usina = ".",
#           produto = ".",
#           volume = "producao") {
#    if (volume == "producao") {
#      x <-
#        spread(
#          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
#                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
#                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
#                                       grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
#                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#            group_by(Ano.Base.Ral, produto.beneficiado) %>%
#            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
#          key = Ano.Base.Ral,
#          value = soma
#        )
#      
#      return(x)
#      
#    } else {
#      if (volume == "venda") {
#        x <-
#          spread(
#            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
#                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
#                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
#                                         grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
#                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#              group_by(Ano.Base.Ral, produto.beneficiado) %>%
#              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
#            key = Ano.Base.Ral,
#            value = soma
#          )
#        
#        return(x)
#      } else {
#        if (volume == "producao.substancia") {
#          x <-
#            spread(
#              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
#                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
#                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
#                                           grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
#                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#                group_by(Ano.Base.Ral, produto.beneficiado) %>%
#                summarise(soma = sum(
#                  Quantidade.Producao.Substancia.com.Ajuste
#                )),
#              key = Ano.Base.Ral,
#              value = soma
#            )
#          
#          return(x)
#        } else {
#          if (volume == "venda.substancia") {
#            x <-
#              spread(
#                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
#                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
#                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
#                                             grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
#                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
#                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
#                  group_by(Ano.Base.Ral, produto.beneficiado) %>%
#                  summarise(soma = sum(
#                    Quantidade.Venda.com.Ajuste
#                  )),
#                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Substancia.AMB) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Substancia.AMB) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.AMB) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.AMB) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
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
  function(Substancia.AMB = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio.Mina = ".",
           usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Substancia.com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Mina, pattern = Municipio.Mina) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.da.Usina, pattern = Nome.da.Usina) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                  summarise(soma = sum(
                    Quantidade.Venda.com.Ajuste
                  )),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

# Avalia??o de prioridades por crit?rio de Quantis----

FUNA_PRODUCAO_Quantil_SPREAD <-
  function(Substancia.AMB = '.', producao = "bruta", GroupBY = 'usina') {
    if (producao == "bruta") {
      x <-
        spread(
          producaoBRUTA[producaoBRUTA$Processo %in% producaoBRUTA[producaoBRUTA$pareto == 1 &
                                                                    grepl(x = producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB), c('Processo')] &
                          grepl(x = producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB), c('Processo', 'Ano.Base.Ral', 'Quantidade.Producao.Com.Ajuste')] %>%
            group_by(Processo, Ano.Base.Ral) %>% summarise("Producao_BRUTA" = sum(Quantidade.Producao.Com.Ajuste)),
          key = "Ano.Base.Ral",
          value = "Producao_BRUTA",
          fill = NA) 
      return(x)
    } else if (producao == "beneficiada" & GroupBY == 'Municipio.Mina') {
        x <-
          spread(
            producaoBENEFICIADA[producaoBENEFICIADA$id_CPF.CNPJ.Titular.Municipio.Mina %in% producaoBENEFICIADA[producaoBENEFICIADA$pareto == 1 &
                                                                                                    grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB), c('id_CPF.CNPJ.Titular.Municipio.Mina')] &
                                  grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB), c('id_CPF.CNPJ.Titular.Municipio.Mina', 'Ano.Base.Ral', 'Quantidade.Producao.Com.Ajuste')] %>%
              group_by(id_CPF.CNPJ.Titular.Municipio.Mina, Ano.Base.Ral) %>% summarise("Producao_BENEFICIADA" = sum(Quantidade.Producao.Com.Ajuste)),
            key = "Ano.Base.Ral",
            value = "Producao_BENEFICIADA",
            fill = NA) 
        return(x)
      } else {
        x <-
          spread(
            producaoBENEFICIADA[producaoBENEFICIADA$id_CPF.CNPJ.Titular.usina %in% producaoBENEFICIADA[producaoBENEFICIADA$pareto == 1 &
                                                                                                grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB), c('id_CPF.CNPJ.Titular.usina')] &
                                  grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB), c('id_CPF.CNPJ.Titular.usina', 'Ano.Base.Ral', 'Quantidade.Producao.Com.Ajuste')] %>%
              group_by(id_CPF.CNPJ.Titular.usina, Ano.Base.Ral) %>% summarise("Producao_BENEFICIADA" = sum(Quantidade.Producao.Com.Ajuste)),
            key = "Ano.Base.Ral",
            value = "Producao_BENEFICIADA",
            fill = NA) 
        return(x)
      }
    }     