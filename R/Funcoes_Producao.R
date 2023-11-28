# Funções PRODUÇÃO BRUTA ----



FUNA_visao_PRODUCAO_BRUTA <-
  function(Processo = '.',
           CPF.CNPJ.Titular = '.',
           Nome.Mina = '.',
           Substancia.AMB = '.',
           Substancia.Agrupadora = '.') {
    producaoBRUTA_groupBY_SUBSTANCIA.AMB(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_MINA(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_PROCESSO(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_MUNICIPIO(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    producaoBRUTA_groupBY_TITULAR(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    a <-
      paste("producaoBRUTA ", paste(Processo, paste(CPF.CNPJ.Titular, paste(Nome.Mina, Substancia.AMB)))) # t?tulo do gr?fico
    producaoBRUTA_GERAL(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) #%>% as.matrix() %>% barplot(main = a)
    # Reserva
    reserva_groupBY_SUBSTANCIA.AMB(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
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
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Nome.Mina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Municipio.Mina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Processo) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.AMB) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Com.Ajuste
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBRUTA$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE,], everything()) %>%
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


# Funções PRODUÇÃO BENEFICIADA ----

FUNA_visao_PRODUCAO_BENEFICIADA <-
  function(CPF.CNPJ.Titular = '.',
           Nome.Usina = '.',
           Substancia.AMB = '.',
           Substancia.Agrupadora = ".",
           produto = ".",
           Municipio = ".") {
    producaoBENEFICIADA_groupBY_SUBSTANCIA.AMB(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Usina = Nome.Usina,
      Municipio = Municipio,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
      #produto = produto
    ) %>%
      print()
    producaoBENEFICIADA_groupBY_USINA(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Substancia.AMB = Substancia.AMB,
      Municipio = Municipio,
      Nome.Usina = Nome.Usina,
      Substancia.Agrupadora = Substancia.Agrupadora
      #produto = produto
    ) %>%
      print()
    #producaoBENEFICIADA_groupBY_PRODUTO(
    #  CPF.CNPJ.Titular = CPF.CNPJ.Titular,
    #  Nome.Usina = Nome.Usina,
    #  Substancia.AMB = Substancia.AMB,
    #  Municipio = Municipio,
    #  produto = produto
    #) %>%
    #  print()
    producaoBENEFICIADA_groupBY_MUNICIPIO(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Usina = Nome.Usina,
      Substancia.AMB = Substancia.AMB,
      Municipio = Municipio,
      Substancia.Agrupadora = Substancia.Agrupadora
      #produto = produto
    ) %>%
      print()
    producaoBENEFICIADA_groupBY_TITULAR(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Usina = Nome.Usina,
      Substancia.AMB = Substancia.AMB,
      Municipio = Municipio,
      Substancia.Agrupadora = Substancia.Agrupadora
      #produto = produto
    ) %>%
      print()
    a <-
      paste("producaoBENEFICIADA ", paste(paste(CPF.CNPJ.Titular, paste(Nome.Usina, Substancia.AMB)))) # t?tulo do gr?fico
    producaoBENEFICIADA_GERAL(
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Usina = Nome.Usina,
      Substancia.AMB = Substancia.AMB,
      Municipio = Municipio,
      Substancia.Agrupadora = Substancia.Agrupadora
      #produto = produto
    ) #%>% as.matrix() %>% barplot(main = a)
    # Reserva
    # reserva_groupBY_SUBSTANCIA.AMB(
    #   Municipio = Municipio,
    #   CPF.CNPJ.Titular = CPF.CNPJ.Titular,
    #   Nome.Mina = Nome.Mina,
    #   Substancia.AMB = Substancia.AMB
    # ) %>%
    #   print()
  }


#_____producaoBENEFICIADA_GERAL
producaoBENEFICIADA_GERAL <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Usina = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral) %>%
                summarise(soma = sum(Quantidade.Producao.Substancia.com.Ajuste)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "teor") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise(soma = sum(Contido.Substancia)/sum(Quantidade.Producao.Com.Ajuste)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
          } else {
            if (volume == "contido") {
              x <-
                spread(
                  select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                               grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                               #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                               grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                               grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                               #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                               grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                    group_by(Ano.Base.Ral) %>%
                    summarise(soma = sum(Contido.Substancia)),
                  key = Ano.Base.Ral,
                  value = soma
                )
              
              return(x)
            
            
          }
        }
      }
    }
  }
  }

#_____producaoBENEFICIADA_groupBY_USINA
producaoBENEFICIADA_groupBY_USINA <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Nome.Usina) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Nome.Usina) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "contido") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Nome.Usina) %>%
                summarise(soma = sum(
                  Contido.Substancia
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "teor") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Nome.Usina) %>%
                  summarise(soma = sum(Contido.Substancia)/sum(Quantidade.Producao.Com.Ajuste)),
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
           Substancia.Agrupadora = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Municipio.Usina) %>%
            summarise(soma = sum(Quantidade.Producao.Com.Ajuste)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Municipio.Usina) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "contido") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Municipio.Usina) %>%
                summarise(soma = sum(
                  Contido.Substancia
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "teor") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Municipio.Usina) %>%
                  summarise(soma = sum(Contido.Substancia)/sum(Quantidade.Producao.Com.Ajuste)),
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
#           Municipio = ".",
#           Nome.Usina = ".",
#           produto = ".",
#           volume = "producao") {
#    if (volume == "producao") {
#      x <-
#        spread(
#          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
#                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
#                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
#                                       grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
#                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
#                                       grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
#            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
#                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
#                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
#                                         grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
#                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
#                                         grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
#              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
#                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
#                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
#                                           grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
#                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
#                                           grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
#                group_by(Ano.Base.Ral, produto.beneficiado) %>%
#                summarise(soma = sum(
#                  Quantidade.Producao.Com.Ajuste
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
#                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
#                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
#                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
#                                             grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
#                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
#                                             grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
           Substancia.Agrupadora = Substancia.Agrupadora,
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Substancia.AMB) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "contido") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.AMB) %>%
                summarise(soma = sum(
                  Contido.Substancia
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "teor") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.AMB) %>%
                  summarise(soma = sum(Contido.Substancia)/sum(Quantidade.Producao.Com.Ajuste)),
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
           Substancia.Agrupadora = ".",
           #Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Usina = ".",
           #produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                       grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
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
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                         #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                         grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
              summarise(soma = sum(Quantidade.Venda.com.Ajuste)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "contido") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                           #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                           grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                summarise(soma = sum(
                  Contido.Substancia
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "teor") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$substancia.ral, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Municipio.Usina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                             #grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto, ignore.case = TRUE) == TRUE &
                                             grepl(producaoBENEFICIADA$Nome.Usina, pattern = Nome.Usina, ignore.case = TRUE) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                  summarise(soma = sum(Contido.Substancia)/sum(Quantidade.Producao.Com.Ajuste)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

# Avaliação de prioridades por critério de Quantis----

FUNA_PRODUCAO_Quantil_WIDE <-
  function(Substancia.AMB = Substancia.AMB,
           probs = 0.8, 
           ano_inicial = 2010,
           producao_Bruta = TRUE) {
    
    if (producao_Bruta == TRUE) {
      
      df <- 
        producaoBRUTA[producaoBRUTA$Ano.Base.Ral >= ano_inicial,]
      
      df_a <-
        df[grepl(df$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE),
                      c("Processo","Ano.Base.Ral","CPF.CNPJ.Titular",
                        "id_subs.ano","Substancia.AMB",
                        "Quantidade.Producao.Com.Ajuste")]
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Processo,
            Substancia.AMB,
            id_subs.ano,
            Ano.Base.Ral),
          "Quantidade.Producao.Com.Ajuste" =
            sum(Quantidade.Producao.Com.Ajuste))
      
      df_b <-
        summarise(
          group_by(df_a, id_subs.ano),
          "Quantil" = quantile(
            Quantidade.Producao.Com.Ajuste,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subs.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Processo,
              df_a$Substancia.AMB,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Quantidade.Producao.Com.Ajuste >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Processo",
          "Substancia.AMB",
          "Ano.Base.Ral",
          "Quantidade.Producao.Com.Ajuste"
        )],
        names_from = Ano.Base.Ral,
        values_from = Quantidade.Producao.Com.Ajuste)
      
      return(df_a)
    
    
        
    } else {
      
      df <- 
        producaoBENEFICIADA[producaoBENEFICIADA$Ano.Base.Ral >= ano_inicial,]
      
      df_a <-
        df[grepl(df$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE),
                            c("Municipio.Usina","Ano.Base.Ral","CPF.CNPJ.Titular",
                              "id_subs.ano","Substancia.AMB",
                              "Quantidade.Producao.Com.Ajuste")]
      
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Municipio.Usina,
            Substancia.AMB,
            id_subs.ano,
            Ano.Base.Ral),
          "Quantidade.Producao.Com.Ajuste" =
            sum(Quantidade.Producao.Com.Ajuste))
      
      df_b <-
        summarise(
          group_by(df_a, id_subs.ano),
          "Quantil" = quantile(
            Quantidade.Producao.Com.Ajuste,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subs.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Municipio.Usina,
              df_a$Substancia.AMB,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Quantidade.Producao.Com.Ajuste >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Municipio.Usina",
          "Substancia.AMB",
          "Ano.Base.Ral",
          "Quantidade.Producao.Com.Ajuste"
        )],
        names_from = Ano.Base.Ral,
        values_from = Quantidade.Producao.Com.Ajuste)
      
      return(df_a)
      
      }
  }



FUNA_PRODUCAO_Quantil_WIDE_Agrupadora <-
  function(Substancia.Agrupadora = Substancia.Agrupadora,
           probs = 0.8, 
           ano_inicial = 2010,
           producao_Bruta = TRUE) {
    
    if (producao_Bruta == TRUE) {
      
      df <- 
        producaoBRUTA[producaoBRUTA$Ano.Base.Ral >= ano_inicial,]
      
      df$id_subsAgr.ano <- 
        paste0(df$Substancia.Agrupadora, df$Ano.Base.Ral)
      
      df_a <-
        df[grepl(df$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE),
           c("Processo","Ano.Base.Ral","CPF.CNPJ.Titular",
             "id_subsAgr.ano","Substancia.Agrupadora",
             "Quantidade.Producao.Com.Ajuste")]
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Processo,
            Substancia.Agrupadora,
            id_subsAgr.ano,
            Ano.Base.Ral),
          "Quantidade.Producao.Com.Ajuste" =
            sum(Quantidade.Producao.Com.Ajuste))
      
      df_b <-
        summarise(
          group_by(df_a, id_subsAgr.ano),
          "Quantil" = quantile(
            Quantidade.Producao.Com.Ajuste,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subsAgr.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Processo,
              df_a$Substancia.Agrupadora,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Quantidade.Producao.Com.Ajuste >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Processo",
          "Substancia.Agrupadora",
          "Ano.Base.Ral",
          "Quantidade.Producao.Com.Ajuste"
        )],
        names_from = Ano.Base.Ral,
        values_from = Quantidade.Producao.Com.Ajuste)
      
      return(df_a)
      
      
      
    } else {
      
      df <- 
        producaoBENEFICIADA[producaoBENEFICIADA$Ano.Base.Ral >= ano_inicial,]
      
      df$id_subsAgr.ano <- 
        paste0(df$Substancia.Agrupadora, df$Ano.Base.Ral)
      
      df_a <-
        df[grepl(df$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE),
           c("Municipio.Usina","Ano.Base.Ral","CPF.CNPJ.Titular", "Nome.Usina",
             "id_subsAgr.ano","Substancia.Agrupadora",
             "Quantidade.Producao.Com.Ajuste")]
      
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Municipio.Usina,
            Substancia.Agrupadora,
            id_subsAgr.ano,
            Ano.Base.Ral),
          "Quantidade.Producao.Com.Ajuste" =
            sum(Quantidade.Producao.Com.Ajuste))
      
      df_b <-
        summarise(
          group_by(df_a, id_subsAgr.ano),
          "Quantil" = quantile(
            Quantidade.Producao.Com.Ajuste,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subsAgr.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Municipio.Usina,
              df_a$Substancia.Agrupadora,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Quantidade.Producao.Com.Ajuste >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Municipio.Usina",
          "Substancia.Agrupadora",
          "Ano.Base.Ral",
          "Quantidade.Producao.Com.Ajuste"
        )],
        names_from = Ano.Base.Ral,
        values_from = Quantidade.Producao.Com.Ajuste)
      
      return(df_a)
      
    }
  }




# Avaliação de prioridades por critério de Quantis [VALOR]----

FUNA_VALOR_Quantil_WIDE <-
  function(Substancia.AMB = ".",
           probs = 0.8, 
           ano_inicial = 2010,
           producao_Bruta = TRUE) {
    
    if (producao_Bruta == TRUE) {
      
      producaoBRUTA <- 
        producaoBRUTA[producaoBRUTA$Ano.Base.Ral >= ano_inicial,]
      
      df$id_subsAgr.ano <- 
        paste0(df$Substancia.Agrupadora, df$Ano.Base.Ral)
      
      df_a <-
        producaoBRUTA[grepl(producaoBRUTA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE),
                      c("Processo","Ano.Base.Ral","CPF.CNPJ.Titular",
                        "id_subs.ano","Substancia.AMB",
                        "Valor.Venda.com.Ajuste.por.Minerio")]
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Processo,
            Substancia.AMB,
            id_subs.ano,
            Ano.Base.Ral),
          "Valor.Venda.com.Ajuste.por.Minerio" =
            sum(Valor.Venda.com.Ajuste.por.Minerio))
      
      df_b <-
        summarise(
          group_by(df_a, id_subs.ano),
          "Quantil" = quantile(
            Valor.Venda.com.Ajuste.por.Minerio,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subs.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Processo,
              df_a$Substancia.AMB,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Valor.Venda.com.Ajuste.por.Minerio >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Processo",
          "Substancia.AMB",
          "Ano.Base.Ral",
          "Valor.Venda.com.Ajuste.por.Minerio"
        )],
        names_from = Ano.Base.Ral,
        values_from = Valor.Venda.com.Ajuste.por.Minerio)
      
      return(df_a)
      
      
    } else {
      
      df <- 
        producaoBENEFICIADA[producaoBENEFICIADA$Ano.Base.Ral >= ano_inicial,]
      
      df_a <-
        df[grepl(df$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE),
                            c("Municipio.Usina","Ano.Base.Ral","CPF.CNPJ.Titular",
                              "id_subs.ano","Substancia.AMB",
                              "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor")]
      
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Municipio.Usina,
            Substancia.AMB,
            id_subs.ano,
            Ano.Base.Ral),
          "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor" =
            sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor))
      
      df_b <-
        summarise(
          group_by(df_a, id_subs.ano),
          "Quantil" = quantile(
            Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subs.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Municipio.Usina,
              df_a$Substancia.AMB,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Municipio.Usina",
          "Substancia.AMB",
          "Ano.Base.Ral",
          "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor"
        )],
        names_from = Ano.Base.Ral,
        values_from = Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor)
      
      return(df_a)
      
     
    }
  }



FUNA_VALOR_Quantil_WIDE_Agrupadora <-
  function(Substancia.Agrupadora = ".",
           probs = 0.8, 
           ano_inicial = 2010,
           producao_Bruta = TRUE) {
    
    if (producao_Bruta == TRUE) {
      
      
      df <- 
        producaoBRUTA[producaoBRUTA$Ano.Base.Ral >= ano_inicial,]
      
      df$id_subsAgr.ano <- 
        paste0(df$Substancia.Agrupadora, df$Ano.Base.Ral)
      
      
      df_a <-
        df[grepl(df$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE),
           c("Processo","Ano.Base.Ral","CPF.CNPJ.Titular",
             "id_subsAgr.ano","Substancia.Agrupadora",
             "Valor.Venda.com.Ajuste.por.Minerio")]
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Processo,
            Substancia.Agrupadora,
            id_subsAgr.ano,
            Ano.Base.Ral),
          "Valor.Venda.com.Ajuste.por.Minerio" =
            sum(Valor.Venda.com.Ajuste.por.Minerio))
      
      df_b <-
        summarise(
          group_by(df_a, id_subsAgr.ano),
          "Quantil" = quantile(
            Valor.Venda.com.Ajuste.por.Minerio,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subsAgr.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Processo,
              df_a$Substancia.Agrupadora,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Valor.Venda.com.Ajuste.por.Minerio >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Processo",
          "Substancia.Agrupadora",
          "Ano.Base.Ral",
          "Valor.Venda.com.Ajuste.por.Minerio"
        )],
        names_from = Ano.Base.Ral,
        values_from = Valor.Venda.com.Ajuste.por.Minerio)
      
      return(df_a)
      
      
    } else {
      
      df <- 
        producaoBENEFICIADA[producaoBENEFICIADA$Ano.Base.Ral >= ano_inicial,]
      
      df$id_subsAgr.ano <- 
        paste0(df$Substancia.Agrupadora, df$Ano.Base.Ral)
      
      df_a <-
        df[grepl(df$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE),
           c("Municipio.Usina","Ano.Base.Ral","CPF.CNPJ.Titular",
             "id_subsAgr.ano","Substancia.Agrupadora",
             "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor")]
      
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Municipio.Usina,
            Substancia.Agrupadora,
            id_subsAgr.ano,
            Ano.Base.Ral),
          "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor" =
            sum(Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor))
      
      df_b <-
        summarise(
          group_by(df_a, id_subsAgr.ano),
          "Quantil" = quantile(
            Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subsAgr.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Municipio.Usina,
              df_a$Substancia.Agrupadora,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Municipio.Usina",
          "Substancia.Agrupadora",
          "Ano.Base.Ral",
          "Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor"
        )],
        names_from = Ano.Base.Ral,
        values_from = Valor.Venda.com.Ajuste.por.Produto.Pre.beneficiado...Valor)
      
      return(df_a)
      
      
    }
  }





# # Registros assinalados Pareto == 1 ()
# FUNA_PRODUCAO_Quantil_SPREAD <-
#   function(Substancia.AMB = '.',
#            producao = "Bruta",
#            GroupBY = 'Nome.Usina') {
#     if (producao == "Bruta") {
#       x <-
#         spread(
#           producaoBRUTA[producaoBRUTA$Processo %in% 
#                           producaoBRUTA[producaoBRUTA$pareto == 1 &
#                                           grepl(x = producaoBRUTA$Substancia.AMB, 
#                                                 pattern = Substancia.AMB, ignore.case = TRUE), c('Processo')] &
#                           grepl(x = producaoBRUTA$Substancia.AMB, 
#                                 pattern = Substancia.AMB, ignore.case = TRUE), c('Processo',
#                                                              'Ano.Base.Ral',
#                                                              'Quantidade.Producao.Com.Ajuste')] %>%
#             group_by(Processo, Ano.Base.Ral) %>% summarise("Producao_BRUTA" = sum(Quantidade.Producao.Com.Ajuste)),
#           key = "Ano.Base.Ral",
#           value = "Producao_BRUTA",
#           fill = NA
#         )
#       return(x)
#     } else if (producao == "Beneficiada" &
#                GroupBY == 'Municipio.Mina') {
#       x <-
#         spread(
#           producaoBENEFICIADA[producaoBENEFICIADA$id_cpfcnpj.municipio %in% 
#                                 producaoBENEFICIADA[producaoBENEFICIADA$pareto == 1 &
#                                                       grepl(x = producaoBENEFICIADA$Substancia.AMB, 
#                                                             pattern = Substancia.AMB, ignore.case = TRUE), 
#                                                     c('id_cpfcnpj.municipio')] &
#                                 grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE), c(
#                                   'id_cpfcnpj.municipio',
#                                   'Ano.Base.Ral',
#                                   'Quantidade.Producao.Com.Ajuste'
#                                 )] %>%
#             group_by(id_cpfcnpj.municipio, Ano.Base.Ral) %>% summarise(
#               "Producao_BENEFICIADA" = sum(Quantidade.Producao.Com.Ajuste)
#             ),
#           key = "Ano.Base.Ral",
#           value = "Producao_BENEFICIADA",
#           fill = NA
#         )
#       return(x)
#     } else {
#       x <-
#         spread(
#           producaoBENEFICIADA[producaoBENEFICIADA$id_cpfcnpj.Nome.Usina %in% 
#                                 producaoBENEFICIADA[producaoBENEFICIADA$pareto == 1 &
#                                                       grepl(x = producaoBENEFICIADA$Substancia.AMB, 
#                                                             pattern = Substancia.AMB, ignore.case = TRUE), 
#                                                     c('id_cpfcnpj.Nome.Usina')] &
#                                 grepl(x = producaoBENEFICIADA$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE), c(
#                                   'id_cpfcnpj.Nome.Usina',
#                                   'Ano.Base.Ral',
#                                   'Quantidade.Producao.Com.Ajuste'
#                                 )] %>%
#             group_by(id_cpfcnpj.Nome.Usina, Ano.Base.Ral) %>% summarise(
#               "Producao_BENEFICIADA" = sum(Quantidade.Producao.Com.Ajuste)
#             ),
#           key = "Ano.Base.Ral",
#           value = "Producao_BENEFICIADA",
#           fill = NA
#         )
#       return(x)
#     }
#   }  