# Funções RESERVA ----

FUNA_visao_RESERVA <-
  function(Processo = '.',
           CPF.CNPJ.Titular = '.',
           Nome.Mina = '.',
           Substancia.AMB = '.',
           Substancia.Agrupadora = '.') {
    print("reserva_groupBY_SUBSTANCIA.AMB:")
    reserva_groupBY_SUBSTANCIA.AMB(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    print("reserva_groupBY_MINA:")
    reserva_groupBY_MINA(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    print("reserva_groupBY_PROCESSO:")
    reserva_groupBY_PROCESSO(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    print("reserva_groupBY_MUNICIPIO:")
    reserva_groupBY_MUNICIPIO(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    print("reserva_groupBY_TITULAR:")
    reserva_groupBY_TITULAR(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    a <-
      paste("reserva ", paste(Processo, 
                              paste(CPF.CNPJ.Titular, 
                                    paste(Nome.Mina, Substancia.AMB)))) # t?tulo do gr?fico
    
    reserva_GERAL(
      Processo = Processo,
      CPF.CNPJ.Titular = CPF.CNPJ.Titular,
      Nome.Mina = Nome.Mina,
      Substancia.AMB = Substancia.AMB,
      Substancia.Agrupadora = Substancia.Agrupadora
    ) %>% as.matrix() %>% barplot(main = a)
    # prodBruta
    print("producaoBRUTA_groupBY_SUBSTANCIA.AMB:")
    producaoBRUTA_groupBY_SUBSTANCIA.AMB(
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


#_____reserva_GERAL
reserva_GERAL <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
            group_by(Ano.Base.Ral) %>%
            summarise(soma = sum(Massa.Medida)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
              group_by(Ano.Base.Ral) %>%
              summarise(soma = sum(Massa.Indicada)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                group_by(Ano.Base.Ral) %>%
                summarise(soma = sum(Massa.Inferida)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise(soma = sum(Massa.Lavravel)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          } else if (reserva == "contido") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise(soma = sum(Contido.Medido)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
          } else if (reserva == "teor") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise('teor' = teor_medido),
                key = Ano.Base.Ral,
                value = teor
              )
            
            return(x)
          }
        }
      }
    }
    }


#_____reserva_groupBY_MINA
reserva_groupBY_MINA <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
            group_by(Ano.Base.Ral, Nome.Mina) %>%
            summarise(soma = sum(Massa.Medida)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
              group_by(Ano.Base.Ral, Nome.Mina) %>%
              summarise(soma = sum(Massa.Indicada)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                group_by(Ano.Base.Ral, Nome.Mina) %>%
                summarise(soma = sum(Massa.Inferida)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Nome.Mina) %>%
                  summarise(soma = sum(Massa.Lavravel)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          } else {
          if (reserva == "contido") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Nome.Mina) %>%
                  summarise(soma = sum(Contido.Medido)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
          } else if (reserva == "teor") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Nome.Mina) %>%
                  summarise('teor' = teor_medido),
                key = Ano.Base.Ral,
                value = teor
              )
            
            return(x)
          }}}}}}

#_____reserva_groupBY_MUNICIPIO
reserva_groupBY_MUNICIPIO <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
            group_by(Ano.Base.Ral, Municipio.Mina) %>%
            summarise(soma = sum(Massa.Medida)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
              group_by(Ano.Base.Ral, Municipio.Mina) %>%
              summarise(soma = sum(Massa.Indicada)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                group_by(Ano.Base.Ral, Municipio.Mina) %>%
                summarise(soma = sum(Massa.Inferida)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Municipio.Mina) %>%
                  summarise(soma = sum(Massa.Lavravel)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          }  else {
            if (reserva == "contido") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, Municipio.Mina) %>%
                    summarise(soma = sum(Contido.Medido)),
                  key = Ano.Base.Ral,
                  value = soma
                )
              
              return(x)
            } else if (reserva == "teor") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, Municipio.Mina) %>%
                    summarise('teor' = teor_medido),
                  key = Ano.Base.Ral,
                  value = teor
                )
              
              return(x)
              
            }
          }
        }
      }
    }
  }

#_____reserva_groupBY_PROCESSO
reserva_groupBY_PROCESSO <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
            group_by(Ano.Base.Ral, Processo) %>%
            summarise(soma = sum(Massa.Medida)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
              group_by(Ano.Base.Ral, Processo) %>%
              summarise(soma = sum(Massa.Indicada)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                group_by(Ano.Base.Ral, Processo) %>%
                summarise(soma = sum(Massa.Inferida)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Processo) %>%
                  summarise(soma = sum(Massa.Lavravel)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          } else {
            if (reserva == "contido") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, Processo) %>%
                    summarise(soma = sum(Contido.Medido)),
                  key = Ano.Base.Ral,
                  value = soma
                )
              
              return(x)
            } else if (reserva == "teor") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, Processo) %>%
                    summarise('teor' = teor_medido),
                  key = Ano.Base.Ral,
                  value = teor
                )
              
              return(x)
              
            }
          }
        }
      }
    }
  }

#_____reserva_groupBY_SUBSTANCIA.AMB
reserva_groupBY_SUBSTANCIA.AMB <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
            group_by(Ano.Base.Ral, Substancia.AMB) %>%
            summarise(soma = sum(Massa.Medida)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
              group_by(Ano.Base.Ral, Substancia.AMB) %>%
              summarise(soma = sum(Massa.Indicada)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.AMB) %>%
                summarise(soma = sum(Massa.Inferida)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.AMB) %>%
                  summarise(soma = sum(Massa.Lavravel)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          } else {
            if (reserva == "contido") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, Substancia.AMB) %>%
                    summarise(soma = sum(Contido.Medido)),
                  key = Ano.Base.Ral,
                  value = soma
                )
              
              return(x)
            } else if (reserva == "teor") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.AMB) %>%
                    summarise('teor' = teor_medido),
                  key = Ano.Base.Ral,
                  value = teor
                )
              
              return(x)
              
            }
          }
        }
      }
    }
  }


#_____reserva_groupBY_TITULAR
reserva_groupBY_TITULAR <-
  function(Substancia.AMB = ".",
           Substancia.Agrupadora = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome.Mina = ".",
           Processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                               grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
            group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
            summarise(soma = sum(Massa.Medida)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                 grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
              group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
              summarise(soma = sum(Massa.Indicada)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                   grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                summarise(soma = sum(Massa.Inferida)),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                     grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                  group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                  summarise(soma = sum(Massa.Lavravel)),
                key = Ano.Base.Ral,
                value = soma
              )
            
            return(x)
            
          } else {
            if (reserva == "contido") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                    summarise(soma = sum(Contido.Medido)),
                  key = Ano.Base.Ral,
                  value = soma
                )
              
              return(x)
            } else if (reserva == "teor") {
              x <-
                spread(
                  select(reserva_AMB[grepl(reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Substancia.RAL, pattern = Substancia.RAL, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Municipio.Mina, pattern = Municipio, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Processo, pattern = Processo, ignore.case = TRUE) == TRUE &
                                       grepl(reserva_AMB$Nome.Mina, pattern = Nome.Mina, ignore.case = TRUE) == TRUE, ], everything()) %>%
                    group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                    summarise('teor' = teor_medido),
                  key = Ano.Base.Ral,
                  value = teor
                )
              
              return(x)
              
            }
          }
        }
      }
    }
  }



FUNA_Tabela_Pareto_SPREAD <-
  function(Substancia.AMB = '.') {
    x <-
      spread(
        reserva_AMB[reserva_AMB$Processo %in% reserva_AMB[reserva_AMB$pareto == 1 &
                                                            grepl(x = reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE), c('Processo')] &
                      grepl(x = reserva_AMB$Substancia.AMB, pattern = Substancia.AMB, ignore.case = TRUE), c('Processo', 'Ano.Base.Ral', 'Massa.Medida')] %>%
          group_by(Processo, Ano.Base.Ral) %>% summarise("Massa.Medida" = sum(Massa.Medida)),
        key = Ano.Base.Ral,
        value = "Massa.Medida",
        fill = NA
      )
   
    return(x)
  }


# Avaliação de prioridades por critério de Quantis----

FUNA_RESERVA_Quantil_WIDE <-
  function(Substancia.Agrupadora = Substancia.Agrupadora,
           probs = 0.8, 
           ano_inicial = 2010,
           reserva = 'medida') {
    
    if (reserva == 'medida') {
      
      df <- 
        reserva_AMB[reserva_AMB$Ano.Base.Ral >= ano_inicial,]
      
      df_a <-
        df[grepl(df$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE),
           c("Processo","Ano.Base.Ral","CPF.CNPJ.Titular",
             "id_subs.ano","Substancia.Agrupadora",
             "Massa.Medida")]
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Processo,
            Substancia.Agrupadora,
            id_subs.ano,
            Ano.Base.Ral),
          "Massa.Medida" =
            sum(Massa.Medida))
      
      df_b <-
        summarise(
          group_by(df_a, id_subs.ano),
          "Quantil" = quantile(
            Massa.Medida,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subs.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Processo,
              df_a$Substancia.Agrupadora,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Massa.Medida >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Processo",
          "Substancia.Agrupadora",
          "Ano.Base.Ral",
          "Massa.Medida"
        )],
        names_from = Ano.Base.Ral,
        values_from = Massa.Medida)
      
      return(df_a)
      
      
      
    } else if (reserva == 'contido') {
      
      df <- 
        reserva_AMB[reserva_AMB$Ano.Base.Ral >= ano_inicial,]
      
      df_a <-
        df[grepl(df$Substancia.Agrupadora, pattern = Substancia.Agrupadora, ignore.case = TRUE),
           c("Processo","Ano.Base.Ral","CPF.CNPJ.Titular",
             "id_subs.ano","Substancia.Agrupadora",
             "Contido.Medido")]
      
      df_a <-
        summarise(
          group_by(
            df_a,
            CPF.CNPJ.Titular,
            Processo,
            Substancia.Agrupadora,
            id_subs.ano,
            Ano.Base.Ral),
          "Contido.Medido" =
            sum(Contido.Medido))
      
      df_b <-
        summarise(
          group_by(df_a, id_subs.ano),
          "Quantil" = quantile(
            Contido.Medido,
            probs = probs,
            na.rm = TRUE))
      
      df_a <-
        left_join(df_a, df_b,
                  by = "id_subs.ano")
      
      df_a$id_titular_municipio_subs <-
        paste(df_a$CPF.CNPJ.Titular,
              df_a$Processo,
              df_a$Substancia.Agrupadora,
              sep = "_")
      
      lista_ID_titular <-
        unique(df_a[df_a$Contido.Medido >
                      df_a$Quantil, ]$id_titular_municipio_subs)
      
      df_a <-
        pivot_wider(df_a[df_a$id_titular_municipio_subs %in% lista_ID_titular, c(
          "CPF.CNPJ.Titular",
          "Processo",
          "Substancia.Agrupadora",
          "Ano.Base.Ral",
          "Contido.Medido"
        )],
        names_from = Ano.Base.Ral,
        values_from = Contido.Medido)
      
      return(df_a)
      
      
      
    }
  }



