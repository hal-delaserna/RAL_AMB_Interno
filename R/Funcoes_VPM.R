# Funções PRODUçãO BENEFICIADA ----
#_____VPM_QuantidadeValorCOMERCIALIZADO_GERAL
valor_VPM_GERAL <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "Produto.Comercializado.Quantidade") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "Produto.Comercializado.Venda") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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

#_____valor_VPM_groupBY_Nome
valor_VPM_groupBY_Nome <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, mina) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, mina) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, mina) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, mina) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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



#_____valor_VPM_groupBY_MUNICIPIO
valor_VPM_groupBY_MUNICIPIO <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Municipio) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Municipio) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Municipio) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Municipio) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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


#_____valor_VPM_groupBY_PRODUTO
valor_VPM_groupBY_PRODUTO <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Produto.Comercializado) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Produto.Comercializado) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Produto.Comercializado) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Produto.Comercializado) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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



#_____valor_VPM_groupBY_SUBSTANCIA.AMB
valor_VPM_groupBY_SUBSTANCIA.AMB <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Substancia.AMB) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Substancia.AMB) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.AMB) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.AMB) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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




#_____valor_VPM_groupBY_SUBSTANCIA.RAL
valor_VPM_groupBY_SUBSTANCIA.RAL <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, Substancia.RAL) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, Substancia.RAL) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, Substancia.RAL) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, Substancia.RAL) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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















#_____valor_VPM_groupBY_TITULAR
valor_VPM_groupBY_TITULAR <-
  function(Substancia.AMB = ".",
           Substancia.RAL = ".",
           CPF.CNPJ.Titular = ".",
           Municipio = ".",
           Nome = ".",
           Produto.Comercializado = ".",
           Tipo.Empreendimento = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
            group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
            summarise(soma = sum(Quantidade.Producao.Comercializada...Substancia)),
          key = Ano.Base.Ral,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
              group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
              summarise(soma = sum(Valor.Producao.Comercializada.Substancia.AMB)),
            key = Ano.Base.Ral,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                summarise(soma = sum(
                  Quantidade.Producao.Comercializada...Produto
                )),
              key = Ano.Base.Ral,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.AMB, pattern = Substancia.AMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Substancia.RAL, pattern = Substancia.RAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$CPF.CNPJ.Titular, pattern = CPF.CNPJ.Titular) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Municipio, pattern = Municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Produto.Comercializado, pattern = Produto.Comercializado) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Tipo.Empreendimento, pattern = Tipo.Empreendimento) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$Nome, pattern = Nome) == TRUE,], everything()) %>%
                  group_by(Ano.Base.Ral, CPF.CNPJ.Titular) %>%
                  summarise(soma = sum(
                    Valor.Producao.Comercializada...Produto
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



