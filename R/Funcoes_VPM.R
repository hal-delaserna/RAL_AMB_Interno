# Funções PRODUÇÃO BENEFICIADA ----
#_____VPM_QuantidadeValorCOMERCIALIZADO_GERAL
valor_VPM_GERAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "produto.Quantidade") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "produto.Venda") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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

#_____valor_VPM_groupBY_unidade
valor_VPM_groupBY_unidade <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano, mina) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano, mina) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano, mina) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano, mina) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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



#_____valor_VPM_groupBY_MUNICIPIO
valor_VPM_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano, municipio) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano, municipio) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano, municipio) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano, municipio) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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


#_____valor_VPM_groupBY_PRODUTO
valor_VPM_groupBY_PRODUTO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano, produto) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano, produto) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano, produto) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano, produto) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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



#_____valor_VPM_groupBY_SUBSTANCIA.AMB
valor_VPM_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano, substancia.amb) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano, substancia.amb) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano, substancia.amb) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano, substancia.amb) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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




#_____valor_VPM_groupBY_SUBSTANCIA.RAL
valor_VPM_groupBY_SUBSTANCIA.RAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano, substancia.ral) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano, substancia.ral) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano, substancia.ral) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano, substancia.ral) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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















#_____valor_VPM_groupBY_TITULAR
valor_VPM_groupBY_TITULAR <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           unidade = ".",
           produto = ".",
           tipo = ".",
           volume = "venda") {
    if (volume == "producao") {
      x <-
        spread(
          select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                     grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
            group_by(ano, cpfcnpj) %>%
            summarise(soma = sum(quantidade.producao.comercializada.substancia)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                       grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
              group_by(ano, cpfcnpj) %>%
              summarise(soma = sum(valor.producao.comercializada.substancia.amb)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                         grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                group_by(ano, cpfcnpj) %>%
                summarise(soma = sum(
                  quantidade.producao.comercializada.produto
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(VPM_QuantidadeValorCOMERCIALIZADO[grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.amb, pattern = subsAMB) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$substancia.ral, pattern = subsRAL) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$municipio, pattern = municipio) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$produto, pattern = produto) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$tipo, pattern = tipo) == TRUE &
                                                           grepl(VPM_QuantidadeValorCOMERCIALIZADO$unidade, pattern = unidade) == TRUE,], everything()) %>%
                  group_by(ano, cpfcnpj) %>%
                  summarise(soma = sum(
                    valor.producao.comercializada.produto
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



