
FUNA_InformacoesComplementares <-
  function(processo = ".",
           cpfcnpj = ".",
           motivo_Processo = ".",
           situacao_Operacional_Processo = ".",
           inf_Complementares = ".") {
    x <-
      select(InformacoesComplementares[grepl(InformacoesComplementares$motivo_Processo, pattern = motivo_Processo) == TRUE &
                               grepl(InformacoesComplementares$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(InformacoesComplementares$situacao_Operacional_Processo, pattern = situacao_Operacional_Processo) == TRUE &
                               grepl(InformacoesComplementares$informacoes_Complementares, pattern = inf_Complementares) == TRUE &
                               grepl(InformacoesComplementares$processo, pattern = processo) == TRUE,], everything())
    return(x)
  }






