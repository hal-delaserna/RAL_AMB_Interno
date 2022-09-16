
FUNA_Eventos_RRR_RFP <-
  function(processo = ".",
           cpfcnpj = ".",
           subsAMB = ".",
           situacao.operacional = ".",
           reavaliacao.reserva = ".",
           alteracao.pae.reserva = ".") {
    x <-
      select(Eventos_RRR_RFP[grepl(Eventos_RRR_RFP$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(Eventos_RRR_RFP$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(Eventos_RRR_RFP$reavaliacao.reserva, pattern = reavaliacao.reserva) == TRUE &
                               grepl(Eventos_RRR_RFP$situacao.operacional.processo, pattern = situacao.operacional) == TRUE &
                               grepl(Eventos_RRR_RFP$processo, pattern = processo) == TRUE,], everything())
    return(x)
  }