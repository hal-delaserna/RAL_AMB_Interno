library(imputeTS)

# boxplot series preço-ano ----
boxQuadro <-
  function(df,
           anos = 2011:2018,
           ylim = c(10, 120)) {
    lista <- list()
    df <- df
    for (i in anos) {
      if (length(df[df$ano == i, c("preco")]) != 0) {
        lista[[i]] <-
          df[df$ano == i, c("preco")]
      }
    }
    boxplot(lista[anos], ylim = ylim, na.rm = TRUE)
  }

# BarPlot ROM-ano ----

FUNA_BARPLOT <-
  function(x) {
    barplot(height = as.matrix(na_replace(x, fill = 0)))
  }