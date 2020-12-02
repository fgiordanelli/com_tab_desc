---
title: "desc_automatizado"
output: html_document
---
# {.tabset}
```{r cars, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}
library(sas7bdat)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(kableExtra)
library(questionr)
library(knitr)
library(descr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(huxtable)
library(xtable)
library(anomalize)
library(rlang)

dados <- read.csv("D:/Users/fabricio_giordanelli/Downloads/dados_seguradora.csv", stringsAsFactors = TRUE)

dados$preco_seguro <- as.numeric(dados$preco_seguro)
dados$franquia <- as.numeric(dados$franquia)

lista=list()

for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE)  {
      teste <- dados %>%
        dplyr::group_by(dados[,i]) %>%
        dplyr::summarise(qtd = n()) %>%
        dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
        dplyr::arrange(-perc) %>%
        dplyr::mutate(n_Count = row_number()) %>%
        dplyr::filter(n_Count <= 10) %>%
        dplyr::rename(col1 = 1)

    lista[[i]]= ggplot2::ggplot(teste, aes(x = col1, y = qtd, fill = col1)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")), size = 2.5, vjust = -1) +
          scale_fill_brewer(palette="Paired") +
          ggplot2::labs(x = NULL,
                        y = "count & perc",
                        fill = colnames(dados[i]),
                        title = paste("Quantidade e Percentual de", colnames(dados[i])))
   
    lista2 = list()
    lista2[[i]]=    dados %>% 
        dplyr::group_by(!!sym(colnames(dados[i]))) %>%
              dplyr::summarize(n = n()) %>%
        dplyr::mutate(perc = 100*round(n/sum(n),4)) %>%
        dplyr::arrange(-perc)
    }
}

```

```{r cars2, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}
library(sas7bdat)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(kableExtra)
library(questionr)
library(knitr)
library(descr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(huxtable)
library(xtable)
library(anomalize)
library(rlang)

dados <- read.csv("D:/Users/fabricio_giordanelli/Downloads/dados_seguradora.csv", stringsAsFactors = TRUE)

dados$preco_seguro <- as.numeric(dados$preco_seguro)
dados$franquia <- as.numeric(dados$franquia)

for (i in 1:ncol(dados)) {
  if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE)  {

  cat('##',colnames(dados)[i],' \n')

    cat('\n\n')
    
cat('\n\n')

  print(
    lista[[i]]
   )
  cat('\n\n')
    print(
    knitr::kable(lista2[[i]])
  )
  cat('\n\n')
  

  
}
}
```
