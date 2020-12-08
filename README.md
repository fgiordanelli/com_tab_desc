---
title: "Relatório Descritiva"
output: 
  html_document:
    keep_md: true
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
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
library(readr)


# dados <- readr::read_csv("D:/Users/fabricio_giordanelli/Downloads/dados_seguradora.csv", col_types = cols(qtd_batidas = col_character()),locale = readr::locale(encoding = "latin1"))
# 
# dados <- as.data.frame(dados)
  
dados <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')

```


# Fator

# {.tabset}
```{r cars, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}



for (i in 1:ncol(dados)) {
  if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE)  {

  cat('##',colnames(dados)[i],' \n')
    
  cat('\n\n')

  
  teste <- dados %>%
        dplyr::group_by(dados[,i]) %>%
        dplyr::summarise(qtd = n()) %>%
        dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
        dplyr::arrange(-perc) %>%
        dplyr::mutate(n_Count = row_number()) %>%
        dplyr::filter(n_Count <= 10) %>%  
        dplyr::rename(col1 = 1)
  
  print(
          ggplot2::ggplot(teste, aes(x = col1, y = qtd, fill = col1)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")), size = 2.5, vjust = -1) +
          scale_fill_brewer(palette="Paired") +
          ggplot2::labs(x = NULL,
                        y = "count & perc",
                        fill = colnames(dados[i]),
                        title = paste("Quantidade e Percentual de", colnames(dados[i])))
  )
   
     cat('\n\n')
     
     print(
       knitr::kable(
      dados %>%
        dplyr::group_by(!!sym(colnames(dados[i]))) %>%
              dplyr::summarize(n = n()) %>%
        dplyr::mutate(perc = 100*round(n/sum(n),4)) %>%
        dplyr::arrange(-perc)
          ) %>%
        kableExtra::kable_styling()
          )
    cat('\n\n')

  
  }
}

```




# Numérica

# {.tabset}
```{r cars2, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}


for (i in 1:ncol(dados)) {
  if (is.numeric(dados[,i]) == TRUE) {

  cat('##',colnames(dados)[i],' \n')
    
  cat('\n\n')
    

      boxplot(dados[,i], main= colnames(dados)[i])

      tryCatch({
      print(
      knitr::kable(
      as_tibble(dados) %>%
          na.omit() %>%
          anomalize(colnames(dados)[i], method = "gesd") %>%
          group_by(anomaly) %>%
          summarise(qtd = n()) %>%
          mutate(perc = 100*round(qtd/sum(qtd),4))
      ) %>%
        kableExtra::kable_styling()
      )
      stop("precisa ser fator")} ,error = function(e){})

    }
  }

```






# Fator e Fator

# {.tabset}
```{r cars3, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}



for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      if ((is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) &
          (is.factor(dados[,j]) == TRUE | is.character(dados[,j]) == TRUE)) {

  cat('##',paste(colnames(dados)[i], "x",colnames(dados)[j]),' \n')
    
  cat('\n\n')
  
        
        teste <- dados %>%
          dplyr::group_by(dados[,i],dados[,j]) %>%
          dplyr::summarise(qtd = n()) %>%
          dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
          dplyr::mutate(n_Count = row_number()) %>%
          dplyr::filter(n_Count <= 10) %>%
          dplyr::rename(col1 = 1, col2 = 2)

        print(
          ggplot2::ggplot(teste, aes(x = col1,y = qtd, fill = col2)) +
            ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
            scale_fill_brewer(palette="Paired") +
            ggplot2::labs(x = NULL,
                          y = "count & perc",
                          fill = colnames(dados[j]),
                          title = paste("Quantidade e Percentual de", colnames(dados[i])," por ",colnames(dados[j]))) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


        )
        
          cat('\n\n')
          
  tryCatch({

        if (dados %>% select_at(colnames(dados[i])) %>% distinct() %>% count()  == 2){

      f <- as.formula(paste(colnames(dados[i]), "~", paste(colnames(dados)[j], collapse=" + ")))

reg <- glm(f, family=binomial, data = dados)

  print(
    knitr::kable(odds.ratio(reg),
                 format = "html",
                         digits = 4,
                         caption = paste(colnames(dados[i]),  " = ",levels(dados[,i])[2], "e", colnames(dados[j]),  " = ", levels(dados[,j])[1]))  %>%
      kableExtra::kable_styling()
    )
        }
  stop("precisa ser fator")} ,error = function(e){})
        
         cat('\n\n') 
          
      }
    }
}

```







# Fator e Numérica

# {.tabset}
```{r cars4, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}

for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) {
      for (j in 1:ncol(dados)) {
        if (is.numeric(dados[,j]) == TRUE) {
          
            cat('##',paste(colnames(dados)[i], "x",colnames(dados)[j]),' \n')
    
  cat('\n\n')

          nome1 <- colnames(dados[i])
          nome2 <- colnames(dados[j])

            teste <- dados %>%
              dplyr::group_by_at(nome1) %>%
              dplyr::summarize_at(.vars = nome2,
                                  list(soma = ~sum(.,na.rm = TRUE))) %>%
              dplyr::mutate(perc = 100*round(soma/sum(soma),4)) %>%
              dplyr::arrange(-perc) %>%
              dplyr::mutate(n_Count = row_number()) %>%
              dplyr::filter(n_Count <= 10) %>%
              dplyr::rename(col1 = 1)

            print(
              ggplot2::ggplot(teste, aes(x = col1, y = soma, fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = paste0("(",round(soma,0),", ",perc,"%",")")), size = 2.5, vjust = -1) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "count & perc",
                              fill = colnames(dados[i]),
                              title = paste("Soma total de ", colnames(dados[j]))) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
            )
            
            cat('\n\n')
            
          


          print(
            ggplot2::ggplot(dados, aes_string(x = nome1,y = nome2)) +
              ggplot2::geom_boxplot() +
              ggplot2::labs(
                title = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")
              ) +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
          )
          
          cat('\n\n')


          print(
            knitr::kable(dados %>%
                           dplyr::group_by_at(colnames(dados[i])) %>%
                           dplyr::summarize_at(.vars = colnames(dados[j]),
                                               list(n = ~ n(),
                                                    ~ min(.,na.rm = TRUE),
                                                    q1 = ~ quantile(.,
                                                                    probs = c(0.25),
                                                                    na.rm = TRUE),
                                                    q3 = ~ quantile(.,
                                                                    probs = c(0.75),
                                                                    na.rm = TRUE),
                                                    ~ max(.,na.rm = TRUE),
                                                    ~ mean(., na.rm = TRUE),
                                                    ~ sd(., na.rm = TRUE))),
                         format = "html",
                         digits = 2,
                         caption = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")) %>%
              kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
          )
          
          cat('\n\n')

        print(
          lm(as.formula(paste(colnames(dados[j]),"~",colnames(dados[i]))), data = dados) %>%
   summary() %>%
    xtable() %>%
    kable(format = "html",
          digits = 4,
          caption = paste("Variável numérica ",colnames(dados[j]),  " pela variável categórica ",colnames(dados[i]), "=", levels(dados[,i])[1])) %>%
     kable_styling()
          )

        cat('\n\n')

        tryCatch({

        if (dados %>% select_at(colnames(dados[i])) %>% distinct() %>% count()  == 2){

      f <- as.formula(paste(colnames(dados[i]), "~", paste(colnames(dados)[j], collapse=" + ")))

reg <- glm(f, family=binomial, data = dados)

  print(
    knitr::kable(odds.ratio(reg),
                 format = "html",
                         digits = 4,
                         caption = paste(colnames(dados[i]),  " = ",levels(dados[,i])[2], "e", colnames(dados[j]),  " = ", "aqui considero o aumento de uma unidade"))  %>%
      kableExtra::kable_styling()
    )
        }
  stop("precisa ser fator")} ,error = function(e){})


          # f <- as.formula(paste(colnames(oi[j]),"~",colnames(oi[i])))
          # reg <- lm(f, data = dados)
          # print(
          # tab_model(reg, CSS = css_theme("cells"))
          # )
        }
      }
    }
  }

```
















# Fator e Numérica (por linhas)


# {.tabset}
```{r cars5, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}

  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) {
      for (j in 1:ncol(dados)) {
        if (is.numeric(dados[,j]) == TRUE) 
          
          cat('##',paste(colnames(dados)[i], "x",colnames(dados)[j]),' \n')
    
          cat('\n\n')

          
          {
          tryCatch({
            dados2 <- dados %>%
              dplyr::select(colnames(dados[i]),colnames(dados[j])) %>%
              dplyr::group_by_at(colnames(dados[i]))

            dados3 <- dados2 %>%
              dplyr::group_split() %>%
              setNames(unlist(group_keys(dados2)))



            dados4 <- do.call("cbind", dados3)

            dados5 <- dplyr::select_if(dados4, is.numeric)

            print(
              PerformanceAnalytics::chart.Correlation(dados5)
            )
            
            cat('\n\n')
            
            
            stop("teste")} ,error = function(e){})

        }
      }
    }
  }


```






















# Fator, Fator e Numérica


# {.tabset}
```{r cars6, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}

  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      for (k in 1:ncol(dados)) {
        if ((is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) &
            (is.factor(dados[,j]) == TRUE | is.character(dados[,j]) == TRUE) &
            is.numeric(dados[,k]) == TRUE ){
          {
            
            cat('##',paste(colnames(dados)[i], "x",colnames(dados)[j],"x",colnames(dados)[k]),' \n')
    
          cat('\n\n')


            tryCatch({

              nome1 <- colnames(dados[i])
              nome2 <- colnames(dados[j])
              nome3 <- colnames(dados[k])



                teste <- dados %>%
                  dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                  dplyr::summarize_at(.vars = nome3,
                                      list(soma = ~sum(.,na.rm = TRUE))) %>%
                  dplyr::mutate(perc = 100*round(soma/sum(soma),4)) %>%
                  dplyr::rename(col1 = 1, col2 = 2)

                print(
                  ggplot2::ggplot(teste, aes(x = col1, y = soma, fill = col2)) +
                    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
                    ggplot2::geom_text(aes(label = paste0("(",round(soma,0),", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
                    scale_fill_brewer(palette="Paired") +
                    ggplot2::labs(x = NULL,
                                  y = "count & perc",
                                  fill = colnames(dados[i]),
                                  title = paste("Soma total de tarifa")) +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
                )
                
                
                cat('\n\n')




              print(
                ggplot2::ggplot(dados, aes_string(x = nome3,y = nome1)) +
                  ggplot2::geom_boxplot() +
                  ggplot2::coord_flip() +
                  ggplot2::facet_wrap(as.formula(paste("~", nome2))) +
                  ggplot2::labs(
                    title = paste(nome1,nome2,nome3,sep = " x ")) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
              )
              
              cat('\n\n')
              
              
              stop("teste")} ,error = function(e){})

            tryCatch({
              print(
                knitr::kable(dados %>%
                               dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                               dplyr::summarize_at(.vars = nome3,
                                                   list(n = ~ n(),
                                                        ~ min(.,na.rm = TRUE),
                                                        q1 = ~ quantile(.,
                                                                        probs = c(0.25),
                                                                        na.rm = TRUE),
                                                        q3 = ~ quantile(.,
                                                                        probs = c(0.75),
                                                                        na.rm = TRUE),
                                                        ~ max(.,na.rm = TRUE),
                                                        ~ mean(., na.rm = TRUE),
                                                        ~ sd(., na.rm = TRUE))),
                             format = "html",
                             digits = 2,
                             caption = paste(nome1,nome2,nome3,sep = " x ")) %>%
                  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")))
              
              cat('\n\n')
              
              
              stop("teste")} ,error = function(e){})




           tryCatch({
             print(
          lm(as.formula(paste(colnames(dados[k]),"~",colnames(dados[i]), " + ", colnames(dados[j]))), data = dados) %>%
   summary() %>%
    xtable() %>%
    kable(format = "html",
          digits = 4,
          caption = paste("Variável numérica ",colnames(dados[k]),  " pela variável categórica ",colnames(dados[i]), "=", levels(dados[,i])[1]," e ", colnames(dados[j]), "=", levels(dados[,j])[1])) %>%
     kable_styling()
            )
             
             cat('\n\n')
             
             
             stop("teste")} ,error = function(e){})


            tryCatch({

        if (dados %>% select_at(colnames(dados[i])) %>% distinct() %>% count()  == 2){

      f <- as.formula(paste(colnames(dados[i]), "~", paste(colnames(dados)[j], "+", paste(colnames(dados)[k]))))

reg <- glm(f, family=binomial, data = dados)

  print(
    knitr::kable(odds.ratio(reg),
                 format = "html",
                         digits = 4,
                         caption = paste(colnames(dados[i]),  " = ",levels(dados[,i])[2], "e", colnames(dados[j]),  " = ", levels(dados[,j])[1], "e", colnames(dados[k]),  " = Numérica"))  %>%
      kableExtra::kable_styling()
    )
        }
              
              cat('\n\n')
              
              
  stop("precisa ser fator")} ,error = function(e){})




          }
        }
      }
    }
  }


```



































# Numérica e Numérica

# {.tabset}
```{r cars7, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}

for (i in 1:ncol(dados)-1) {
  for (j in (1+i):ncol(dados)) {
    if (is.numeric(dados[,i]) == TRUE & is.numeric(dados[,j]) == TRUE )  {

      
      cat('##',paste(colnames(dados)[i], "x",colnames(dados)[j]),' \n')
    
          cat('\n\n')
          
      nome1 <- colnames(dados[i])
      nome2 <- colnames(dados[j])

      print(
        PerformanceAnalytics::chart.Correlation(dados[,c(i,j)], histogram=TRUE, pch=19
        )
          )
      cat('\n\n')

      for (k in 1:ncol(dados)){
        if (is.factor(dados[,k]) == TRUE | is.character(dados[,k]) == TRUE) {

          tryCatch({
            a <- dados %>%
              dplyr::group_by(dados[,k]) %>%
              dplyr::summarize(corr = cor(!!sym(colnames(dados[i])),!!sym(colnames(dados[j]))))  %>%
              rename(col1 = 1)


            print(
              ggplot2::ggplot(a, aes(x = col1, y = corr,fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = round(corr,2))) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "Correlação",
                              fill = colnames(dados[k]),
                              title = paste("Correlação entre ",colnames(dados[i])," e ",colnames(dados[j]), "pela variável categórica",colnames(dados[k]))) +
                ggplot2::lims(y = c(-1,1)) +
                ggplot2::coord_flip() +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
            )
            
            cat('\n\n')

            stop("teste")} ,error = function(e){})
        }


      }

    }



  }
}


```
