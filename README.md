---
title: "Capítulo 2 - Aprendizagem Estatística"
author: "Hellen Medeiros Cardoso de Oliveira"
date: "5 de abril de 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = TRUE)
```
Livro: An Introduction to Statistical Learning with Applications in R

## Exercise 1

```{r, fig.width=7, fig.height=5}

set.seed(132)
f <- function(x){out <- 40-15*x+35*x^(2)-25*x^(3);return(out)}
n <- 20; 
L <- 0.1; U <- 0.9
x <- runif(n,L,U); x <- round(x,4)
y <- f(x)+rnorm(n,0,0.1)
y <- round(y,4)

dados <- data.frame(x,y)
summary(dados)

x.aux1 <- seq(L,U,0.01);y.aux1 <- f(x.aux1)
length(x.aux1)==length(y.aux1)

dados2<-data.frame(x.aux1)

plot(dados$x,dados$y,type="p",pch=16,col="black",xlab="x",ylab="y",xlim=c(L,U))
points(x.aux1,y.aux1,type="l",col="blue")

```
<p> **Figure 1:** The dataset and its generating process </p>

```{r nnet, fig.width=12, fig.height=12}

library(neuralnet)

error <- NULL
N<- c(1,2,3,4,5,6,10,15,20,30,45,80)

par(mfrow=c(4,3))
for(k in 1:12){
  
  i<- N[k]
  nn <- neuralnet(y~x, data=dados, hidden=i, algorithm="rprop+", err.fct="sse", linear.output=TRUE)
  betas.hat <- as.vector(nn$weights[[1]][[1]][,1])
  error[k] <- nn$result.matrix[1,1]
  
  y.aux2<- compute(nn,dados2)$net.result
  
  plot(dados$x,dados$y,type="p",pch=16,col="black",xlab="x",ylab="y",xlim=c(L,U),main=i)
  points(x.aux1,y.aux1, type="l",col="blue")
  points(dados2$x.aux1,y.aux2, type="l",col="red")
}

```
<p> **Figure 2:** Estimated model for different values of N (hidden) = 1, 2, 3, 4, 5, 6, 10, 15, 20, 30, 45 e 80 </p>
Como podemos observar, se o número de camadas ocultas forem muito baixas ou muito altas, ocorre o subajustamento ou superajustamento. No caso do exemplo acima, podemos ver claramente que N (hidden) = 15 funcionou melhor que os damais números de camadas ocultas.

```{r}
data.frame(N,error)


```

# Exercicio1-Redes-Neurais
Click [aqui](https://hellenmed.github.io/Exercicio1-Redes-Neurais/) 
