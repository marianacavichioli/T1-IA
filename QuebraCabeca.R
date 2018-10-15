source("Estado.R")

## Classe e métodos para o problema do Quebra-Cabeça de 8 peças
QuebraCabeca <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuebraCabeca", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuebraCabeca = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuebraCabeca <- function(obj) {
  quebra_cabeca <- t(matrix(obj$desc,nrow=3,ncol=3))
  print(quebra_cabeca)
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuebraCabeca <- function(atual, ...){

  if(is.null(atual$desc))
    return(Inf)

  objetivo = rbind(c(1,2,3), c(8,0,4), c(7,6,5))
  matriz = matrix(unlist(atual$desc), ncol = 3, byrow = TRUE)

  dist = 0

  for(i in 1:3){
    for(j in 1:3){
      if(matriz[i,j] != objetivo[i,j]){
        for(k in 1:3){
          for(l in 1:3){
            if(matriz[i,j] == objetivo[k,l]){
              if (i > k)
                linha = i - k
              else
                linha = k - i
              if (j > l)
                coluna = j - l
              else 
                coluna = l - j

              dist = dist + linha + coluna
              break
            }
          }
        }
      }
    }
  }
  
  return(dist)
}

geraFilhos.QuebraCabeca <- function(obj) {

  filhos <- list()
  filhosDesc <- list()

  desc <- obj$desc 
  atual = matrix(unlist(t(desc)), ncol = 3, byrow = TRUE) #matriz


  i <- which(atual == 0, arr.ind=T)[1]
  j <- which(atual == 0, arr.ind=T)[2]

  if(i==1){
    if(j==1){
    
      operadores <- list(rbind(c(atual[1,2],0,atual[1,3]), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[2,1],atual[1,2],atual[1,3]), c(0,atual[2,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])))
    }else if(j==2){

      operadores <- list(rbind(c(0,atual[1,1],atual[1,3]), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,3],0), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[2,2],atual[1,3]), c(atual[2,1],0,atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])))
    }else{ ## j==3
      operadores <- list(rbind(c(atual[1,1],0,atual[1,2]), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[2,3]), c(atual[2,1],atual[2,2],0), c(atual[3,1],atual[3,2],atual[3,3])))
    }

  }else if(i==2){

    if(j==1){
      operadores <- list(rbind(c(0,atual[1,2],atual[1,3]), c(atual[1,1],atual[2,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,2],0,atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[3,1],atual[2,2],atual[2,3]), c(0,atual[3,2],atual[3,3])))
    }else if(j==2){
      operadores <- list(rbind(c(atual[1,1],0,atual[1,3]), c(atual[2,1],atual[1,2],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(0,atual[2,1],atual[2,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,3],0), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[3,2],atual[2,3]), c(atual[3,1],0,atual[3,3])))
    }else{ ## j==3
      operadores <- list(rbind(c(atual[1,1],atual[1,2],0), c(atual[2,1],atual[2,2],atual[1,3]), c(atual[3,1],atual[3,2],atual[3,3])),
                          rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],0,atual[2,2]), c(atual[3,1],atual[3,2],atual[3,3])),
                         rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,2],atual[3,3]), c(atual[3,1],atual[3,2],0)))
    }

  }else{  ##i==3

    if(j==1){
      operadores <- list(rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(0,atual[2,2],atual[2,3]), c(atual[2,1],atual[3,2],atual[3,3])),
          rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,2],0,atual[3,3])))

    }else if(j==2){
      operadores <- list(rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],0,atual[2,3]), c(atual[3,1],atual[2,2],atual[3,3])),
           rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,2],atual[2,3]), c(0,atual[3,1],atual[3,3])),
           rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,1],atual[3,3],0)))
    }else{ ## j==3
      operadores <- list(rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,2],0), c(atual[3,1],atual[3,2],atual[2,3])),
          rbind(c(atual[1,1],atual[1,2],atual[1,3]), c(atual[2,1],atual[2,2],atual[2,3]), c(atual[3,1],0,atual[3,2])))
    }
  }

  filhosDesc <- c(t(operadores))
  
  for(filhoDesc in filhosDesc){
    filhoDesc <- unlist(c(t(filhoDesc)))
    filho <- QuebraCabeca(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- 0
    filho$f <- filho$h 
    filhos <- c(filhos, list(filho))
  }

  return(filhos) 
}
