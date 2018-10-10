source("Estado.R")

Cidades <- function(desc=NULL, pai=NULL, cidades=NULL){

  e <- environment()
 
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("cidades", cidades, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Cidades", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Cidades = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Cidades <- function(obj) {
  cat("Cidades: ", obj$desc, "\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica <- function(atual, ...){
  
  # if(is.null(atual$desc))
  #   return(Inf)
  
  if(atual$desc == "A"){
  	distancia = 366
  }else if(atual$desc == "B"){
  	distancia = 0
  }else if(atual$desc == "C"){
  	distancia = 160
  }else if(atual$desc == "D"){
  	distancia = 242
  }else if(atual$desc == "F"){
  	distancia = 178
  }else if(atual$desc == "G"){
  	distancia = 77
  }else if(atual$desc == "L"){
  	distancia = 244
  }else if(atual$desc == "M"){
  	distancia = 241
  }else if(atual$desc == "O"){
  	distancia = 380
  }else if(atual$desc == "P"){
  	distancia = 98
  }else if(atual$desc == "R"){
  	distancia = 193
  }else if(atual$desc == "S"){
  	distancia = 253
  }else if(atual$desc == "T"){
  	distancia = 329
  }else if(atual$desc == "U"){
  	distancia = 80
  }else if(atual$desc == "Z"){
  	distancia = 374
  }

  return(distancia)
}

geraFilhos <- function(obj) {
  
  cidade <- obj$cidades
  vizinhos <- c(cidade[obj$desc,])
  filhos <- list()
  filhosDesc <- list()

  i <- which(vizinhos != 0)

  filhosDesc <- c(filhosDesc, unlist(names(i)))
  #print(filhosDesc)

  ## gera os objetos Canibais para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Cidades(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    #print(vizinhos[filhoDesc])
    filho$g <- obj$g + vizinhos[filhoDesc]
    filho$f <- filho$h + filho$g
    filhos <- c(filhos, list(filho))
  }
  #print(filhos)
  return(filhos)
} 