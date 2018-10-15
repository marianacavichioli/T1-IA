source("QuebraCabeca.R")
source("buscaDesinformada.R")
source("buscaInformada.R")


inicial <- QuebraCabeca(desc = c(2,8,3,1,6,4,7,0,5)) ## 8 peças e 1 lugar vazio     
print(inicial)

objetivo <- QuebraCabeca()
objetivo$desc <- c(1, 2, 3, 8, 0, 4, 7, 6, 5)
print(objetivo$desc)

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))