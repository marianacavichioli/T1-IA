debugSource("Cidades.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")


inicial <- Cidades(desc = rbind(c(1,2,3), c(4,5,6), c(7,8,0))) ## 8 peças e 1 lugar vazio     

objetivo <- Cidades()
objetivo$desc <- rbind(c(1,2,3), c(8,0,4), c(7,6,5))

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