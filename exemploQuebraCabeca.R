debugSource("QuebraCabeca.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")


inicial <- QuebraCabeca(desc = rbind(c(1,2,3), c(4,5,6), c(7,8,0))) ## 8 peças e 1 lugar vazio     

# 1 2 3
# 4 5 6 
# 7 8 0

objetivo <- QuebraCabeca()
objetivo$desc <- rbind(c(1,2,3), c(8,0,4), c(7,6,5))

# 1 2 3
# 8 0 4 
# 7 6 5

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