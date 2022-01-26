library(xlsx)
library(igraph)
library(dplyr)
prot1<- c("M6P3","RALA","GABARAPL2","GABARAPL2","GABARAPL2","GABARAPL2","GABARAPL2","OTC", "FUNDC1")
prot2<- c("PLIN3","EXOC8","CD300C","UVRAG","FUNDC1","PIK3R4","ATG14","ARG2","OTC")
covid1<-c("E","E","E","M","M")
protcov<-c("M6P3","EXOC8","GABARAPL2","PIK3R4","ARG2")
humana<- data.frame(prot1,prot2)
covid<- data.frame(covid1,protcov)
humana.igraph <- graph_from_data_frame(humana)
covid.igraph <- graph_from_data_frame(covid)


a<- match(network.table$PreyGeneName, RedHumana$protein1)
b<- match(network.table$PreyGeneName, RedHumana$protein2)

prueba <- all_simple_paths(humana.igraph, from = "GABARAPL2" ,to=V(humana.igraph))
prueba
prueba1 <-all_simple_paths(covid.igraph,from = "M6PR", to=V(covid.igraph))
#Acceder a la lista
nodo_salida <- prueba [[1]][[1]]$name
nodo_relaciona<- prueba[[1]][[2]]$name

##Prueba de algoritmos
covprot <- covid$protcov 
gen=covprot[2]
camino<- all_simple_paths(humana.igraph, from=gen,to=V(humana.igraph), mode="all")
caminoReves <- all_simple_paths(humana.igraph, from=gen, to=V(humana.igraph), mode="out")

for (j in 1:length(camino)){
  vS1<- c(vS1, camino[[j]][[1]]$name)
  vS2 <- c(vS2,camino[[j]][[2]]$name )
}









for(i in 1:length(vectorProt)){
  gen<- vectorProt[i]
  camino<- all_simple_paths(RedHumana, from = gen ,  to=V(RedHumana))
  for (j in 1:length(camino)){
    vS1<- c(VS1, camino[[j]][[1]]$name)
    vS2 <- c(vS2,camino[[j]][[2]]$name )
  }
}

# Algoritmo
#creamos vector con los genes unido a las proteinas del covid
covprot <- covid$protcov 
#accedemos a los valores de este vector
covprot[1]
#tendremos un objeto iterador i=1, i<length(covprot), i++
i=1
# usaremos all_simple_paths para averiguar todos los genes con quien se une mi valor de covprot[1]
# guardaremos en una variable el resultado y añadiremos cada uno de los componentes en nuestro vector de relaciones.
#vectorProt<- covprot, RedHumana<- humana, iterador=i

vS1<- c()
vS2<- c()
#Cambiar la definicion del to por un gen que se une con la del covid virica.
obtener_red <- function(vectorProt, RedHumana){
  for(i in 1:length(vectorProt)){
    gen<- vectorProt[i]
    gen1<- vectorProt[i]
    camino<- all_simple_paths(RedHumana, from = gen ,  to=V(RedHumana), mode="all")
    print(camino)
    for (j in 1:length(camino)){
      print(paste0(camino[[j]], " "))
     vS1<- c(vS1, camino[[j]][[1]]$name)
     vS2 <- c(vS2,camino[[j]][[2]]$name )
    }
  }
  a<- data.frame(vS1,vS2)
  return (a)
}

b<- obtener_red(covprot,humana.igraph)

b <- b%>% distinct
c <- unique(b)
ba<- graph_from_data_frame(c)
plot_graph <- function(graph, vertex_color="tomato"){
  V(graph)$label <-NA
  V(graph)$name <- NA
  #V(graph)$size = vertex_size
  V(graph)$size <- degree(graph)/10
  E(graph)$edge.color <- "gray80"
  E(graph)$width <- .01
  V(graph)$color <- vertex_color
  #graph_attr(graph, "layout") <- layout_with_lgl
  graph_attr(graph, "layout") <- layout.kamada.kawai
  plot(graph)
  
}
plot_graph(ba)
### Lectura de proteoma completo:
protComp <- read.csv("ProteomaTCompleto.csv", header=TRUE)
protComp <- protComp[-1,]
a<- match(network.table$PreyGeneName, protComp$V2)
b<- match(network.table$PreyGeneName, protComp$V3)

