install.packages("igraph")
library(igraph)

#CRIANDO O GRAFO:
lista_de_arestas <- read.csv(file = "edges.csv",
                             header = TRUE, sep = ';')

lista_de_vertices <- read.csv(file = "nodes.csv",
                              header = TRUE, sep = ';')

grafo <- graph_from_data_frame(lista_de_arestas,
                               directed = FALSE,
                               vertices = lista_de_vertices)

#ANALISANDO AS DIMINSOES DO GRAFO:
gorder(grafo)
gsize(grafo)


#Livro com maior numero de interacao entre os personagens
hist(lista_de_arestas$book)
max(lista_de_arestas$book)


#PLOT DO GRAFO:
plot(grafo)
tkplot(grafo, vertex.color = "lightblue")


#GRAU:
grau <- degree(grafo)
maior_grau <- max(grau)
vertice_de_maior_grau <- which(grau == maior_grau)
sort(grau, decreasing = TRUE)[1:10]
summary(grau)


#INTERMEDIACAO DE VERTICE:
intermediacao_vertice <- betweenness(grafo)
maior_intermediacao_vertice <- max(intermediacao_vertice)
vertice_maior_intermediacao <- which(intermediacao_vertice == maior_intermediacao_vertice)
sort(intermediacao_vertice, decreasing = TRUE)[1:10]


#DISTANCIAS:
distances(grafo)
mean_distance(grafo)
shortest_paths(grafo, "Voymir", "Ciri")
diameter(grafo)
caminho_do_diametro <- get_diameter(grafo)
caminho_do_diametro


#DENSIDADE DA REDE:
edge_density(grafo)


#CENTRALIDADE DE AUTOVETOR:
centralidade <- eigen_centrality(grafo)
sort(centralidade $vector, decreasing = TRUE)[1:10]


#VIZINHANCA E SUBGRAFO:
neighborhood(grafo, order = 1, "Geralt")
neighborhood(grafo, order = 1, "Ciri")
neighborhood(grafo, order = 1, "Dandelion")
neighborhood(grafo, order = 1, "Yennefer")

sub_grafo1 <- subgraph.edges(grafo, E(grafo)[inc(c("Geralt", "Ciri", "Dandelion", "Yennefer"))])
plot(sub_grafo1,
     vertex.label = ifelse(V(grafo)$name %in% c("Geralt", "Ciri", "Dandelion", "Yennefer"),
                           V(grafo)$name,
                           NA),
     vertex.size = ifelse(V(grafo)$name %in% c("Geralt", "Ciri", "Dandelion", "Yennefer"),
                          13,
                          4),
     vertex.color = ifelse(V(grafo)$name %in% c("Geralt", "Ciri", "Dandelion", "Yennefer"),
                           "red",
                           "blue"))


#COMUNIDADES:
comunidade1 <- cluster_walktrap(grafo)
modularity(comunidade1)
plot(comunidade1, grafo, vertex.label = NA, vertex.size = 10)

#MEMBROS DAS COMUNIDADES:
membros_comunidade1 <- membership(comunidade1)
lista <- table(membros_comunidade1)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade1, grafo, vertex.size = 10, vertex.label = as.character(membros_comunidade1))
plot(comunidade1, grafo, vertex.size = 10, vertex.label = NA)
membros_comunidade1[membros_comunidade1 == 8]
membros_comunidade1[membros_comunidade1 == 2]


#CLIQUES(subgrafos de maior influencia):
cliques(grafo)
largest.cliques(grafo)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
  
  
#GRAFO DO PRIMEIRO LIVRO
lista_de_arestas1 <- lista_de_arestas[lista_de_arestas$book==1, ]
lista_de_vertices1<- read.csv(file = "nodes1.csv",
                              header = TRUE, sep = ';')

lista_de_arestas1<- read.csv(file = "edges1.csv",
                              header = TRUE, sep = ';')

grafo1 <- graph_from_data_frame(lista_de_arestas1,
                               directed = FALSE,
                               vertices = lista_de_vertices1)
gorder(grafo1)
gsize(grafo1)

edge_density(grafo1)
centralidade_1 <- eigen_centrality(grafo1)
sort(centralidade_1$vector, decreasing = TRUE)[1:10]
neighborhood(grafo1, order = 1, "Geralt")
neighborhood(grafo1, order = 1, "Ciri")
neighborhood(grafo1, order = 1, "Yennefer")
neighborhood(grafo1, order = 1, "Dandelion")

plot(grafo1, vertex.color = "lightblue")

comunidade_1 <- cluster_walktrap(grafo1)
modularity(comunidade_1)
plot(comunidade_1, grafo1, vertex.label = NA, vertex.size = 10)
membros_comunidade_1 <- membership(comunidade_1)
lista <- table(membros_comunidade_1)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_1, grafo1, vertex.size = 12, vertex.label = as.character(membros_comunidade_1))
membros_comunidade_1[membros_comunidade_1 == 1]
membros_comunidade_1[membros_comunidade_1== 18]

pesos_sourse_1 <- lista_de_arestas$Weight[lista_de_arestas$book==1][lista_de_arestas$Source=='Geralt'][1:17]
pesos_target_1 <- lista_de_arestas$Weight[lista_de_arestas$book==1][lista_de_arestas$Target=='Geralt'][1:19]
(mean(pesos_sourse_1) + mean(pesos_target_1))/2

distancia_geralt_1 <- distances(grafo1)['Geralt', ]
lista_1 <- sort(distancia_geralt_1, decreasing = FALSE)[1:85]
mean(lista_1)



#GRAFO DO SEGUNDO LIVRO
lista_de_arestas2 <- lista_de_arestas[lista_de_arestas$book==2, ]
lista_de_vertices2<- read.csv(file = "nodes2.csv",
                              header = TRUE, sep = ';')
grafo2 <- graph_from_data_frame(lista_de_arestas2,
                                directed = FALSE,
                                vertices = lista_de_vertices2)

gorder(grafo2)
gsize(grafo2)

edge_density(grafo2)
centralidade_2 <- eigen_centrality(grafo2)
sort(centralidade_2$vector, decreasing = TRUE)[1:10]
neighborhood(grafo2, order = 1, "Geralt")
neighborhood(grafo2, order = 1, "Ciri")
neighborhood(grafo2, order = 1, "Yennefer")
neighborhood(grafo2, order = 1, "Dandelion")

plot(grafo2, vertex.color = "lightblue")

comunidade_2 <- cluster_walktrap(grafo2)
modularity(comunidade_2)
plot(comunidade_2, grafo2, vertex.label = NA, vertex.size = 10)
membros_comunidade_2 <- membership(comunidade_2)
lista <- table(membros_comunidade_2)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_2, grafo2, vertex.size = 12, vertex.label = as.character(membros_comunidade_2))
membros_comunidade_2[membros_comunidade_2 == 3]
membros_comunidade_2[membros_comunidade_2== 2]




#GRAFO DO TERCEIRO LIVRO
lista_de_arestas3 <- lista_de_arestas[lista_de_arestas$book==3, ]
lista_de_vertices3 <- read.csv(file = "nodes3.csv",
                              header = TRUE, sep = ';')
grafo3 <- graph_from_data_frame(lista_de_arestas3,
                                directed = FALSE,
                                vertices = lista_de_vertices3)
gorder(grafo3)
gsize(grafo3)

edge_density(grafo3)
centralidade_3 <- eigen_centrality(grafo3)
sort(centralidade_3$vector, decreasing = TRUE)[1:10]
neighborhood(grafo3, order = 1, "Geralt")
neighborhood(grafo3, order = 1, "Ciri")
neighborhood(grafo3, order = 1, "Yennefer")
neighborhood(grafo3, order = 1, "Dandelion")

plot(grafo3, vertex.color = "lightblue")

comunidade_3 <- cluster_walktrap(grafo3)
modularity(comunidade_3)
plot(comunidade_3, grafo3, vertex.label = NA, vertex.size = 10)
membros_comunidade_3 <- membership(comunidade_3)
lista <- table(membros_comunidade_3)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_3, grafo3, vertex.size = 12, vertex.label = as.character(membros_comunidade_3))
membros_comunidade_3[membros_comunidade_3 == 3]
membros_comunidade_3[membros_comunidade_3== 2]

pesos_sourse_3 <- lista_de_arestas$Weight[lista_de_arestas$book==3][lista_de_arestas$Source=='Geralt'][1:23]
pesos_target_3 <- lista_de_arestas$Weight[lista_de_arestas$book==3][lista_de_arestas$Target=='Geralt'][1:29]
(mean(pesos_sourse_3) + mean(pesos_target_3))/2

distancia_geralt_3 <- distances(grafo3)['Geralt', ]
lista_3 <- sort(distancia_geralt_3, decreasing = FALSE)[1:59]
mean(lista_3)



#GRAFO DO QUARTO LIVRO
lista_de_arestas4 <- lista_de_arestas[lista_de_arestas$book==4, ]
lista_de_vertices4<- read.csv(file = "nodes4.csv",
                              header = TRUE, sep = ';')
grafo4 <- graph_from_data_frame(lista_de_arestas4,
                                directed = FALSE,
                                vertices = lista_de_vertices4)

gorder(grafo4)
gsize(grafo4)

edge_density(grafo4)
centralidade_4 <- eigen_centrality(grafo4)
sort(centralidade_4$vector, decreasing = TRUE)[1:10]
neighborhood(grafo4, order = 1, "Geralt")
neighborhood(grafo4, order = 1, "Ciri")
neighborhood(grafo4, order = 1, "Yennefer")
neighborhood(grafo4, order = 1, "Dandelion")

plot(grafo4, vertex.color = "lightblue")

comunidade_4 <- cluster_walktrap(grafo4)
modularity(comunidade_4)
plot(comunidade_4, grafo4, vertex.label = NA, vertex.size = 10)
membros_comunidade_4 <- membership(comunidade_4)
lista <- table(membros_comunidade_4)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_4, grafo4, vertex.size = 12, vertex.label = as.character(membros_comunidade_4))
membros_comunidade_4[membros_comunidade_4 == 4]
membros_comunidade_4[membros_comunidade_4== 2]



#GRAFO DO QUINTO LIVRO
lista_de_arestas5 <- lista_de_arestas[lista_de_arestas$book==5, ]
lista_de_vertices5<- read.csv(file = "nodes5.csv",
                              header = TRUE, sep = ';')
grafo5 <- graph_from_data_frame(lista_de_arestas5,
                                directed = FALSE,
                                vertices = lista_de_vertices5)

gorder(grafo5)
gsize(grafo5)

edge_density(grafo5)
centralidade_5 <- eigen_centrality(grafo5)
sort(centralidade_5$vector, decreasing = TRUE)[1:10]
neighborhood(grafo5, order = 1, "Geralt")
neighborhood(grafo5, order = 1, "Ciri")
neighborhood(grafo5, order = 1, "Yennefer")
neighborhood(grafo5, order = 1, "Dandelion")

plot(grafo5, vertex.color = "lightblue")

comunidade_5 <- cluster_walktrap(grafo5)
modularity(comunidade_5)
plot(comunidade_5, grafo5, vertex.label = NA, vertex.size = 10)
membros_comunidade_5 <- membership(comunidade_5)
lista <- table(membros_comunidade_5)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_5, grafo5, vertex.size = 12, vertex.label = as.character(membros_comunidade_5))
membros_comunidade_5[membros_comunidade_5 == 1]
membros_comunidade_5[membros_comunidade_5== 2]



#GRAFO DO SEXTO LIVRO
lista_de_arestas6 <- lista_de_arestas[lista_de_arestas$book==6, ]
lista_de_vertices6 <- read.csv(file = "nodes6.csv",
                              header = TRUE, sep = ';')
grafo6 <- graph_from_data_frame(lista_de_arestas6,
                                directed = FALSE,
                                vertices = lista_de_vertices6)

gorder(grafo6)
gsize(grafo6)

edge_density(grafo6)
centralidade_6 <- eigen_centrality(grafo6)
sort(centralidade_6$vector, decreasing = TRUE)[1:10]
neighborhood(grafo6, order = 1, "Geralt")
neighborhood(grafo6, order = 1, "Ciri")
neighborhood(grafo6, order = 1, "Yennefer")
neighborhood(grafo6, order = 1, "Dandelion")

plot(grafo6, vertex.color = "lightblue")

comunidade_6 <- cluster_walktrap(grafo6)
modularity(comunidade_6)
plot(comunidade_6, grafo6, vertex.label = NA, vertex.size = 10)
membros_comunidade_6 <- membership(comunidade_6)
lista <- table(membros_comunidade_6)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_6, grafo6, vertex.size = 12, vertex.label = as.character(membros_comunidade_6))
membros_comunidade_6[membros_comunidade_6 == 1]
membros_comunidade_6[membros_comunidade_6== 5]


#GRAFO DO SETIMO LIVRO
lista_de_arestas7 <- lista_de_arestas[lista_de_arestas$book==7, ]
lista_de_vertices7 <- read.csv(file = "nodes7.csv",
                               header = TRUE, sep = ';')
grafo7 <- graph_from_data_frame(lista_de_arestas7,
                                directed = FALSE,
                                vertices = lista_de_vertices7)
gorder(grafo7)
gsize(grafo7)

edge_density(grafo7)
centralidade_7 <- eigen_centrality(grafo7)
sort(centralidade_7$vector, decreasing = TRUE)[1:10]
neighborhood(grafo7, order = 1, "Geralt")
neighborhood(grafo7, order = 1, "Ciri")
neighborhood(grafo7, order = 1, "Yennefer")
neighborhood(grafo7, order = 1, "Dandelion")

plot(grafo7, vertex.color = "lightblue")

comunidade_7 <- cluster_walktrap(grafo7)
modularity(comunidade_7)
plot(comunidade_7, grafo7, vertex.label = NA, vertex.size = 10)
membros_comunidade_7 <- membership(comunidade_7)
lista <- table(membros_comunidade_7)
lista_2 <- sort(lista, decreasing = TRUE)[1:5]
lista_2
plot(comunidade_7, grafo7, vertex.size = 12, vertex.label = as.character(membros_comunidade_7))
membros_comunidade_7[membros_comunidade_7 == 6]
membros_comunidade_7[membros_comunidade_7== 2]








































