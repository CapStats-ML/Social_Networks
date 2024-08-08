################################################################################
# Script Taller 5: Relaciones entre profesores por departementos en SEDE Bogota HORUS
# Autor: Cesar Augusto Prieto Sarmiento, Alejandro Urrego Lopez
# Fecha de Creación: 15/05/2024
# Ultima Fecha de Mod: 25/05/2024
# Descripción: Se plantea la realizacion del codigo para el taller 5 el cual consta de 
#           el analisis de las redes de trabajo entre los profesores por departamentos
#           de la facultad de ciencias de la sede Bogota
# Versión: 1.8
################################################################################

#### NOTAS ---- 

# Para el correcto desarrollo de este script de R se requiere que los paquetes a continuacion
# sean instalados y cargados, ademas de esto, en el desarrollo se planteo tener en el directorio
# de trabajo los archivos .Json correspondientes con los datos usados para cada departamento
# de la Facultad de Ciencias de la Universidad Nacional de Colombia Sede Bogotá, si no se quiere
# Seguir esta instruccion se debera cargar los archivos de la forma deseada y modificar un poco el Scrip

####


#### CARGA DE PAQUETES UTILIZADOS Y DIRECTORIO DE TRABAJO ----

library(dplyr)
library(igraph)
library(knitr)
library(RColorBrewer)
library(jsonlite)

#### RUTA Y CARGA DE ARCHIVOS JSON ----

setwd("~/En Proceso/Taller5")
ruta_carpeta <- "~/En Proceso/Taller5"

archivos <- list.files(path = ruta_carpeta, pattern = "\\.json$", full.names = TRUE)

nombres_archivos <- basename(archivos) # Extrae solo los nombres de archivo sin la ruta completa
nombres_archivos <- gsub("\\.json$", "", nombres_archivos)  # Elimina la extensión .json
nombres_archivos <- sapply(strsplit(nombres_archivos, "-"), function(x) x[length(x)])
nombres_archivos <- gsub("\\Departamento de ","", nombres_archivos)

# Crea un data frame con los nombres de archivo y los nombres de departamento
df_archivos <- data.frame(archivos, row.names = nombres_archivos)

BIO <- fromJSON(df_archivos["Biología",])
EST <- fromJSON(df_archivos["Estadística",])
FAR <- fromJSON(df_archivos["Farmacia",])
FIS <- fromJSON(df_archivos["Física",])
GEO <- fromJSON(df_archivos["Geociencias",])
MAT <- fromJSON(df_archivos["Matemáticas",])
QUIM <- fromJSON(df_archivos["Química",])


#### FUNCIONES UTILIZADAS PARA EL DESARROLLO ---- 

#Funcion para abreviacion de los nombres (Utilidad Graficas)

obtener_abreviado <- function(nombre) {
  palabras <- strsplit(nombre, " ")[[1]]
  paste(palabras[1], substr(palabras[2], 1, 1),'', sep = ".")
}

colors <- c(
  Estadistica = "#1f77b4",
  Biologia = "#2ca02c",
  Farmacia = "#9467bd",
  Fisica = "#d62728",
  Geociencias = "#8c564b",
  Matematicas = "#ff7f0e",
  Quimica = "#17becf"
)

cols <- c(brewer.pal(9,"Set1")[1:9], brewer.pal(8,"Set2")[1:7], 
          brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])


#### DESARROLLO PARA EL DEPARTAMENTO DE ESTADISTICA ----

# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- EST$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- EST$links
links <- links %>%
  filter(!source %in% c('2d4d5a17-7ef0-4fc4-8b25-d1af93445f50')) %>%
  filter(!target %in% c('2d4d5a17-7ef0-4fc4-8b25-d1af93445f50'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name

# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor('#1f77b4', 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#011f4b",0.05), vertex.frame.color = "#1f77b4", vertex.size = (strength(Proj))/4, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE ESTADISTICA")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.15), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.007), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G <- igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_fr(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.5),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#011f4b", 0.1),
     vertex.size = (strength(projection.G))/10, main = "")

# Segundo plot
plot(projection.G, vertex.label = Apellidos, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.5),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#011f4b", 0.08), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE ESTADISTICA",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_louvain - Mod = 0.2767", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))

# asortatividad 

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G),
                    directed = F, normalized = TRUE), 3)


#### DESARROLLO PARA EL DEPARTAMENTO DE FISICA ----

# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- FIS$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- FIS$links
links <- links %>%
  filter(!source %in% c('425bda01-002d-4b05-95fb-71196da8736e')) %>%
  filter(!target %in% c('425bda01-002d-4b05-95fb-71196da8736e'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name


# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor('#d62728', 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#4b0101",0.05), vertex.frame.color = "#d62728", vertex.size = (strength(Proj))/6, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE FISICA")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.15), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.007), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G<-igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_kk(projection.G)
l = layout_with_fr(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#4b0101", 0.02),
     vertex.size = (strength(projection.G)) / 20, 
     main = "SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE FISICA \n kc_louvain - Mod = 0.2171")

# Segundo plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#4b0101", 0.05), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE FISICA",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_louvain - Mod = 0.2171", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))



# asortatividad 

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G),
                    directed = F, normalized = TRUE), 3)


#### DESARROLLO PARA EL DEPARTAMENTO DE BIOLOGIA ----

# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- BIO$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- BIO$links
links <- links %>%
  filter(!source %in% c('e0a9295b-404c-4134-99f0-756e8849413a')) %>%
  filter(!target %in% c('e0a9295b-404c-4134-99f0-756e8849413a'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name


# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor('#2ca02c', 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#005200",0.05), vertex.frame.color = "#2ca02c", vertex.size = (strength(Proj))/6, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE BIOLOGIA")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.15), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.007), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G<-igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

#Abreviacion de los nombres apellido + inicial 

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_kk(projection.G)
l = layout_with_fr(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_leading_eigen$membership], 0.2),
     vertex.frame.color = cols[kc_leading_eigen$membership],
     edge.color = adjustcolor("#005200", 0.02),
     vertex.size = (strength(projection.G)) / 25, 
     main = "SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE BIOLOGIA \n kc_leading_eigen - Mod = 0.2743")

# Segundo plot
plot(projection.G, vertex.label = Apellidos, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_leading_eigen$membership], 0.2),
     vertex.frame.color = cols[kc_leading_eigen$membership],
     edge.color = adjustcolor("#005200", 0.02), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE BIOLOGIA",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_leading_eigen - Mod = 0.2743", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G),
                    directed = F, normalized = TRUE), 3)


#### DESARROLLO PARA EL DEPARTAMENTO DE MATEMATICAS ----

# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- MAT$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- MAT$links
links <- links %>%
  filter(!source %in% c('9b94b0ad-bff6-4f20-ae80-170d728a6b9c')) %>%
  filter(!target %in% c('9b94b0ad-bff6-4f20-ae80-170d728a6b9c'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name


# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor("#ff7f0e", 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#582900",0.02), vertex.frame.color = "#ff7f0e", vertex.size = (strength(Proj))/6, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE MATEMATICAS")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.15), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.007), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G<-igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

#Abreviacion de los nombres apellido + inicial 

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_kk(projection.G)
l = layout_with_drl(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#582900", 0.02),
     vertex.size = (strength(projection.G)/10) ,
     main = "SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE MATEMATICAS \n kc_louvain - Mod = 0.2603")

# Segundo plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#582900", 0.02), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE MATEMATICAS",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_louvain - Mod = 0.2603", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))

# asortatividad 

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G) ,
                    directed = F, normalized = TRUE))


#### DESARROLLO PARA EL DEPARTAMENTO DE FARMACIA ----

# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- FAR$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- FAR$links
links <- links %>%
  filter(!source %in% c('a43e06fd-60e5-48c6-8a38-ed271b978ea4')) %>%
  filter(!target %in% c('a43e06fd-60e5-48c6-8a38-ed271b978ea4'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name


# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor("#8500ff", 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#2c0055",0.03), vertex.frame.color = "#8500ff", vertex.size = (strength(Proj))/8, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE FARMACIA")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.15), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.007), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G<-igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

#Abreviacion de los nombres apellido + inicial 

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_fr(projection.G)
l = layout_with_kk(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#2c0055", 0.02),
     vertex.size = (strength(projection.G)) / 20, 
     main = "SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE FARMACIA \n kc_louvain - Mod = 0.2328")

# Segundo plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#2c0055", 0.02), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE FARMACIA",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_louvain - Mod = 0.2328", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))

# asortatividad 

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G),
                    directed = F, normalized = TRUE))

#### DESARROLLO PARA EL DEPARTAMENTO DE GEOCIENCIAS ----

# Acceder a los datos de nodes, links y options desde GEO
# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- GEO$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- GEO$links
links <- links %>%
  filter(!source %in% c('f9f44c87-afce-4bd3-a5aa-4988976fa40b')) %>%
  filter(!target %in% c('f9f44c87-afce-4bd3-a5aa-4988976fa40b'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name


# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor("#8c564b", 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#53352f",0.03), vertex.frame.color = "#8c564b", vertex.size = (degree(Proj))*2, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE GEOCIENCIAS")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.15), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.007), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G<-igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

#Abreviacion de los nombres apellido + inicial 

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_fr(projection.G)
l = layout_with_kk(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#53352f", 0.02),
     vertex.size = (strength(projection.G)) / 10, 
     main = "SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE GEOCIENCIAS \n kc_louvain - Mod = 0.1976 ")

# Segundo plot
plot(projection.G, vertex.label = Apellidos, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#53352f", 0.02), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE GEOCIENCIAS",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_louvain - Mod = 0.1976", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))

# asortatividad 

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G),
                    directed = F, normalized = TRUE))

#### DESARROLLO PARA EL DEPARTAMENTO DE QUIMICA ----

# Filtrando nodos para eliminar aquellos de tipo 'uab'
nodes <- QUIM$nodes
nodes <- nodes %>%
  filter(!type %in% c('uab'))

# Filtrando enlaces para eliminar aquellos que tengan como fuente o destino un nodo específico
links <- QUIM$links
links <- links %>%
  filter(!source %in% c('c290f8c7-1919-44c2-8e05-111249363db7')) %>%
  filter(!target %in% c('c290f8c7-1919-44c2-8e05-111249363db7'))

# Creando una columna booleana 'type_bool' para identificar si un nodo es un 'author'
type_bool <- ifelse(nodes[3] == 'author', TRUE, FALSE)
nodes <- nodes[-3]
nodes <- cbind(nodes, type_bool)

# Uniendo los enlaces con los nodos para obtener información detallada
REST <- links %>%
  left_join(nodes, by = c('source' = 'id')) %>%
  select(source, label.source = label, type.source = type, target) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  select(source, label.source, type.source, target, label.target = label, type.target = type) %>%
  as.data.frame()

# Filtrando los enlaces donde uno es autor y el otro no
A <- REST[REST['type.target'] + REST['type.source'] == 1, ]

# Filtrando y reestructurando los datos para obtener relaciones autor-tema
A1 <- A %>%
  filter(type.source == TRUE & type.target == FALSE) %>%
  select(author = source, topic = target) %>%
  as.data.frame()

A2 <- A %>%
  filter(type.source == FALSE & type.target == TRUE) %>%
  select(author = target, topic = source) %>%
  as.data.frame()

# Uniendo ambas tablas de relaciones autor-tema y agregando información del autor
At <- rbind(A1, A2) %>%
  left_join(nodes, by = c('author' = 'id')) %>%
  select(name = label, topic) %>%
  as.data.frame()

# Creando un data frame con IDs únicos para cada tema
id_topic <- data.frame(Ntopic = unique(At$topic),
                       id = seq(1, length(unique(At$topic))))

# Uniendo el data frame de temas con los IDs y seleccionando las columnas necesarias
At <- At %>%
  left_join(id_topic, by = c('topic' = 'Ntopic')) %>%
  select(name, id) %>%
  as.data.frame()

# Creando un vector de tipos para el grafo (autores y temas)
type <- c(rep(TRUE, length(unique(At$name))),
          rep(FALSE, length(unique(At$id))))

# Creando el grafo a partir del data frame de relaciones
grafo <- graph_from_data_frame(At)

# Asignando el atributo 'type' a los vértices del grafo
V(grafo)$type <- type
V(grafo)$name


# calculo de la matriz de proyeccion 
Proj <- bipartite_projection(grafo)$proj2

# resumenes del grafo

summary(Proj)
is_weighted(Proj)
E(Proj)$weight

# Plot del grafo

l =layout_with_kk(Proj, kkconst = 1, maxiter = 1000, epsilon = 0)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

plot(Proj, layout = l, vertex.label =NA , rescale = FALSE , xlim = c(min(l[,1]) ,max(l[,1])),
     vertex.color = adjustcolor("#17becf", 0.2), ylim = c(min((l[,2])),max(l[,2])),
     edge.color = adjustcolor("#0e5860",0.01), vertex.frame.color = "#17becf", vertex.size = (strength(Proj))/15, 
     main = "GRAFO DE LA RED DE INTERACCION ENTRE \n PROFESORES DEL DEP DE QUIMICA")


# grado de la red
d <- degree(graph = Proj)
head(sort(d, decreasing = T), n = 5)
tail(sort(d, decreasing = T), n = 5)

# fuerza de la red
wd <- strength(Proj)
head(sort(wd, decreasing = T), n = 5)
tail(sort(wd, decreasing = T), n = 5)

#grafica del grado 

n <- vcount(Proj)

plot(NA, NA, type = "n", xlim = c(0,max(d)+5), ylim = c(0,0.06), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion del grado en el grafo")
hist(d, freq = F, col = "lightskyblue", border = "royalblue", add = T)

plot(NA, NA, type = "n", xlim = c(0,max(wd)+50), ylim = c(0,0.002), xlab = "Grado", 
     ylab = "Densidad", main = "Distribucion de la fuerza del grafo" )
hist(wd, freq = F, col ="royalblue", border = "blue", add = T)

# medidas de centralidad

cc <- closeness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(cc, decreasing = T), n = 5),3)

bc <- betweenness(graph = Proj, normalized = T, weights =E(Proj)$weight)
round(head(sort(bc, decreasing = T), n = 5),3)

ec <- eigen_centrality(graph = Proj, scale = T, weights =E(Proj)$weight)$vector
round(head(sort(ec, decreasing = T), n = 5),3)

# Numero clan y transitividad de la red

largest_cliques(graph = Proj)
clique_num(graph = Proj)
edge_density(graph = Proj)
transitivity(graph = Proj, type = "global")
as.list(components(Proj)[1])

# Agrupamiento 
components <- igraph::components(Proj, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(Proj)[components$membership == biggest_cluster_id]

# subgraph

projection.G<-igraph::induced_subgraph(Proj, vert_ids)

# Tecnicas de agrupamiento sin pesos

kc_fast_greedy <- cluster_fast_greedy(projection.G)
kc_leading_eigen <- cluster_leading_eigen(projection.G)
kc_walktrap <- cluster_walktrap(projection.G)
kc_louvain <- cluster_louvain(projection.G)
kc_label_prop <- cluster_label_prop(projection.G)
kc_spinglass <- cluster_spinglass(projection.G)
kc_infomap <- cluster_infomap(projection.G)

resultados1 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass", "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedy), modularity(kc_leading_eigen),
                  modularity(kc_walktrap), modularity(kc_louvain),modularity(kc_label_prop),
                  modularity(kc_spinglass), modularity(kc_infomap))
)

# Tecnicas de agrupamiento con pesos

kc_fast_greedyw <- cluster_fast_greedy(projection.G,weights =E(projection.G)$weight)
kc_leading_eigenw <- cluster_leading_eigen(projection.G,weights =E(projection.G)$weight)
kc_walktrapw <- cluster_walktrap(projection.G,weights =E(projection.G)$weight)
kc_louvainw <- cluster_louvain(projection.G,weights =E(projection.G)$weight)
kc_label_propw <- cluster_label_prop(projection.G,weights =E(projection.G)$weight)
kc_spinglassw <- cluster_spinglass(projection.G,weights =E(projection.G)$weight)
kc_infomapw <- cluster_infomap(projection.G,e.weights =E(projection.G)$weight)

resultados2 <- data.frame(
  Algoritmo = c("kc_fast_greedy", "kc_leading_eigen", "kc_walktrap", "kc_louvain", 
                "kc_label_prop", "kc_spinglass",  "kc_infomap"),
  Modularidad = c(modularity(kc_fast_greedyw), modularity(kc_leading_eigenw),
                  modularity(kc_walktrapw), modularity(kc_louvainw),modularity(kc_label_propw),
                  modularity(kc_spinglassw),  modularity(kc_infomapw))
)

#Resultados tecnicas de agrupamiento con y sin pesos 

resultados_combinados <- merge(resultados1, resultados2, by = "Algoritmo", suffixes = c("", "_W"))
kable(resultados_combinados, caption = " \n Resultados combinados de modularidad para \n diferentes algoritmos de detección de comunidades")
resultados_combinados[which.max(resultados_combinados$Modularidad),]

# gráficos

#Abreviacion de los nombres apellido + inicial 

V(projection.G)$name
nombres_completos <- V(projection.G)$name
Apellidos <- sapply(nombres_completos, obtener_abreviado)


V(projection.G)$label.cex = 0.5
V(projection.G)$label.dist= 0
V(projection.G)$label.color='black'

set.seed(1)
l = layout_with_drl(projection.G)
l = layout_with_fr(projection.G)
l = layout_with_kk(projection.G)
max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])


# Configuración del layout y los márgenes
par(mfrow = c(1, 2), mar = c(1, 1, 4, 1), mgp = c(1, 1, 0), cex.main = 0.8)

# Primer plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#0e5860", 0.01),
     vertex.size = (degree(projection.G))/8, 
     main = "SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE QUIMICA \n kc_louvain - Mod = 0.1681")

# Segundo plot
plot(projection.G, vertex.label = NA, rescale = FALSE,
     xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.2),
     vertex.frame.color = cols[kc_louvain$membership],
     edge.color = adjustcolor("#0e5860", 0.01), main = "")

# Añadir el título centrado en la parte superior
mtext("SUBGRAFO CON AGRUPAMIENTO PARA EL DEP DE QUIMICA",
      outer = TRUE, side = 3, cex = 1, line = -1)

# Añadir el subtitulo justo debajo del título
mtext("kc_louvain - Mod = 0.1681", outer = TRUE, side = 3, cex = 1, line = -2)

# Restaurar el diseño de una sola fila y columna
par(mfrow = c(1, 1))

# asortatividad 

round(assortativity_degree(projection.G),3)
round(assortativity(projection.G, values = strength(projection.G),
                    directed = F, normalized = TRUE))
