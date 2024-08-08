##### Settear el directorio de trabajo

setwd("C:/Users/HP/Downloads/Taller3a")

#---------------------BALI 

library(intergraph)
library(igraph)
load("C:/Users/HP/Downloads/Taller3a/Bali.rda")
iBali <- asIgraph(Bali)
Coord <- tkplot(iBali, vertex.size=3,
                vertex.label=V(iBali)$role,
                vertex.color="darkgreen")
# Edit plot in Tk graphics window before
# running next two commands.
MCoords <- tkplot.getcoords(Coord)
tk_close(Coord)
plot(iBali, layout=MCoords, vertex.size=5,
     vertex.label=NA, vertex.color="lightblue")


library(networkD3)
src <- c("A","A","B","B","C","E")
target <- c("B","C","C","D","B","C")
net_edge <- data.frame(src, target)
simpleNetwork(net_edge)


net_D3 <- simpleNetwork(net_edge)
saveNetwork(net_D3,file = 'Net_test1.html',
            selfcontained=TRUE)


iBali_edge <- get.edgelist(iBali)
iBali_edge <- iBali_edge - 1
iBali_edge <- data.frame(iBali_edge)
iBali_nodes <- data.frame(NodeID=as.numeric(V(iBali)-1),
                          Group=V(iBali)$role,
                          Nodesize=(degree(iBali)))
forceNetwork(Links = iBali_edge, Nodes = iBali_nodes,
             Source = "X1", Target = "X2",
             NodeID = "NodeID",Nodesize = "Nodesize",
             radiusCalculation="Math.sqrt(d.nodesize)*3",
             Group = "Group", opacity = 0.8,
             legend=TRUE)

net_D3 <- forceNetwork(Links = iBali_edge,
                       Nodes = iBali_nodes,
                       Source = "X1", Target = "X2",
                       NodeID = "NodeID",Nodesize = "Nodesize",
                       radiusCalculation="Math.sqrt(d.nodesize)*3",
                       Group = "Group", opacity = 0.8,
                       legend=TRUE)

saveNetwork(net_D3,file = 'Net_test2.html',
            selfcontained=TRUE)

library(visNetwork)
iBali_edge <- get.edgelist(iBali)
iBali_edge <- data.frame(from = iBali_edge[,1],
                         to = iBali_edge[,2])
iBali_nodes <- data.frame(id = as.numeric(V(iBali)))
visNetwork(iBali_nodes, iBali_edge, width = "100%")


iBali_nodes$group <- V(iBali)$role
iBali_nodes$value <- degree(iBali)
net <- visNetwork(iBali_nodes, iBali_edge,
                  width = "100%",legend=TRUE)
net <- visLegend(net)
net
visOptions(net,highlightNearest = TRUE)


net <- visNetwork(iBali_nodes, iBali_edge,
                  width = "100%",legend=TRUE)
net <- visOptions(net,highlightNearest = TRUE)
net <- visInteraction(net,navigationButtons = TRUE)


library(htmlwidgets)
saveWidget(net, "Net_test3.html")

library(statnetWeb)
run_sw()


########################

library(devtools)
install_github("gastonstat/arcdiagram")

library(arcdiagram)
library(igraph)
library(intergraph)
load("C:/Users/HP/Downloads/Taller3a/Simpsons.rda")
iSimp <- asIgraph(Simpsons)
simp_edge <- get.edgelist(iSimp)

arcplot(simp_edge)

s_grp <- V(iSimp)$group
s_col = c("#a6611a", "#dfc27d","#80cdc1","#018571")
cols = s_col[s_grp]
node_deg <- igraph::degree(iSimp)


arcplot(simp_edge, lwd.arcs=2, cex.nodes=node_deg/2,
        labels=V(iSimp)$vertex.names,
        col.labels="darkgreen",font=1,
        pch.nodes=21,line=1,col.nodes = cols,
        bg.nodes = cols, show.nodes = TRUE)


######## FIFA

library(statnet)
library(circlize)
load("C:/Users/HP/Downloads/Taller3a/FIFA_Nether.rda")
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
names <- c("GK1","DF3","DF4","DF5","MF6",
           "FW7","FW9","MF10","FW11","DF2","MF8")
rownames(FIFAm) = names
colnames(FIFAm) = names
FIFAm

xtable(FIFAm)


FIFAm[FIFAm < 10] <- 0
FIFAm

xtable(FIFAm)

chordDiagram(FIFAm)

grid.col <- c("#AA3939",rep("#AA6C39",4),
              rep("#2D882D",3),rep("#226666",3))
chordDiagram(FIFAm,directional = TRUE,
             grid.col = grid.col,
             order=c("GK1","DF2","DF3","DF4","DF5",
                     "MF6","MF8","MF10","FW7",
                     "FW9","FW11"))

####### FIFA 2

load("C:/Users/HP/Downloads/Taller3a/FIFA_Nether.rda")
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
colnames(FIFAm) <- c("GK1","DF3","DF4","DF5",
                     "MF6","FW7","FW9","MF10",
                     "FW11","DF2","MF8")
rownames(FIFAm) <- c("GK1","DF3","DF4","DF5",
                     "MF6","FW7","FW9","MF10",
                     "FW11","DF2","MF8")

palf <- colorRampPalette(c("#669999", "#003333"))
heatmap(FIFAm[,11:1],Rowv = NA,Colv = NA,col = palf(60),
        scale="none", margins=c(11,11) )

###### FIFA 3

edgeMaker <- function(whichRow, layoutCoordinates, adjacencyList, len = 100, curved = TRUE) {
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]
  graphCenter <- colMeans(layoutCoordinates)
  bezierMid <- c(fromC[1], toC[2])
  distance1 <- sum((graphCenter - bezierMid)^2)
  
  if (distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)) {
    bezierMid <- c(toC[1], fromC[2])
  }
  
  bezierMid <- (fromC + toC + bezierMid) / 3
  
  if (!curved) {
    bezierMid <- (fromC + toC) / 2
  }
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),
                            c(fromC[2], bezierMid[2], toC[2]),
                            evaluation = len))
  edge$Sequence <- 1:len
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  
  return(edge)
}


library(sna)
library(ggplot2)
library(Hmisc)

load("C:/Users/HP/Downloads/Taller3a/FIFA_Nether.rda")
fifa <- FIFA_Nether
fifa.edge <- as.edgelist.sna(fifa,attrname='passes')
fifa.edge <- data.frame(fifa.edge)
names(fifa.edge)[3] <- "value"
fifa.edge <- fifa.edge[fifa.edge$value > 9,]
adjacencyList <- fifa.edge


layoutCoordinates <- gplot(network(fifa.edge))

allEdges <- lapply(1:nrow(fifa.edge), function(i) {
  edgeMaker(i, layoutCoordinates, fifa.edge, len = 500, curved = TRUE)
})

allEdges <- do.call(rbind, allEdges)


new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0,0,-1,-1),
                                         unit = "lines",
                                         valid.unit = 3L,
                                         class = "unit")

zp1 <- ggplot(allEdges)
zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group,
                           colour=Sequence, size=-Sequence))
zp1 <- zp1 + geom_point(data =
                          data.frame(layoutCoordinates),
                        aes(x = x, y = y),
                        size = 4, pch = 21,
                        colour = "black", fill = "gray")
zp1 <- zp1 + scale_colour_gradient(low = gray(0),
                                   high = gray(9/10),
                                   guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1.5),
                        guide = "none")
zp1 <- zp1 + new_theme_empty
print(zp1)
