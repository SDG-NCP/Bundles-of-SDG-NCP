# Bundles-of-SDG-NCP "RScript for DELPHI Data NCP/SDG  ###
##########################################################
##########################################################
## This code has been developed using as example, part of the 
## "Matrix visualizations with ggplot2" code by Laszlo Gadar (2016)
## Construct the matrix based on the data 

PER <- matrix(c(30,28,14,5,4,0,0,11,0,18,3,23,2,8,7,6,3,
                21,4,2,1,5,1,11,11,16,2,7,8,0,0,3,1,3,
                16,5,1,4,2,3,31,12,9,9,7,13,7,0,1,2,4,
                2,3,22,0,2,0,1,0,2,3,2,2,0,0,3,0,0,
                2,3,5,28,19,0,3,9,13,8,3,9,0,2,1,21,21,
                0,1,7,21,24,0,0,12,2,14,4,6,0,0,2,22,18,
                0,0,17,13,10,0,0,0,2,1,4,0,0,0,0,4,3,
                6,1,1,10,7,1,9,15,11,15,4,4,4,2,7,17,13,
                3,2,2,0,0,14,11,3,2,4,7,0,28,8,5,1,1,
                2,9,3,0,0,30,4,1,1,4,5,3,9,5,5,1,0,
                1,2,3,0,0,27,0,0,2,1,3,2,1,26,0,0,1,
                1,1,2,0,0,8,6,2,11,1,17,0,15,4,2,0,1,
                2,1,0,1,0,0,4,2,4,1,10,3,10,6,23,1,0,
                0,0,1,0,0,0,0,0,0,0,9,1,4,0,1,0,0,
                2,0,10,2,0,3,0,0,0,1,2,0,1,0,0,0,0,
                2,17,0,0,0,0,0,0,0,0,0,3,0,0,11,0,0,
                0,1,0,0,0,1,1,1,1,1,0,1,5,23,0,0,0,
                2,15,0,1,0,3,0,2,1,0,0,7,0,5,15,0,1), byrow=T, nrow=18, ncol=17)

rownames(PER) <- c("Food and feed","Materials and assistance",
                   "Energy", "Medicinal,biochemical and genetic resources",
                   "Learning and inspiration",	"Supporting identities",
                   "Physical and psychological experiences",
                   "Maintenance of options",	"Regulation of climate","Regulation of freshwater quantity, flow and timing",
                   "Regulation of freshwater and coastal water quality",
                   "Regulation of hazards and extreme events","Habitat creation and maintenance",
                   "Regulation of air quality","Regulation of organisms detrimental to humans",
                   "Pollination and dispersal of seeds and other propagules", "Regulation of ocean acidification", 
                   "Protection and decontamination of soils and sediments")
colnames(PER) <- c("SDG1",
                   "SDG2",
                   "SDG3",
                   "SDG4",
                   "SDG5",
                   "SDG6",
                   "SDG7",
                   "SDG8","SDG9","SDG10","SDG11","SDG12","SDG13","SDG14","SDG15","SDG16", "SDG17")


### check for the data

print(PER)

#### Install the reshape2 package

library(reshape2)

longData<-melt(PER)
longData

## 

sizes <- c(4,4,4,1,1,3,2,1,1,1,1,1,1,1,4,2,3,2,2,1,1,1,3,1,1,1,
           4,1,4,4,1,1,4,3,3,4,1,1,2,2,1,1,3,3,1,2,4,4,4,3,1,1,
           1,2,3,1,1,4,4,3,3,1,2,1,4,4,4,3,2,1,2,4,4,1,2,3,4,2,
           3,2,1,4,4,4,3,4,4,2,1,1,1,1,1,4,3,1,4,1,1,4,1,1,1,4,
           2,1,1,4,1,3,2,3,4,1,4,2,2,1,1,1,1,1,2,3,3,1,2,2,2,2,
           3,3,2,4,3,3,1,4,3,4,1,3,3,2,2,1,2,1,2,1,3,1,3,2,4,3,
           1,4,3,2,1,3,3,1,1,3,3,4,2,3,4,3,3,2,1,2,1,1,3,3,3,1,
           4,1,4,4,3,1,1,4,4,2,4,1,1,1,2,2,2,4,4,2,4,1,1,1,1)
value <- c(30,21,16,2,2,6,3,2,1,1,2,2,2,2,28,4,5,3,3,1,1,2,9,2,
           1,1,17,1,15,14,2,1,22,5,7,17,1,2,3,3,2,1,10,5,1,4,28,
           21,13,10,1,2,1,4,5,2,2,19,24,10,7,1,3,1,14,30,27,8,3,
           1,3,11,31,1,3,9,11,4,6,4,1,11,11,12,9,12,15,3,1,2,2,1,
           2,16,9,2,13,2,2,11,2,1,2,11,4,1,1,18,2,9,3,8,14,1,15,
           4,4,1,1,1,1,1,3,7,7,2,3,4,4,4,7,5,3,17,10,9,2,23,8,13,
           2,9,6,4,3,2,3,1,3,1,7,2,7,4,28,9,1,15,10,4,1,5,8,2,2,8,
           5,26,4,6,23,5,7,3,1,3,1,2,7,5,5,2,23,1,11,15,6,1,2,21,
           22,4,17,1,1,1,3,3,4,21,18,3,13,1,1,1,1)
value
sizes

x = data.frame(v1=value, v2=sizes)
x

library(plyr)

data2 <- rename(x, c("v1"="value", "v2"="siz"))
data2
longData
longData<-longData[longData$value!=0,]
longData
longDatat <- cbind(longData, data2)
longDatat
longData <- longDatat 
longData

## data to delete duplicate variable (value)

class(longData)
longData$value <- NULL
longData

## We get a heatmap to explore the matrix

ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(size= value, fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Sustainable Development Goals", y="Categories of Nature's Contributions to People (NCP)", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=45, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

## In ordering the matrix we use the seriate function, installing the seriation package.

library(seriation)

### set seed 

set.seed(2)
o <- seriate(PER, method="BEA_TSP")
o

# Using the longData

longData$Var1 <- factor(longData$Var1, levels=names(unlist(o[[1]][]))) 
longData$Var2 <- factor(longData$Var2, levels=names(unlist(o[[2]][])))

# The second matrix with values

ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Sustainable Development Goals", y="Categories of Nature's Contributions to People (NCP)", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=45, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))+
  geom_text(data=longData, aes(Var2, Var1, label = value, size= 5))

################################################################
# Install the igraph package "install.packages("igraph")"
library(igraph)
#define a graph that is represented as adjacency matrix using the "PER" matrix
g <- graph.incidence(PER, weighted = TRUE)

#cluster with Louvain algorithm.This function implements the multi-level 
# modularity optimization. It is based on the modularity measure and hierarchial approach.
# Authors: Tom Gregorovic & Tamas Nepusz 
# Applying the Louvain for the data

lou <- cluster_louvain(g)
df.lou <- data.frame(lou$names,lou$membership)
df.lou
class(df.lou)
# Install the package dplyr "install.packages("dplyr")"
library(dplyr)

# Using the longData

longData$Var1 <- as.factor(longData$Var1)
longData <- left_join(longData, df.lou, by=c("Var1"="lou.names"))
colnames(longData)[5] <- "Var1_clust"
longData$Var2 <- as.factor(longData$Var2)
longData <- left_join(longData, df.lou, by=c("Var2"="lou.names"))
colnames(longData)[6] <- "Var2_clust"
longData
longData$colour <- ifelse(longData$Var1_clust==longData$Var2_clust, longData$Var1_clust, 0)


longData$Var1 <- factor(longData$Var1, levels=unique(arrange(longData, Var1_clust)[,1]))
longData$Var2 <- factor(longData$Var2, levels=unique(arrange(longData, Var2_clust)[,2]))
#levels must be names
longData$colour <- factor(longData$colour)
#for colours variabes must be factors (discrete scale) otherwise ggplot recognize it continous

ggplot(longData, aes(x = Var2, y = Var1, fill=colour)) + 
  geom_raster() + 
  scale_fill_manual(values=c("grey80", "chartreuse4", "yellow2", "slateblue4", "darkred","darkturquoise", "deeppink4")) +
  labs(x="Sustainable Development Goals", y="Categories of Nature's Contributions to People (NCP)", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=45, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11),
                     legend.text=element_text(size=7))
# Finally we export the data to Excell
# Install package "xlsx"

library("xlsx")

write.xlsx(longData, file = "SDGNCP.xlsx",
           sheetName = "longdata", append = FALSE)

# Now we can perform the analysis in Excell using the long data
longData
