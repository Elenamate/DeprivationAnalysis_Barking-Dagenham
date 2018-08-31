library(readr)
library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)
require(maptools)
library(igraph)
library(rgeos)
library(ggplot2)
library(geosphere)
library(tidyr)
library(raster)
library(stplanr)
library(tmap)
library(spdep)

# Load geographical data
London <- readOGR("statistical-gis-boundaries-london/lsoas_London.shp")
UKBNG <- "+init=epsg:27700"
London <- spTransform(London,CRS(UKBNG))
#now, order by LSOA code 
London <- London[order(London$LSOA11CD),]
#Load distance/time list and pois data
times <- read.csv('Data/dis_bustuberail_london_500.csv',sep=',')
lsoas_pois_count3 <- readOGR("statistical-gis-boundaries-london/ESRI/lsoas_pois_count3.shp")
lsoas_pois_count3 <- readOGR("statistical-gis-boundaries-london/ESRI/lsoas_pois_count3.shp")

model_data <- merge(times, lsoas_pois_count3, by.x="zone_dest", by.y='LSOA11C')
model_data <- model_data[,c(1:5,18:21)]

#now chop out the intra-lsoas
model_subset <-  model_data[as.character(model_data$zone_origin)!= model_data$zone_dest,]

# Calculate complete distances between LSOAS
######################################### read neighborgs data
nbs <- read.csv('Data/nbs.csv')
#arrange codes by origin to match
times <- arrange(times, zone_origin, zone_dest)
nbs <- arrange(nbs, Var1, Var2)
#merge list of times with the neighboring list.
times_nbs  <- merge(times, nbs, by.x=c("zone_origin",'zone_dest'), by.y=c('Var1','Var2'))
times_nbs$time <- times_nbs$shortestpath_mins
times_nbs <- times_nbs[,c(1:4,6)]

times_nbs$shortestpath_mins <- as.numeric(times_nbs$shortestpath_mins)
#calculating the walking distance for the neighbouring lsoas without routes
times_subset <- times_nbs[times_nbs$shortestpath_mins==0,]
times_subset <- subset(times_subset,times_subset$zone_origin!=times_subset$zone_dest)
times_subset <- subset(times_subset, times_subset$value!=0)
#multiply subset * 12 (5Km/h)
times_subset$time <- times_subset$crowfly_KM*12
times_nbs2  <- merge(times_nbs, times_subset, by.x=c("zone_origin",'zone_dest'), by.y=c("zone_origin",'zone_dest'), all.x=TRUE)
#keep only the pairs that have an assigned time because those are going to represent the connected pairs on the graph object
times_nbs2$time[is.na(times_nbs2$time)] <- 0
times_nbs2$time <- times_nbs2$time + times_nbs2$shortestpath_mins.x
times_nbs2 <- times_nbs2[,c(1:3,9)]

#find closest neighborg
#1. Create a network with the existing times as weight
idNode1 <- as.character(as.numeric(times_nbs2$zone_origin))
idNode2 <- as.character(as.numeric(times_nbs2$zone_dest))
weight <-as.numeric(times_nbs2$time)

ncol=data.frame(idNode1=as.character(as.numeric(times_nbs2$zone_origin)),idNode2=as.character(as.numeric(times_nbs2$zone_dest)),weight=as.numeric(times_nbs2$time))
sample <- subset(ncol,ncol$weight!=0)
G=graph.data.frame(sample,directed=F)

#Let us compute the shortestpath algorithm to calculate all the times in our graph
length(E(G))
names <- unique(nbs$Var1)
distMatrix <- shortest.paths(G, v=V(G), to=V(G))
distMatrix2 <- distMatrix
rownames(distMatrix) <- names
colnames(distMatrix) <- names
write.csv(distMatrix,'Data/shortestpathsmat.csv')
#convert distMatrix into a list
dist.list <- melt(distMatrix)
write.csv(dist.list,'Data/shortestpathlist.csv')
dist.list <- read.csv('Data/shortestpathlist.csv')

#join the final time to the data 
model_subset <- merge(model_subset, dist.list, by.x=c('zone_origin','zone_dest'), by.y= c('Var1','Var2'))
#####################################


#Calculate the accessibility measures to knowledge based industries (kbi) and to retail and entertainment (ral)
#set up some variables to hold our parameter values in:
beta <- -1
Ej_kbi <- model_subset$kbi_cnt
Ej_kbi_2007 <- model_subset$kb2007_
Ej_ral <- model_subset$ral_cnt
Ej_ral_2007 <- model_subset$rl2007_
time_beta <- model_subset$value^beta

#Calculate Accessibility and 
#head(names)
model_subset$Akb <- round(Ej_kbi*time_beta,5)
model_subset$Akb2007 <- round(Ej_kbi_2007*time_beta,5)
model_subset$Aral <- round(Ej_ral*time_beta,5)
model_subset$Aral2007 <- round(Ej_ral_2007*time_beta,5)

#create a matrix with the accessibility values from LSOA to LSOA adding a column for the total measure per LSOA

modelmat1 <- dcast(model_subset, zone_origin ~ zone_dest, sum, value.var = "Akb", margins=c("zone_origin", "zone_dest"))
modelmat2 <- dcast(model_subset, zone_origin ~ zone_dest, sum, value.var = "Akb2007", margins=c("zone_origin", "zone_dest"))
modelmat3 <- dcast(model_subset, zone_origin ~ zone_dest, sum, value.var = "Aral", margins=c("zone_origin", "zone_dest"))
modelmat4 <- dcast(model_subset, zone_origin ~ zone_dest, sum, value.var = "Aral2007", margins=c("zone_origin", "zone_dest"))
#Take only the column with the total sum for the accessibility
Akb <- modelmat1[1:4835,c(1,4837)]
Akb2007 <- modelmat2[1:4835,c(1,4837)]
Aral <- modelmat3[1:4835,c(1,4837)]
Aral2007 <- modelmat4[1:4835,c(1,4837)]
#change column names
colnames(Akb)[2] <- 'akb'
colnames(Akb2007)[2] <- 'akb2007'
colnames(Aral)[2] <- 'aral'
colnames(Aral2007)[2] <- 'aral2007'

#merge all the metrics in a dataframe
accessibility <- merge(Akb,Akb2007, by.x='zone_origin',by.y='zone_origin')
accessibility <- merge(accessibility,Aral, by.x='zone_origin',by.y='zone_origin')
accessibility <- merge(accessibility,Aral2007, by.x='zone_origin',by.y='zone_origin')

write.csv(accessibility,'Data/accessibility_measures.csv')