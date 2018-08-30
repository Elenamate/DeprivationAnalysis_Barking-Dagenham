#This script is to create the metrics of accessibility to schools in London

library(readr)
library(ggplot2)
library(reshape2)
library(devtools)
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(tmaptools)
library(tmap)
library(rgeos)
library(OpenStreetMap)
library(geojsonio)
library(classInt)
library(raster)
library(rgdal)
library(dplyr)
library(data.table)
library(igraph)
require(broom)

# Load London Boundaries
London <- readOGR("statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")
UKBNG <- "+init=epsg:27700"
London <- spTransform(London,CRS(UKBNG))

#Load population census data for 2007 and 2015
pop2007 <- read.csv('Data/pop2007.csv',sep=';')
pop2007 <- pop2007[,c(1,3)]
colnames(pop2007)[2] <- 'pop2007'  
pop2015 <- read.csv('Data/pop_estimates_2015_lsoa.csv',sep=';')
pop2015 <- pop2015[,c(1,4)]
colnames(pop2015)[2] <- 'pop2015'  

#Schools
#####################################
#Load school data for the same years. Primary and secondary schools
School_2007_primary <- read.csv('Data/Schools_2007_primary.csv',sep=';')
School_2007_secondary <- read.csv('Data/Schools_2007_secondary.csv',sep=';')
School_2015_primary <- read.csv('Data/Schools_2015_primary.csv',sep=';')
School_2015_secondary <- read.csv('Data/Schools_2015_secondary.csv',sep=';')

#Load postcode directory
postcode <- read.csv('Data/London_postcode.csv',sep=';')
postcode_loc <- postcode[,c('pcd',"pcd2","pcds","lsoa01","lsoa11","lat","long")]

#Match postcode with schools and project
school1_2007_1 <- merge(postcode_loc, School_2007_primary, by.x="pcd", by.y='pcode')
school1_2007_2 <- merge(postcode_loc, School_2007_primary, by.x="pcd2", by.y='pcode')
school1_2007_3 <- merge(postcode_loc, School_2007_primary, by.x="pcds", by.y='pcode')
school1_2007 <- rbind(school1_2007_1,school1_2007_2,school1_2007_3)

school2_2007_1 <- merge(postcode_loc, School_2007_secondary, by.x="pcd", by.y='institution.postcode')
school2_2007_2 <- merge(postcode_loc, School_2007_secondary, by.x="pcd2", by.y='institution.postcode')
school2_2007_3 <- merge(postcode_loc, School_2007_secondary, by.x="pcds", by.y='institution.postcode')
school2_2007 <- rbind(school2_2007_1,school2_2007_2,school2_2007_3)

school1_2015_1 <- merge(postcode_loc, School_2015_primary, by.x="pcd", by.y='PCODE')
school1_2015_2 <- merge(postcode_loc, School_2015_primary, by.x="pcd2", by.y='PCODE')
school1_2015_3 <- merge(postcode_loc, School_2015_primary, by.x="pcds", by.y='PCODE')
school1_2015 <- rbind(school1_2015_1,school1_2015_2,school1_2015_3)

school2_2015_1 <- merge(postcode_loc, School_2015_secondary, by.x="pcd", by.y='PCODE')
school2_2015_2 <- merge(postcode_loc, School_2015_secondary, by.x="pcd2", by.y='PCODE')
school2_2015_3 <- merge(postcode_loc, School_2015_secondary, by.x="pcds", by.y='PCODE')
school2_2015 <- rbind(school2_2015_1,school2_2015_2,school2_2015_3)

#remove duplicates
school1_2007 <- school1_2007[!duplicated(school1_2007), ]
school2_2007 <- school2_2007[!duplicated(school2_2007), ]
school1_2015 <- school1_2015[!duplicated(school1_2015), ]
school2_2015 <- school2_2015[!duplicated(school2_2015), ]
                    


###########################

#Transform schools into Spatial Object and transform to UKBNG coordinate system
coordinates(school1_2007) <- ~ long + lat
proj4string(school1_2007) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
school1 <- spTransform(school1_2007,UKBNG)
coordinates(school2_2007) <- ~ long + lat
proj4string(school2_2007) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
school2 <- spTransform(school2_2007,UKBNG)
coordinates(school1_2015) <- ~ long + lat
proj4string(school1_2015) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
school1_2015 <- spTransform(school1_2015,UKBNG)
coordinates(school2_2015) <- ~ long + lat
proj4string(school2_2015) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
school2_2015 <- spTransform(school2_2015,UKBNG)


#Create buffer around schools that contains attribute data
school1.buff <- gBuffer(school1, width = 700,byid=TRUE)
school2.buff <- gBuffer(school2, width = 700,byid=TRUE)
school1_15.buff <- gBuffer(school1_2015, width = 700,byid=TRUE)
school2_15.buff <- gBuffer(school2_2015, width = 700,byid=TRUE)

#Intersect buffers with Lsoas
INTER1 <-raster::intersect(London, school1.buff)
INTER2 <-raster::intersect(London, school2.buff)
INTER1_15 <-raster::intersect(London, school1_15.buff)
INTER2_15 <-raster::intersect(London, school2_15.buff)

INTER1 <- as.data.frame(INTER1)
INTER2 <- as.data.frame(INTER2)
INTER1_15 <- as.data.frame(INTER1_15)
INTER2_15 <- as.data.frame(INTER2_15)
#Calculate total number of students
#########################################
#Count how many LSOAs each school influences and divide num of students by that number.
freq1 <-as.data.frame(table(INTER1$URN))
freq2 <-as.data.frame(table(INTER2$URN))
freq1_15 <-as.data.frame(table(INTER1_15$URN))
freq2_15 <-as.data.frame(table(INTER2_15$URN))


#merge the frequency of the schools with the previous intersect. 
INTER1 <- merge(INTER1,freq1,by.x='URN',by.y='Var1',all=TRUE)
INTER2 <- merge(INTER2,freq2,by.x='URN',by.y='Var1',all=TRUE)
INTER1_15 <- merge(INTER1_15,freq1_15,by.x='URN',by.y='Var1',all=TRUE)
INTER2_15 <- merge(INTER2_15,freq2_15,by.x='URN',by.y='Var1',all=TRUE)

#divide the total number of pupils by the frequency to obtain estimates
INTER1$TOTPUPS1_2007 <- round(INTER1$totpups/INTER1$Freq,0) 
INTER2$TOTPUPS2_2007 <- round(INTER2$number.of.pupils.on.roll..all.ages./INTER2$Freq,0) 
INTER1_15$TOTPUPS1_2015 <- round(INTER1_15$TOTPUPS/INTER1_15$Freq,0) 
INTER2_15$TOTPUPS2_2015 <- round(INTER2_15$TOTPUPS/INTER2_15$Freq,0) 
list <- as.data.frame(unique(INTER1$LSOA11CD))
# aggregate students by lsoa
num_students1_2007 <- INTER1 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS1_2007))

num_students2_2007 <- INTER2 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS2_2007))

num_students1_2015 <-INTER1_15 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS1_2015))

num_students2_2015 <- INTER2_15 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS2_2015))

#calculate total number of students per lsoa
totalpups_2007 <- merge(num_students1_2007, num_students2_2007, by.x='LSOA11CD', by.y='LSOA11CD', all=TRUE)
totalpups_2007[is.na(totalpups_2007)] <- 0
totalpups_2007$total <- totalpups_2007$pups.x + totalpups_2007$pups.y

totalpups_2015 <- merge(num_students1_2015, num_students2_2015, by.x="LSOA11CD", by.y='LSOA11CD', all=TRUE)
totalpups_2015[is.na(totalpups_2015)] <- 0
totalpups_2015$total <- totalpups_2015$pups.x + totalpups_2015$pups.y
#########################
pups2007 <-totalpups_2007[,c(1,4)] 
colnames(pups2007)[2] <- 'pups2007'
pups2015 <-totalpups_2015[,c(1,4)] 
colnames(pups2015)[2] <- 'pups2015'

#Subset schools based on  cva measure
good1<-  subset(INTER1, as.numeric(as.character(CVAmeas)) >= 100.6)
good2 <-  subset(INTER2,as.numeric(as.character(school2_2007$KS2..KS4.contextual.value.added.measure))>= 1012.7)
good1_15 <-  subset(INTER1_15, as.numeric(as.character(school1_2015$OVAMEAS))>= 100.6)
good2_15 <-  subset(INTER2_15,as.numeric(as.character(school2_2015$VAMEA))>= 1012.7)

# aggregate students in good  ed by lsoa
 num_good1_2007 <- good1 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS1_2007))

num_good2_2007 <- good2 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS2_2007))

num_good1_2015 <- good1_15 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS1_2015))

num_good2_2015 <- good2_15 %>% 
  group_by(LSOA11CD) %>% 
  summarise(pups = sum(TOTPUPS2_2015))

#calculate total number of students per lsoa
good_2007 <- merge(num_good1_2007, num_good2_2007, by.x="LSOA11CD", by.y='LSOA11CD', all=TRUE)
good_2007[is.na(good_2007)] <- 0
good_2007$good2007 <- good_2007$pups.x + good_2007$pups.y

good_2015 <- merge(num_good1_2015, num_good2_2015, by.x="LSOA11CD", by.y='LSOA11CD', all=TRUE)
good_2015[is.na(good_2015)] <- 0
good_2015$good2015 <- good_2015$pups.x + good_2015$pups.y

#############################################
good_2007 <-good_2007[,c(1,4)] 
good_2015 <-good_2015[,c(1,4)] 

#merge student data and pop data with the shapefile
London_data <- merge(London,pups2007,by.x='LSOA11CD', by.y='LSOA11CD')
London_data <- merge(London_data,pups2015,by.x='LSOA11CD', by.y='LSOA11CD')
London_data <- merge(London_data,pop2007,by.x='LSOA11CD', by.y='Codes')
London_data <- merge(London_data,pop2015,by.x='LSOA11CD', by.y='Area.Codes')
London_data <- merge(London_data,good_2007,by.x='LSOA11CD', by.y='LSOA11CD')
London_data <- merge(London_data,good_2015,by.x='LSOA11CD', by.y='LSOA11CD')
London_data$pups2007 [is.na(London_data$pups2007)] <- 0
London_data$pups2015 [is.na(London_data$pups2015)] <- 0
London_data$good2007 [is.na(London_data$good2007)] <- 0
London_data$good2015 [is.na(London_data$good2015)] <- 0

#Calculate ratio student/pop
London_data$ratio_stu_pop2007 <- (London_data$pups2007/London_data$pop2007)
London_data$ratio_stu_pop2015 <-as.numeric(London_data$pups2015)/as.numeric(London_data$pop2015)

#save file as a csv
l <- as.data.frame(London_data)
write.csv(l,'Data/Education_meas.csv')


#Map schools
tmap_mode('view')
tm_shape(London) +
  tm_polygons(col = NA, alpha = 0.8) +
  tm_shape(school1.buff) +
  tm_polygons('blue', alpha = 0.5)+
  tm_layout("Buffer around existing primary schools in 2007", inner.margins=c(0,0,.1,0), title.size=1)
