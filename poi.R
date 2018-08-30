
#This script is to classify and aggregate the points of interest per LSOA

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
head(London)
#Barking boundaries
lsoas_b <- London[grep("^Barking",London@data$LSOA11NM),]
lsoas_b_SF <- st_as_sf(lsoas_b,CRS(UKBNG))

#Points of Interest
#The points of interest are not provided in this Github as you need to ask permission to Ordnance Survey. 
#For data from 2015 they can be requested and downloaded from http://digimap.edina.ac.uk/
#####################################

#read poi data
poi <-read.table("poi_2015/poi_2015/poi.csv", sep="|", header=TRUE)
poi2 <-read.table("poi_2/poi_2504509/poi.csv", sep="|", header=TRUE)
poi2_london <- poi2[grep("Greater London", poi2$Geographic.County),]
poi_london <- poi[grep("Greater London", poi$Geographic.County),]

poi_2007 <- readOGR("Data/poi_2007/london_poi_2007.shp")
poi_2007 <- spTransform(poi_2007,CRS(UKBNG))
poi_2007 <- raster::intersect(poi_2007, London)

#remove pois that not correspond to employment opportunities
pois_to_remove1 <- c(3580804,3580805,3580803,3580807,3580808,3580806, 3180253,3180814,3180254,03180255, 03190257,03190259,03200269,06340453,06340454,
                     6340455,6340433,6340435,6340437,6340456,6340457,6340438,6340459,6340460,06340461,06340443,06340802)
pois_to_remove2 <- c(10530729,10540730,10540733,
                     10540734, 10540739,10540740,10540742,10540743,10540744, 10550746,10550747,10550749,10560751, 10590732)

# The points of interest from 2007 didn't include the leading zero on the PointX reference code.
#Add leading zeros for 2007 data to match

pois_to_remove3 <- paste0("0", pois_to_remove1)
pois_to_remove <- c(pois_to_remove1,pois_to_remove2)
pois_to_remove2007 <- c(pois_to_remove2,pois_to_remove3)
#Subset the pois without the codes to remove
poi2_london_subset <- poi2_london[!poi2_london$PointX.Classification.Code %in% pois_to_remove, ]
poi_london_subset2007 <- poi_2007[!poi_2007$class_code %in% pois_to_remove2007, ]


#Transform POIs into Spatial Object
coordinates(poi2_london_subset) <- ~ Feature.Easting + Feature.Northing
poi2_london_subset@proj4string = CRS(UKBNG)

#Classification of poi in Knowledge based industries and retail and entertaiment
###########################
#dictionary to transform SIC to POI
poi_to_SIC <- read.table("poi_2015/poi_2015/docs/POI_CLASS_TO_SIC_LOOKUP.csv", sep="|", header=TRUE, quote=NULL, colClasses="character", col.names = paste("V", seq_len(16))) 
names(poi_to_SIC) <- c('PointX Classification Code', 'Description', 'First SIC 2003','Second SIC 2003', 'Third SIC 2003', 'Fourth SIC 2003', 'Fifth SIC 2003', 'Sixth SIC 2003', 'Seventh SIC 2003', 'First SIC 2007','Second SIC 2007', 'Third SIC 2007', 'Fourth SIC 2007', 'Fifth SIC 2007', 'Sixth SIC 2007', 'Seventh SIC 2007')
#Knowledge-based industry SIC codes.
# Digital technologies
sic_digital = c(2611,2612,2620,2640,2651,2652,2680,3313,
                5821,5829,6201,6202,6203,6209,6311,
                6312,9511)
# Life Sciences and healthcare
sic_science = c(2110,2120,2660,2670,3250,7211,7500,8610,
                8621,8622,8623,8690)
# Publishing and broadcasting
sic_publishing = c(2630,2670,5811,5812,5813,5814,5814,
                   5819,5911,5912,5913,
                   5914,5920,6010,6020,6110,6120, 6130,6190,
                   6391,6399,7311,7312,7320,7410,7420,
                   9512)
# Other scientific activities of manufacturing
sic_otherScManuf = c(1920,1920,2011,2012,2013,9521,9522,9525,
                     2014,2015,2016,2017,2020,2030,2041,
                     2041,2042,2051,2052,2053,2059,2060,2521,
                     2530,2540,2651,2651,2652,2711,2712,2720,
                     2731,2732,2733,2740,2751,2752,2790,2811,
                     2812,2813,2813,2814,2815,2821,2822,2823,
                     2824,2825,2829,2830,2830,2841,2849,2891,
                     2892,2892,2892,2893,2894,2895,2896,2899,
                     2910,2920,2920,2920,2931,2932,3011,3012,
                     3020,3030,3040,3091,3092,3099,3212,3240,
                     3312,3314,3315,3316,3317)
# Other scientific activity services
sic_otherScServices = c(5110,5121,5122,7111,7112,7112,
                        7112,7120,7219,7220,7490,7490,8541,8542,
                        8542)
sic_kbi = c(sic_digital, sic_science, sic_publishing,sic_otherScManuf, sic_otherScServices)
sic_kbi <- as.data.frame(sic_kbi)
#merge of all codes
sic_poi_kbi1 <- merge(poi_to_SIC, sic_kbi, by.x="First SIC 2007", by.y='sic_kbi')
sic_poi_kbi2 <- merge(poi_to_SIC, sic_kbi, by.x="Second SIC 2007", by.y='sic_kbi')
sic_poi_kbi3 <- merge(poi_to_SIC, sic_kbi, by.x="Third SIC 2007", by.y='sic_kbi')
sic_poi_kbi4 <- merge(poi_to_SIC, sic_kbi, by.x="Fourth SIC 2007", by.y='sic_kbi')
sic_poi_kbi5 <- merge(poi_to_SIC, sic_kbi, by.x="Fifth SIC 2007", by.y='sic_kbi')
sic_poi_kbi6 <- merge(poi_to_SIC, sic_kbi, by.x="Sixth SIC 2007", by.y='sic_kbi')
sic_poi_kbi7 <- merge(poi_to_SIC, sic_kbi, by.x="Seventh SIC 2007", by.y='sic_kbi')
sic_poi_kbi <- rbind(sic_poi_kbi1, sic_poi_kbi2,sic_poi_kbi3,sic_poi_kbi4,sic_poi_kbi5,sic_poi_kbi6,sic_poi_kbi7)
codes <- as.data.frame(unique(sic_poi_kbi$`PointX Classification Code`))
names <- as.data.frame(unique(sic_poi_kbi$Description))
names(codes) <- 'codes'
#Add a 0 in front of the codes for the 2015 data.
codes2 <- lapply(codes, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))
codes2 <- as.data.frame(codes2)
poi_london_subset2007 <- as.data.frame(poi_london_subset2007)
poi2_london_subset <- as.data.frame(poi2_london_subset)
kbi_subset2007 <- merge(poi_london_subset2007, codes, by.x="class_code", by.y='codes')
kbi_subset <- merge(poi2_london_subset, codes2, by.x="PointX.Classification.Code", by.y='codes')

codes$codes
#Retail  and entertainment industry codes

# Food and accomodation
sic_food = c(5510,5520,5530, 5610,
             5621,5629)
# Entertainment
sic_entertainment = c(5914, 8230, 9001,9101,9102,9103,
                      9104,9200,9311, 9312,9313,9319,9319,9302)
# Retail except retail trade of motor vehicles
sic_retail = c(4719,4799,4729,4778,4778,4781,4789,4782,
               4778,4711,4779,4743,4730,4725,4761,4724,
               4753,4771,4741,4775,4754,4723,4776,4772,
               4721,4759,4765,4752,4774,4772,4722,4774,
               4742,4763,4759,4762,4779,4764,4742,4751,
               4726,4777,4791)
sic_ral = c(sic_food, sic_entertainment, sic_retail)
sic_ral <- as.data.frame(sic_ral)
sic_poi_ral1 <- merge(poi_to_SIC, sic_ral, by.x="First SIC 2007", by.y='sic_ral')
sic_poi_ral2 <- merge(poi_to_SIC, sic_ral, by.x="Second SIC 2007", by.y='sic_ral')
sic_poi_ral3 <- merge(poi_to_SIC, sic_ral, by.x="Third SIC 2007", by.y='sic_ral')
sic_poi_ral4 <- merge(poi_to_SIC, sic_ral, by.x="Fourth SIC 2007", by.y='sic_ral')
sic_poi_ral5 <- merge(poi_to_SIC, sic_ral, by.x="Fifth SIC 2007", by.y='sic_ral')
sic_poi_ral6 <- merge(poi_to_SIC, sic_ral, by.x="Sixth SIC 2007", by.y='sic_ral')
sic_poi_ral7 <- merge(poi_to_SIC, sic_ral, by.x="Seventh SIC 2007", by.y='sic_ral')
sic_poi_ral <- rbind(sic_poi_ral1, sic_poi_ral2,sic_poi_ral3,sic_poi_ral4,sic_poi_ral5,sic_poi_ral6,sic_poi_ral7)
codesral <- as.data.frame(unique(sic_poi_ral$`PointX Classification Code`))
names(codesral) <- 'codes'
#Add a 0 in front of the codes for the 2015 data.
codesral2 <- lapply(codesral, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))
codesral2 <- as.data.frame(codesral2)
#subset pois based on the classification codes.
ral_subset2007 <- merge(poi_london_subset2007, codesral, by.x="class_code", by.y='codes')
ral_subset <- merge(poi2_london_subset, codesral2, by.x="PointX.Classification.Code", by.y='codes')
#Convert back to spatial objects
coordinates(kbi_subset) <- ~ Feature.Easting + Feature.Northing
kbi_subset@proj4string = CRS(UKBNG)
coordinates(kbi_subset2007) <- ~ easting + northing
kbi_subset2007@proj4string = CRS(UKBNG)
coordinates(ral_subset) <- ~ Feature.Easting + Feature.Northing
ral_subset@proj4string = CRS(UKBNG)
coordinates(ral_subset2007) <- ~easting + northing
ral_subset2007@proj4string = CRS(UKBNG)
################################################
#Overlay spatial boundaries with pois
#tmap_mode('view')
tm_shape(London) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(kbi_subset) +
  tm_dots('red') 



coordinates(poi_london_subset) <- ~ Feature.Easting + Feature.Northing
poi_london_subset@proj4string = CRS(UKBNG)
coordinates(poi_london_subset2007) <- ~ easting + northing
poi_london_subset2007@proj4string = CRS(UKBNG)

# This counts how many pois are in each LSOA
kbi.count <- colSums(gContains(lsoas, kbi_subset, byid = TRUE))
ral.count <- colSums(gContains(lsoas, ral_subset, byid = TRUE))
kbi2007.count <- colSums(gContains(lsoas, kbi_subset2007, byid = TRUE))
ral2007.count <- colSums(gContains(lsoas, ral_subset2007, byid = TRUE))
#add the count to the lsoas
lsoas@data$kbi.count <- kbi.count
lsoas@data$kbi2007.count <- kbi2007.count
lsoas@data$ral.count <- ral.count
lsoas@data$ral2007.count <- ral2007.count
lsoas@data

write_shape(pois,"statistical-gis-boundaries-london/ESRI/lsoas_pois_count.shp")

