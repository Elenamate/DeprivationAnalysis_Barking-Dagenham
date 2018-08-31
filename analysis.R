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
library(PerformanceAnalytics)
library(car)
library(spdep)
library(tidyr)

# Load geographical data
lsoas<- read_shape("statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")
UKBNG <- "+init=epsg:27700"
lsoas <- spTransform(lsoas, CRS(UKBNG))

#read data for the regressions.
mydata <- write.csv(mydata, 'Data/final_data.csv')
mydata <- read.csv('Data/final_data.csv')
mydata <- mydata[,c(2:48,51:54)]
boxplot(mydata2$change_good)
mydata2 <- mydata
mydata2 <- na.omit(mydata2)
sd(mydata$change_ral)
summary(mydata[,c(49:52,45:48)])
#correlation charts between the dependent and independent variables

pairs.panels(mydata2[,c(49:52,45:48)], 
             method.col='#6588A7',
             method = "pearson", # correlation method
             hist.col = "#6588A7",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
#Merge data with the geometry for the spatial regression later.
mydata_london <- merge(London_lsoas, mydata, by='LSOA11CD')
mydata_london <- subset( mydata_london,   mydata_london$LAD11CD.y == 'E09000002'| mydata_london$LAD11CD.y == 'E09000001'|mydata_london$LAD11CD.y == 'E09000008'|mydata_london$LAD11CD.y == 'E09000011'|mydata_london$LAD11CD.y == 'E09000012'|mydata_london$LAD11CD.y == 'E09000016'|mydata_london$LAD11CD.y == 'E09000003'|mydata_london$LAD11CD.y == 'E09000033')
#create neighbours list to check Moran's test and perform spatial regression
nbs <- poly2nb(mydata_london, queen= TRUE)
lw <- nb2listw(nbs, zero.policy=TRUE)


#Multiple linear regression analysis with the change of rank in IMD as dependent varriable
#################
Analysis <- lm(difIMD ~ log(changeratio) +  log(change_good) + change_kb+ change_ral, data= mydata)
summary(Analysis)
#vif(Analysis3)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(Analysis)
#To obtain the residuals vs fitted plot with colors per Borough
mydata$res <- Analysis$residuals
mydata$fit <- Analysis$fitted.values
plot <- ggplot(data=mydata, aes(x=fit, y=res, color=LAD11NM)) + geom_point() + xlab('Fitted') + ylab('Residuals') + ggtitle('Model I') + theme(legend.position=c(0.9,0.82))
plot + scale_color_brewer(palette="Set1") 
#Moran's test
mydata_london$residuals <- Analysis$residuals
resnb <- sapply(nbs, function(x) mean(mydata_london$residuals[x]))
# Plot residuals vs 'Mean adjacent residuals' to visualize if there is spatial correlation
plot(mydata_london$residuals, resnb, xlab='Residuals', ylab='Mean adjacent residuals')
#Moran's test
moran.test(mydata_london$residuals, lw, 999,zero.policy=TRUE)
##################

#Multiple linear regression analysis with the change of rank in Income deprivation as dependent varriable
#################
Analysis2 <- lm( difIncome ~ log(changeratio) +  log(change_good) + change_kb+ change_ral  , data= mydata)
summary(Analysis2)
plot(Analysis2)
mydata$res2 <- Analysis2$residuals
mydata$fit2 <- Analysis2$fitted.values
#Moran's test
mydata_london$residuals2 <- Analysis2$residuals
resnb2 <- sapply(nbs, function(x) mean(mydata_london$residuals2[x]))
plot(mydata_london$residuals2, resnb2, xlab='Residuals', ylab='Mean adjacent residuals')
moran.test(mydata_london$residuals2, lw, 999,zero.policy=TRUE)
##################

#Multiple linear regression analysis with the change of rank in Employment deprivation as dependent varriable
#################
Analysis3 <- lm( difEmploy ~ log(changeratio) +  log(change_good) + change_kb+ change_ral, data= mydata)
summary(Analysis3)
plot(Analysis3)
mydata$res3 <- Analysis3$residuals
mydata$fit3 <- Analysis3$fitted.values
mydata_london$residuals3 <- Analysis3$residuals
#Moran's test
resnb3 <- sapply(nbs, function(x) mean(mydata_london$residuals3[x]))
plot(mydata_london$residuals3, resnb3, xlab='Residuals', ylab='Mean adjacent residuals')
moran.test(mydata_london$residuals3, lw, 999,zero.policy=TRUE)
##################

#Multiple linear regression analysis with the change of rank in Education deprivation as dependent varriable
#################
Analysis4 <- lm( difEdu ~ log(changeratio) +  log(change_good) + change_kb+ change_ral, data= mydata)
summary(Analysis6)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(Analysis4)
outlierTest(Analysis4)
mydata$res4 <- Analysis4$residuals
mydata$fit4 <- Analysis4$fitted.values
#Moran's test
mydata_london$residuals4 <- Analysis4$residuals
resnb4 <- sapply(nbs, function(x) mean(mydata_london$residuals4[x]))
plot(mydata_london$residuals4, resnb4, xlab='Residuals', ylab='Mean adjacent residuals')
moran.test(mydata_london$residuals4, lw, 999,zero.policy=TRUE)



coordsW <- coordinates(mydata_london)
#plot them
plot(nbs, coordinates(coordsW), col="red")
#add a map underneath
plot(mydata_london, add=T)

#Spatial regression
#define formulas
f1 <- difIMD~ log(changeratio) +  log(change_good) +change_kb+ change_ral
f2 <-  difIncome ~ log(changeratio) +  log(change_good) + change_kb+ change_ral
f3 <- difEmploy ~ log(changeratio) +  log(change_good) + change_kb+ change_ral
f4 <- difEdu ~ log(changeratio) +  log(change_good) +change_kb+ change_ral
#Run the model using the formulas and the neighbouring list
m1s = lagsarlm(formula=f1, data=mydata_london, lw, tol.solve=1.0e-30,zero.policy=TRUE)
m2s = lagsarlm(formula=f2, data=mydata_london, lw, tol.solve=1.0e-30,zero.policy=TRUE)
m3s = lagsarlm(formula=f3, data=mydata_london, lw, tol.solve=1.0e-30,zero.policy=TRUE)
m4s = lagsarlm(formula=f4, data=mydata_london, lw, tol.solve=1.0e-30,zero.policy=TRUE)

#Call the summary to check the coefficients with  Nagelkerke pseudo-R squared 
summary(m1s,Nagelkerke=T)
#check Moran's test again
mydata_london$residuals <- residuals(m1s)
mydata_london$f <- m1s$fitted.values
plot(mydata_london$residuals,mydata_london$f)
moran.mc(mydata_london$residuals, lw, 999,zero.policy=TRUE)

