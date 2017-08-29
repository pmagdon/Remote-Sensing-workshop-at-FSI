###################################
# Workshop: Linking NFI and Remote Sensing data
# FAO TCP/IND/3503 India, 2017
# Dr. Paul Magdon
# pmagdon[at]gwdg.de
# V 1.0
###################################

###############################################
# Importing libaries
###############################################

library(raster)
library(rgdal)
library(gridExtra)
library(ggplot2)
library(ggmap)
library(broom)



################################################
# Step 1: Import the prepared NFI data for Theri Garwahl
################################################

plots<-readOGR(dsn="data/vector/field_plots_utm_44_volume.shp")

# Some of the plots in the dataset seem to have errors as they have 0 volume even if trees can be found
# The plots with 0 volume were all checked visualy (e.g. mapview(plots[plots@data$sno==36,]))
# The plots identified as incorrect are removed from the dataset



#incorrect.ids<-c(20,23,36,45,64,66,85,88,90,105,107,108,112,113,135,139,143,
#                 161,162,163,169,170,173,177,180,182,184,188,189,190,217,218,106,160)


#incorrect.ids<-c(21,24,38,48,49,70,72,93,96,98,113,115,116,120,121,146,150,154,
#                 174,175,176,183,186,190,193,195,197,201,202,203,231,232)

plots.cleared<-plots[plots$vol_ha>0,]
#Remote the false plots

#plots.cleared<-plots[-which(plots$sno %in% incorrect.ids),]


################################################
# Step 2: Import pre-processed LISS and Sentinel2 images 
################################################


liss<-raster::brick('data/raster/liss/Mosaic_theri_Garhwal_TOA_TC_VI.tif')
names(liss)<-c('band2','band3','band4','band5','ndvi','ratio','SAVI','MSAVI','NDWI')

sentinel<-raster::brick('data/raster/sentinel/Mosaic_theri_Garhwal_TOA_TC_VI.tif')
names(sentinel)<-c('band2','band3','band4','band5','band6','band7','band8','band11','band12','ndvi','ratio','SAVI','MSAVI','NDWI','EVI')

NAvalue(sentinel)<-0
NAvalue(liss)<-0

################################################
# Step 3: Extract the pixel values within the field plots 
################################################

# Extract plot values as mean value of all pixels having their center within the plot boundary from LISS
liss.train<-raster::extract(liss,plots.cleared,fun=mean,na.rm=TRUE,
                            df=TRUE,progress='text')

# Attach the attributes from the nfi field data
liss.train$SNo<-plots.cleared@data$sno
liss.train$vol_ha<-plots.cleared@data$vol_ha
liss.train$landcover<-plots.cleared@data$class

# Remove the plots that are not covered by the LISS data (5 removed)
liss.train<-liss.train[complete.cases(liss.train),]


# Extract plot values as mean value of all pixels having their center within the plot boundary from Sentinel
sentinel.train<-raster::extract(sentinel,plots.cleared,fun=mean,na.rm=TRUE,df=TRUE)

sentinel.train$SNo<-plots.cleared@data$sno
sentinel.train$vol_ha<-plots.cleared@data$vol_ha
sentinel.train$landcover<-plots.cleared@data$class

# Remove the plots that are not covered by the Sentinel data (7 removed)
sentinel.train<-sentinel.train[complete.cases(sentinel.train),]



################################################
# Step 4: Analysing the training data 
################################################

p1<-ggplot(sentinel.train, aes(y= vol_ha, x= ndvi, label=SNo))+
  geom_point() +
  geom_text(aes(label=ifelse(ndvi>0,as.character(SNo),'')),hjust=0,vjust=0)+
  ggtitle('Sentinel')

p2<-ggplot(liss.train, aes(y= vol_ha, x= ndvi,label=SNo))+
  geom_point() +
  geom_text(aes(label=ifelse(ndvi>0,as.character(SNo),'')),hjust=0,vjust=0)+
  ggtitle('LISS')

grid.arrange(p1, p2, ncol=2)

# The plots following plots are suspicous
mapview(plots[plots$sno==173,]) # A building
mapview(plots[plots$sno==114,]) # Might be snow
# mapview(plots[plots$sno==32,]) # No trees
false.ids<-c(173,114,32)
sentinel.train<-sentinel.train[-which(sentinel.train$SNo %in% false.ids),]
liss.train<-liss.train[-which(liss.train$SNo %in% false.ids),]

p1<-ggplot(data=liss.train, aes(x=landcover, y= ndvi)) +
  geom_boxplot(fill="#4271AE",alpha=0.7) +
  scale_y_continuous(limits= c(0,1)) +
  ggtitle('NDVI by forest type LISS 3 TOA') +
  theme_bw() +
  xlab('Forest Stratum') +
  ylab('NDVI')

p2<-ggplot(data=sentinel.train, aes(x=landcover, y= ndvi)) +
  geom_boxplot(fill="#4271AE",alpha=0.7) +
  scale_y_continuous(limits= c(0,1)) +
  ggtitle('NDVI by forest type Sentinel 2 BOA') +
  theme_bw() +
  xlab('Forest Stratum') +
  ylab('NDVI')

grid.arrange(p1, p2, ncol=2)


################################################
# Step 5: Savint the training data 
################################################


save(liss.train,file='data/model/liss_training.RData')
save(sentinel.train,file='data/model/sentinel_train.RData')

