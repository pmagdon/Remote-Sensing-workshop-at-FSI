####################################
# Workshop: Linking NFI and Remote Sensing data
# FAO TCP/IND/3503 India, 2017
# Dr. Paul Magdon
# pmagdon[at]gwdg.de
# V 1.0
###################################

###############################################
# Importing libaries
###############################################
library(randomForest)
library(caret)
library(raster)

###############################################
#Step 1: Importing the models and images
###############################################

load(file='data/model/prediction_model_liss.RData')
load(file='data/model/prediction_model_sentinel.RData')

liss<-raster::brick('data/raster/liss/Mosaic_theri_Garhwal_TOA_TC_VI.tif')
names(liss)<-c('band2','band3','band4','band5','ndvi','ratio','SAVI','MSAVI','NDWI')

sentinel<-raster::brick('data/raster/sentinel/Mosaic_theri_Garhwal_TOA_TC_VI.tif')
names(sentinel)<-c('band2','band3','band4','band5','band6','band7','band8','band11','band12','ndvi','ratio','SAVI','MSAVI','NDWI','EVI')

fcc<-raster::brick('data/raster/fcc/fcc_theri_garwahl.tif')
NAvalue(sentinel)<-0
NAvalue(liss)<-0
#NAvalue(fcc)<-0

# Resample the fcc map to match the extent and resolution of the prediction maps

beginCluster(n=5)
fcc.liss<-raster::resample(fcc,liss,method ='ngb',progress='text')
fcc.sentinel<-raster::resample(fcc,sentinel, method='ngb',progress='text')
endCluster()

###############################################
#Step 2: Create prediction map using parallel processing
###############################################

beginCluster()
pred_vol_liss <- clusterR(liss, raster::predict,
                          args = list(model = model.liss),
                          progress='text')
pred_vol_sentinel <- clusterR(sentinel, raster::predict,
                              args = list(model = model.sentinel),
                              progress='text')
endCluster()

###############################################
#Step 3: MAsking the areas with no forest cover based on the FCC map

##############################################

#Reclass FCC to binary mal

m <- c(0, 1, 0, 2, 4, 1,  4, 13, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
fcc_mask.liss    =raster::reclassify(fcc.liss,rclmat)
fcc_mask.sentinel=raster::reclassify(fcc.sentinel,rclmat)

pred_vol_liss.masked<-pred_vol_liss*fcc_mask.liss 
pred_vol_sentinel.masked<-pred_vol_sentinel*fcc_mask.sentinel


###############################################
#Step 4: Exporting the final volume maps
##############################################
raster::writeRaster(pred_vol_liss.masked,file='data/raster/maps/LISS_volume_map_theri_garhwal.tif',overwrite=T)
raster::writeRaster(pred_vol_sentinel.masked,file='data/raster/maps/Sentinel_volume_map_theri_garhwal.tif',overwrite=T)




