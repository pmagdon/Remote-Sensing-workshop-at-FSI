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
library(RStoolbox)
library(gridExtra)
library(mapview)


###############################################
# Defining functions
###############################################


NDVI<-function(red,nir){
  # Function to calculate the Normalized Vegetation Index (NDVI)
  ndvi=(nir-red)/(nir+red)
  names(ndvi)<-'NDVI'
  return(ndvi)
}

RATIO<-function(red,nir){
  # Function to calculate the Ratio vegetation index
  ratio=nir/red
  names(ratio)<-'RATIO'
  return(ratio)
}

SAVI<-function(red,nir,L=0.5){
  # Function to calculate the Soil Adjusted Vegetation Index
  savi=((nir-red)/(nir+red+L))*(1+L)
  names(savi)<-'SAVI'
  return(savi)
}

MSAVI<-function(red,nir){
  # Function to calculate the Modiefied Soil Adjusted Vegetation Index
  msavi<-(2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))/2
  names(msavi)<-'MSAVI'
  return(msavi)
}

NDWI<-function(green,swir){
  # Function to calculate the Normalized Difference Water Index
  ndwi<-(green-swir)/(green+swir)
  names(ndwi)<-'NDWI'
  return(ndwi)
}

EVI<-function(blue,red,nir,L=1,C1=6,C2=7.5,G=2.5){
  # Function to calculate the Enhanced Vegetation Index
  evi<-G*((nir-red)/(nir+C1*red-C2*blue+L))
  names(evi)<-'EVI'
  return(evi)
}


###############################################
# Step 1 Calculating the vegetation indices for the LISS mosaic
##############################################

#Import the mosaic
mosaic.file<-'data/raster/liss/Mosaic_theri_Garhwal_TOA_TC.tif'

liss<-raster::brick(mosaic.file)
names(liss)<-c('green','red','nir','swir')
crs(liss) <- "+init=epsg:32644" 

ndvi<-NDVI(red=liss$red,nir=liss$nir)
ratio<-RATIO(red=liss$red,nir=liss$nir)
savi<-SAVI(red=liss$red,nir=liss$nir)
ndwi<-NDWI(green=liss$green,swir=liss$swir)
msavi<-MSAVI(red=liss$red,nir=liss$nir)

# Check results with interactive map
mapview(ndvi,legend=T)

###############################################
# Step 2 Create a new layer stack with all bands and export GTIFF
##############################################

image<-raster::stack(list(liss,ndvi,ratio,savi,ndwi,msavi))
raster::writeRaster(image,
                    file='data/raster/liss/Mosaic_theri_Garhwal_TOA_TC_VI.tif',
                    overwrite=T,progress='text')

