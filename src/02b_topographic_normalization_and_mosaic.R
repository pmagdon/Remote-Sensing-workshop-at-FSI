###################################
# Workshop: Linking NFI and Remote Sensing data
# FAO TCP/IND/3503 India, 2017
# Dr. Paul Magdon
# pmagdon[at]gwdg.de
###################################


###############################################
# Importing libaries
###############################################

library(raster)
library(rgdal)
library(RStoolbox)
library(gridExtra)
library(ggplot2)


###############################################
# Step 1 Topographic normalization
##############################################

# The topographic correction is done with the open source software SAGA GIS www.saga-gis.org/
# The function saga_cmd ta_lighting  was used to correct for illumination differences caused by the
# topographie. To model the topography the digital surface model (DSM) AW3D30 V1.1 provided by 
# Japan Aerospace Exploration Agency (JAXA) with a spatial resolution of 30m was used.
# http://www.eorc.jaxa.jp/ALOS/en/aw3d30/index.htm
# The Minnaert with slope correction algorithm (Law & Nichol(2004), ISPRS 2004 International Society for Photogrammetry and Remote Sensing]
# was used for topographic correction in SAGA GIS 

# Define file names of the topographically corrected TOA images
image1.cor<-brick('data/raster/liss/97-49_Theri_Garhwal_ortho_TOA_TC.tif')
image2.cor<-brick('data/raster/liss/97-50_Theri_Garhwal_ortho_TOA_TC.tif')
image3.cor<-brick('data/raster/liss/96-49_Theri_Garhwal_ortho_TOA_TC.tif')

# Set NA value to 0
NAvalue(image1.cor)<-0
NAvalue(image2.cor)<-0
NAvalue(image3.cor)<-0

###############################################
# Step 2 Merging the pre-processed tiles into one image mosaic
##############################################

# Merge rasters
image.cor<-raster::merge(image1.cor,image2.cor,image3.cor,progress='text')
crs(image.cor) <- "+init=epsg:32644" 


# Export single bands raster as GeoTIFF
writeRaster(image.cor,file='data/raster/liss/Mosaic_theri_Garhwal_TOA_TC.tif',
              overwrite=TRUE,progress='text')

###############################################
# Step 3 Merging the original raw LISS tiles into one image mosaic for comparison
##############################################

# Define file names of the raw images
image1<-brick('data/raster/liss/97-49_Theri_Garhwal_ortho.tif')
image2<-brick('data/raster/liss/97-50_Theri_Garhwal_ortho.tif')
image3<-brick('data/raster/liss/96-49_Theri_Garhwal_ortho.tif')

# Set NA values
NAvalue(image1)<-0
NAvalue(image2)<-0
NAvalue(image3)<-0

# Merge raster
image<-raster::merge(image1,image2,image3,progress='text')

# Set the CRC
crs(image) <- "+init=epsg:32644" 

# Export single bands to GeoTIFF
writeRaster(image,file='data/raster/liss/Mosaic_theri_Garhwal_ortho_.tif',
              overwrite=TRUE,progress='text')

###############################################
# Step 3 Plotting the differences before and after pre-processing
##############################################


p1<-ggRGB(image.cor, r = 3, g = 2, b = 1, stretch = "lin", limits = c(0,3000), clipValues = "limits",
      quantiles = c(0.02, 0.98))+
      ggtitle('Mosaic of the pre-processed LISS 3 images')


p2<-ggRGB(image, r = 3, g = 2, b = 1, stretch = "lin", limits = c(0,2000), clipValues = "limits",
        quantiles = c(0.02, 0.98))+
        ggtitle('Mosaic of the LISS 3 raw images')

ggsave(p1,file='figures/Mosaic_LISS3_preprocessed.png')
ggsave(p2,file='figures/Mosaic_LISS3_original.png')
