####################################
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
library(landsat)
library(snow)

###############################################
# Defining functions
###############################################

convert.to.radiance<-function(...,band){
  #Function to convert DN values of Resourcessat2 LISS-§ sensor into spectral radiance
  #using the scaling method- Lmax values are taken from the meta data files
  Lmax=c(52,47,31.5,7.5)
  Lmin=0
  NDmax=2^10
  NDmin=0
  Lrad<-((Lmax[band]-Lmin)/(NDmax-NDmin))*(...-NDmin)
  return(Lrad*10)
}

convert.to.reflectance<-function(...,date,band,sun_elev){
  #Function to convert radiance to TOA reflectance using the Sun-Earth distance.
  #Values for the at sensor Exo-atm. solar irradiance are taken from Keerthi & Kumar (2011) Table 2 Eo_NL
  band=band
  ESUN=data.frame(band2=1850.05,band3=1588.86,band4=1106.72,band5=241.80)
  ESD<-landsat::ESdist(date)
  sze=(90-sun_elev)*(pi/180)
  ESUNB=as.numeric(ESUN[band])
  Lref<-(pi*...*ESD^2)/(ESUNB*cos(sze))
  return(Lref)
}

toa.calibration<-function(brick,date,sun_elev){
  #Function to calculate the Top of Atmosphere reflectance all bands in a raster stack
  # returns an integer raster
  reflectance=raster::stack()
  for (i in 1:nlayers(brick)){
    print(paste('Calibrating Band',i))
    band<-raster(brick,layer=i)
    rad<-raster::calc(band,function(x){convert.to.radiance(x,band=i)})
    ref<-raster::calc(rad,function(x){convert.to.reflectance(x,band=i,date=date,sun_elev=sun_elev)})
    reflectance<-raster::stack(reflectance,ref)
  }
  
  # Convert to float values to 16bit unsigned keeping 2 decimal digits
  reflectancebit=raster::stack()
  for (i in 1:nlayers(reflectance)){
    band<-raster(reflectance,layer=i)
    ref<-as.integer(band*10000)
    reflectancebit<-raster::stack(reflectancebit,ref)
  }
  
  return(reflectancebit)
  
}


calibration.model<-function(img, samplesize=10000){
  # Function to fit image-to-image calibration model
  # Returns linear model
  sample<-as.data.frame(raster::sampleRegular(img,10000))
  sample<-sample[complete.cases(sample),]
  par<-par(mfrow=c(1,2))
  plot(sample,xlim=c(0,2500),ylim=c(0,2500),main=paste('Band',i))
  model<-lm(master~.,data=sample)
  abline(model)
  abline(a=0,b=1,lty=2)
  plot(sample$master,predict(model),main=paste('Corrected Band',i),xlab='master',ylab='corrected slave',
       xlim=c(0,2500),ylim=c(0,2500))
  abline(a=0,b=1,lty=2)
  summary(model)
  return(model)
}

###############################################
# Defining image files
###############################################
image.file.1<-'data/raster/liss/96-49_Theri_Garhwal_ortho.tif'
image.file.2<-'data/raster/liss/97-49_Theri_Garhwal_ortho.tif'
image.file.3<-'data/raster/liss/97-50_Theri_Garhwal_ortho.tif'

################################################
# Step 1: Performing the TOA convertion
################################################

reflectancebit.1<-toa.calibration(raster::brick(image.file.1),date='2015-10-23',sun_elev=45.731884)
reflectancebit.2<-toa.calibration(raster::brick(image.file.2),date='2015-11-21',sun_elev=37.460142)
reflectancebit.3<-toa.calibration(raster::brick(image.file.3),date='2015-11-21',sun_elev=38.534636)

# Plot false color composits of the corrected LISS-3 satellite images
raster::plotRGB(reflectancebit.1,r=3,g=2,b=1,scale=10000,main='96-49')
raster::plotRGB(reflectancebit.2,r=3,g=2,b=1,scale=10000,main='97-49')
raster::plotRGB(reflectancebit.3,r=3,g=2,b=1,scale=10000,main='97-50')


################################################
# Step 2: Relative Image-to-Image radiometric calibration
################################################
# Since image 1 is taken on a different data an image to image calibration is required
# We use image 2 as master and image 1 as slave image for the calibration
################################################

master<-reflectancebit.2
slave <-reflectancebit.1

# Resample the slave images to match the extent and resolution of the master image
beginCluster()
slave<-raster::resample(slave,master)
endCluster()

# Set value 0 as NA values in both images

NAvalue(master)<-0
NAvalue(slave)<-0

# Extract the overlapping regions of both images

sub.slave<-raster::mask(slave,master)
sub.master<-raster::mask(master,slave)

# Do the image-to-image calibration for each band separetly
calibrated=raster::stack()
for (i in 1:nlayers(master)){
  par(mfrow=c(1,2))
  print(paste('Processing Band',i))
  img<-stack(subset(master,i),subset(slave,i))
  names(img)<-c('master','slave')
  model<-calibration.model(img)
  cal<-raster::calc(subset(slave,i),function(x){as.integer(round(model$coefficients[1]+model$coefficients[2]*x,0))})
  calibrated<-raster::stack(calibrated,cal)
}



################################################
# Step 3: Export the TOA  images
################################################

# This steps requires > 4GB RAM memory
raster::writeRaster(calibrated,      dataType='INT2U',file='data/raster/liss/96-49_Theri_Garhwal_ortho_TOA.tif',overwrite=TRUE)
raster::writeRaster(reflectancebit.2,dataType='INT2U',file='data/raster/liss/97-49_Theri_Garhwal_ortho_TOA.tif',overwrite=TRUE)
raster::writeRaster(reflectancebit.3,dataType='INT2U',file='data/raster/liss/97-50_Theri_Garhwal_ortho_TOA.tif',overwrite=TRUE)

