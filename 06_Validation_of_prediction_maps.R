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
library(raster)
library(rgdal)
library(gridExtra)

#Define the workspace
setwd('E:/FSI/workshop/')

###############################################
#Step 1: Importing the plots and predictions
###############################################

plots<-readOGR(dsn="data/vector/field_plots_utm_44_volume.shp")
incorrect.ids<-c(21,24,38,48,49,70,72,93,96,98,113,115,116,120,121,146,150,154,
                 174,175,176,183,186,190,193,195,197,201,202,203,231,232)
plots.cleared<-plots[-which(plots$sno %in% incorrect.ids),]

liss.vol<-raster::raster('data/raster/maps/LISS_volume_map_theri_garhwal.tif')
names(liss.vol)<-'pred_vol'
NAvalue(liss.vol)<-0
sentinel.vol<-raster::brick('data/raster/maps/Sentinel_volume_map_theri_garhwal.tif')
names(sentinel.vol)<-'pred_vol'
NAvalue(sentinel.vol)<-0
###############################################
#Step 2: Plotting predicted vs. observed
###############################################

predicted.sentinel<-raster::extract(sentinel.vol,plots.cleared,fun=mean,na.rm=TRUE,df=TRUE)
validation.sentinel<-cbind(plots.cleared@data,predicted.sentinel)

p1<-ggplot(validation.sentinel,aes(x=vol_ha, pred_vol)) +
  geom_point(shape=1)+
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE) +
  geom_abline(slope=1, intercept=0,
              na.rm = FALSE, show.legend = NA) +
  scale_x_continuous(limits = c(0,500))+
  scale_y_continuous(limits = c(0,500)) +
  xlab('Observed Volume per ha') +
  ylab('Predicted Volume per ha') +
  ggtitle('Validation Sentinel Model')

predicted.LISS<-raster::extract(liss.vol,plots.cleared,fun=mean,na.rm=TRUE,df=TRUE)
validation.LISS<-cbind(plots.cleared@data,predicted.LISS)

p2<-ggplot(validation.LISS,aes(x=vol_ha,y=pred_vol)) +
  geom_point(shape=1)+
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE) +
  geom_abline(slope=1, intercept=0,
              na.rm = FALSE, show.legend = NA) +
  scale_x_continuous(limits = c(0,500))+
  scale_y_continuous(limits = c(0,500)) +
  xlab('Observed Volume per ha') +
  ylab('Predicted Volume per ha') +
  ggtitle('Validation LISSS Model')
plot<-grid.arrange(p1, p2, ncol=2)

ggsave(plot,file='figures/08_map_validation.pdf')
