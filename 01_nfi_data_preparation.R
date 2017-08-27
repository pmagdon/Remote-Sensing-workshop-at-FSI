####################################
# Workshop: Linking NFI and Remote Sensing data
# FAO TCP/IND/3503 India, 2017
# Dr. Paul Magdon
# pmagdon[at]gwdg.de
###################################

###############################################
# Setting up the wokspace and installing required libaries
###############################################

#Define the workspace
setwd('D:/write/your/abolute/path/to/the/working/directory/here/')

#Excel file reader
library(xlsx)
# Libaries for plotting
library(ggplot2)
library(gridExtra)
library(ggmap)
library(mapview)
# Libraries for spatial data
library(sp)
library(rgdal)
library(raster)
library(landsat)
library(RStoolbox)
library(broom)
# Libraries for modellinf
library(randomForest)
library(caret)

###############################################
# Defining functions
###############################################

createPlot<-function(center,length,id){
  # Function to create a squarded plot based on the side lenght and the center coordinates
  # Assuming a catesian coordinate system
  ul<-c(center[1]-length/2,center[2]+length/2)
  ur<-c(center[1]+length/2,center[2]+length/2)
  ll<-c(center[1]-length/2,center[2]-length/2)
  lr<-c(center[1]+length/2,center[2]-length/2)
  box<-rbind(ul,ur,lr,ll,ul)
  #return a list of polygons
  return(Polygons(list(Polygon(box)),ID=id))
}


################################################
# Step 1: Importing and analysing the NFI data from Excel file
################################################

nfi.file='data/nfi/Plot_Volume_tehri_11_August.csv'
types<-c('integer','character','character','character',
         'numeric','numeric','factor', 'numeric')

# Import the data from csv file
nfi.data<-read.table(nfi.file,
                     sep=';',
                     stringsAsFactors=F,
                     header=T,
                     colClasses=types)

# Sanitize the column names
names(nfi.data)<-c("sno","year","mapsheet_n","grid_code","lat","lon", "class","plot_volume")

# Only keep plots with complete measurements
nfi.data<-nfi.data[complete.cases(nfi.data),]


# Calculate the per hectare plot volume based on the assumption that the plot size is 1000m² 
expansion.factor<-10000/1000
nfi.data$vol.ha<-nfi.data$plot_volume*expansion.factor

# Select plots belonging to the land use classes forest or scrub 
forest.data<-nfi.data[which(nfi.data$class%in%c('open forest','dense forest','closed forest','scrub')),]
forest.data$class<-factor(forest.data$class)

# Remove all plots with zero volume
forest.data<-forest.data[forest.data$plot_volume>0,]

# Create a plot to check the distribution of the target variable
p1<-ggplot(data=forest.data, aes(vol.ha)) +
    geom_histogram() +
    xlab('Plot volume per ha') +
    ggtitle('Histogram of Volume per ha')

p2<-ggplot(data=forest.data, aes(x=class, y= vol.ha)) +
    geom_boxplot(fill="#4271AE",alpha=0.7) +
    ggtitle('Volume per hectare by forest type') +
    theme_bw() +
    xlab('Forest Stratum') +
    ylab('Volume in cbm/hectare')

p<-grid.arrange(p1, p2, ncol=2)
ggsave(p,file='figures/01_nfi_vol_ha.png')


################################################
# Step 2: Creating a shapefile with the plot boundaries
################################################

nfi.points=SpatialPointsDataFrame(coords=cbind(forest.data$lon,forest.data$lat),
                                  forest.data,
                                  proj4string = CRS("+init=epsg:4326"))

# Reproject sample points to UTM 44 N
nfi.points<-spTransform(nfi.points,CRS("+init=epsg:32644"))

# Create squared polygons for each sample point
tmp=list()
for (i in 1:nrow(nfi.points@data)){
  print(i)
  tmp[[i]]<-createPlot(center=coordinates(nfi.points[i,]),length= sqrt(1000), id=nfi.points[i,]@data$sno)
}        

# Convet to spatial polygons dataframe and attach attribute table
plots=SpatialPolygons(tmp,proj4string =CRS("+init=epsg:32644"))
plots<-SpatialPolygonsDataFrame(plots,nfi.points@data,match.ID = 'sno')

# Creating a map plot
myLocation<- as.numeric(geocode("New Tehri"))
mapData <- get_map(location = myLocation,
                   source = "google",
                   maptype = "terrain",
                   zoom = 9)
map<-ggmap(mapData) + 
        ggtitle('NFI plot design Tehri Garhwal') + 
        geom_point(aes(x=lon, y=lat),data=nfi.data, col='blue') 

mapview(plots)
ggsave(map,file='figures/02_nfi_sample_design.pdf')


# Export the plot boundary geometries to ESRI shapefile
writeOGR(plots, dsn="data/vector/field_plots_utm_44_volume.shp", "field_plots_utm44_volume", 
         driver="ESRI Shapefile",overwrite_layer = T)



