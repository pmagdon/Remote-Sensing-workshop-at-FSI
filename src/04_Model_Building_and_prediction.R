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


###############################################
#Step 1: Importing the training data
###############################################

load(file='data/model/liss_training.RData')
load(file='data/model/sentinel_train.RData')


###############################################
#Step 2: Analysing the variable importance for feature selection
###############################################

control <- rfeControl(functions=rfFuncs, method="cv",
                      number=10)

results.sentinel <- rfe(x=sentinel.train[
  ,c('ndvi','ratio','SAVI','MSAVI','NDWI','EVI','band2',
     'band3','band4','band5','band6','band7','band8',
     'band11','band12')], 
  y=sentinel.train$vol_ha, sizes=c(1:10), 
  rfeControl=control)

plot(results.sentinel, type=c("g", "o"),main='Backward Feature Selection Sentinel',xlab=' No. Variables',ylim=c(80,110))
predictors(results.sentinel)


control <- rfeControl(functions=rfFuncs, method="cv", number=10)

results.liss <- rfe(x=liss.train[,c('ndvi','ratio','SAVI','MSAVI','NDWI','band2','band3','band4','band5')], 
               y=liss.train$vol_ha, sizes=c(1:10), rfeControl=control)
plot(results.liss, type=c("g", "o"),main='Backward Feature Selection LISS',xlab=' No. Variables',ylim=c(80,110))
predictors(results.liss)

###############################################
#Step 3: Building the final prediction model
###############################################

model.sentinel<-randomForest(vol_ha~.,data=sentinel.train[,c('vol_ha','band4','band5')],
                             importance=T,ntree=500)
model.liss<-randomForest(vol_ha~.,data=liss.train[,c('vol_ha','NDWI','SAVI','MSAVI','ratio','band2','band3','band5')],importance=T)


###############################################
# Step 4: Evaluate the model 
##############################################

p1<-ggplot(sentinel.train,aes(x=sentinel.train$vol_ha,y=predict(model.sentinel))) +
  geom_point()+
  labs(x='Observed Vol/ha',y='Predicted Vol/ha')

###############################################
#Step 4: Save the prediction model
###############################################

save(model.sentinel,file='data/model/prediction_model_sentinel.RData')
save(model.liss,file='data/model/prediction_model_liss.RData')

