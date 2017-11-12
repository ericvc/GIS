####                                                                        ####
####       Create a continuous woody cover raster for the study site        ####
####                                                                        ####
####                                                                        ####

####(1)                      Set Up The Workspace                           ####

setwd("/Volumes/Lockbox/Leopard Analysis/")

library(e1071)
library(raster)
library(maptools)
library(PBSmapping)
library(spatstat)
library(RColorBrewer)
library(betareg)

#Load habitat map (for extent and projection)
habitat = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL.tif")
ref = raster("Environmental Variables/Habitat Type/ProportionWoodyCover_309x307.tif")
ref2 = projectRaster(ref,habitat)

#Landsat data and machine learning algorithms.
lsatFiles = list.files("/Volumes/Shoebox/GIS/Landsat_LC81680602014178LGN00/",
                       pattern = ".TIF", full.names=TRUE)
#Stack LANDSAT 7 bands (5,4,3,2, and QA)
#see https://landsat.usgs.gov/qualityband for QA band code definitions.
lsatData = stack(lsatFiles[c(4:7,12)])
lsatData.cropped = crop(lsatData, extent(habitat))
lsatData.repro = projectRaster(lsatData.cropped, habitat, method="ngb")

####(1)                      Set Up The Workspace                           ####

#Expand coverage of the habitat map to include outlying areas. I will do this using
#support vector machines.
cover = raster(ref2)
# cover[] = cut(ref2[], seq(0,1,0.1))
useCell = getValues(lsatData.repro[[5]])
cellVals = getValues(ref2)
set.seed(138)
useTrain = which(useCell==20480 & !is.na(cellVals))
trainCell = sample(useTrain, 1e4, replace=FALSE)
trainData = data.frame(raster::extract(lsatData.repro, trainCell))
names(trainData) = c("B2","B3","B4","B5","QA")
trainData$cover = cellVals[trainCell]
#model tuning uses grid approximation to find optimal input values for SVM model.
svmTune = tune(svm, cover ~ B2 + B3 + B4 + B5,
               probability=TRUE,
               data=trainData,
               #tunecontrol=tune.control(sampling="fix"),
               ranges = list(gamma=2^seq(-3, 1, by=0.2), cost=2^seq(0, 6, by=0.2))
)
# #save(svmTune, file="Workspace/svm_habitat_model_tuned.Rdata") #save to hard disk
plot(svmTune) #examine fit
print(svmTune)
tunedModel = svmTune$best.model #get the best fit model from tune object
fullData = data.frame(getValues(lsatData.repro))
names(fullData) = c("B2","B3","B4","B5","QA")
modelPreds = predict(tunedModel, fullData[,-5])
cells = as.numeric(names(modelPreds))
vals = as.numeric(modelPreds)
r = cover
r[] = NA; names(r) = "Data"
r[cells] = vals
r[which(r[]<0)] <- 0
#r[trainCell] = 10
r[r>1] <- 1
plot(r)

library(caret)

set.seed(138)
sampleCells = which(!is.na(ref2[]))
trainCell2 = sample(sampleCells, 2e4, replace=FALSE)
trainData2 = data.frame(extract(lsatData.repro, trainCell2))[,-5]
names(trainData2) = c("B2","B3","B4","B5")
trainData2$cover = cellVals[trainCell2]
modelPreds = predict(tunedModel, trainData2[,-5])
rho = cor(trainData2$cover, modelPreds)
#s = spline(trainData2$cover, modelPreds, n=50)
plot(trainData2$cover, modelPreds)
lines(s$x, s$y, col="red", lwd=0.8)
text(0.9,0.8, paste("r=",round(rho,2),sep=""), cex=1.5)
# cM = confusionMatrix(preds, reference = trainData2[,5])

#write product to hard disk.
writeRaster(r,
            filename = paste("Environmental Variables/Local Predictors/",
                             "percent_cover_full",".tif",sep=""),
            overwrite=TRUE
)
