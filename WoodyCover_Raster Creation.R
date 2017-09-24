#I will create a separate script for the "Cover" raster creation because the methods
#are unique.

setwd("/Volumes/Lockbox/Leopard Analysis/")

library(e1071)
library(raster)
library(maptools)
library(spatstat)
library(RColorBrewer)

varNames = c("Open", "Grass", "Cover", "River", "Roads", 
             "Human", "Glade", "GladeEdge", "Escarpment", "Lugga", "Water")
colvec = colorRampPalette(rev(brewer.pal(8, "Set2")))(length(varNames))

# new raster extent
r = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL.tif")
r[] = NA
# percent woody cover data
percCover = raster("Environmental Variables/Habitat Type/ProportionWoodyCover_309x307.tif")

# Landsat data and machine learning algorithms.
lsatFiles = list.files("/Volumes/Shoebox/GIS/Landsat_LC81680602014178LGN00/", pattern = ".TIF", full.names=TRUE)

# Stack vegetation bands (5,4,3)
lsatData = stack(lsatFiles[c(5:7)])
# reproject to UTM coordinate system
lsatData.repro = projectRaster(lsatData, r)
# crop data to fit the extent of the habitat raster
lsatData.cropped = crop(lsatData.repro, extent(r))

# load terrain data
ter = raster("Environmental Variables/Elevation_(derived from  SRTM).tif")
ter = crop(ter, r)
tri = terrain(ter, opt = "TRI")
rTrain = stack(lsatData.cropped, tri)

# sample cells at random to train the algorithm.
nsamp = 4e3 #round( 0.2 * ncell(percCover[!is.na(percCover)]) )
set.seed(138)
sampleCells = sample(which(!is.na(percCover[])), 
                     size = nsamp, 
                     replace = FALSE)
# extract training data
# try with principal components analysis
trainData = extract(rTrain, sampleCells)
#predict(trainData2, fullData)
# get cover class from cells
# cover = cut(extract(percCover, sampleCells), breaks=seq(0,1,0.05))
cover = extract(percCover, sampleCells)
train = data.frame(trainData)
names(train) = c("B3","B4","B5","TRI")
train$cover = cover

# fit svm model using grid approximation
tuneResult <- tune(svm, cover ~  B3 + B4 + B5 + TRI, data=train, kernel="sigmoid",
                   ranges = list(epsilon = seq(0.82, 0.86, 0.01), gamma=seq(-2, 0, by=0.25))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)
tunedModel <- tuneResult$best.model

# use model to predict cover outside the original satellite 
fullData = data.frame(getValues(rTrain))
names(fullData) = c("B3","B4","B5","TRI")
# use SVM model to predict cover class from Landsat data (very slow).
modelPreds = predict(tunedModel, fullData)
#save(modelPreds, file="Workspace/svm_woodycover_model_predictions.Rdata")

# plot results
cells = as.numeric(names(modelPreds))
vals = as.numeric(modelPreds)
r[] = NA; names(r) = "Data"
r[][cells] = vals
plot(r)

# assess accuracy
preds = r[sampleCells]
reals = percCover[sampleCells]

# percent accuracy
sum(preds == reals) / length(reals)
