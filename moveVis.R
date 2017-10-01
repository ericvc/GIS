################################################################################
##  Script for visualizing leopard movements with the "moveVis" package for R ##
################################################################################
#install.packages('moveVis')
library(moveVis)

#install ImageMagick software
#for macOS, this can be done with Homebrew or MacPorts.
#system("brew install ImageMagick")

#data files
f = list.files("~/Leopard Analysis/Workspace/",
               pattern = "stepUD_data",
               full.names = TRUE)[-7] #remove morani

#create movement objects
dataList = sapply(f,read.csv)
vars = c("x","y","local.time","Name")
data = do.call("rbind",lapply(dataList, function(x) x[,vars]))
times = as.POSIXct(data$local.time, tz="Africa/Nairobi")
data2 = data[order(data$times),]
m = move(data$x, data$y, times, animal=data$Name, proj = CRS("+proj=longlat +datum=WGS84"))

base = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL.tif")
base2 = projectRaster(base, 
                      crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
                      )
base_layer = list()
for(i in 1:length(unique(m$time))) base_layer[[i]] = base2
base_layer_dt = unique(m$time)

#color vector
colvec = c("sandybrown", "#EFCC6B", "green4", "#66C2A5", "#7D6027", 
            "red", "#0CAB76", "#176E43","#CF948B", "#CE9C76", "#66C2A5")
animate_move(m, "/Volumes/Data/moveVis", 
             conv_dir = "/Volumes/Data/ImageMagick", 
             layer = base_layer, 
             layer_col = colvec,
             layer_dt=base_layer_dt,
             tail_elements = 4, 
             frames_nmax = 300, 
             paths_alpha = 0.7, 
             frames_interval = 2, 
             frames_width = 1920, 
             frames_height=1080
             )
