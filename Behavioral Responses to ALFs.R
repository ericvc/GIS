#################################################################################
#R Script for evaluating behavioral response to anthropogenic landscape features#
#################################################################################
library(rethinking)
library(spatstat)
library(raster)
library(adehabitatHR)
library(maptools)

#Set the working directory to the project folder
setwd('~/Leopard Analysis/')

#----------------------------Step 1: Data Preparation---------------------------
# 
# #Load movement locations...
# load('Workspace/Analysis.R') #Loads data in an object called 'leopards'
# load('Workspace/Analysis2.R') #Loads data in an object called 'leopards2'
# 
# #create new master data object...
# d1 = leopards_UTM@data
# d2 = leopards2_UTM.sub@data
# dat = d1[,c("utm.easting","utm.northing","local.time","Name","state")]
# dat2 = d2[,c("Easting","Northing","local.time","individual.local.identifier")]
# dat2$state = NA
# dat2[,1:2] = leopards2_UTM.sub@coords #Coordinates in data.frame are fucked.
# names(dat2) <- names(dat)
# dat3 = data.frame(rbind(droplevels(dat),droplevels(dat2)), check.names = FALSE)
# dat4 = dat3[complete.cases(dat3[,1:4]),]
# dat4$Name[dat4$Name=="Chumvi2"] = "Chumvi"
# dat4$ID = as.character(dat4$Name)
# dat5 = droplevels(dat4[dat4$ID!="Morani",])
# 
# refdate = strptime("2014-01-01 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
# leop_trNA = setNA(leop_tr, date.ref = refdate, dt = 0.25, units = "hour")
# leop_tr0 = sett0(leop_trNA, date.ref = refdate, dt = 0.25, units = "hour")
# 
# #Define functions for angle calculations
# 
# #Original source: 
# #http://www.r-bloggers.com/calculate-turning-angles-and-step-lengths-from-location-data/
# 
# #Read-in spatial data for ALFs
# 
# #load habitat map
# habitat = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL.tif")
# #PROJ.4 string
# p4s_UTM = CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# #vector data
# roads = readShapeLines("Workspace/Shapefiles/Roads/roads.shp", proj4string = p4s_UTM)
# glades = readShapePoly("Workspace/Shapefiles/Glades/glades.shp", proj4string = p4s_UTM)
# humans = readShapePoly("Workspace/Shapefiles/Humans/humans.shp", 
#                                    proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#                        )
# 
# #convert to SpatialPoints object format
# roads_sp = as(roads, "SpatialPoints")
# glades_sp = as(as(glades, "SpatialLines"),"SpatialPoints")
# humans_sp = spTransform(as(as(humans, "SpatialLines"),"SpatialPoints"),p4s_UTM)
# #Create 'owin' object for use in calculating distances.
# win = owin(extent(habitat)[1:2], extent(habitat)[3:4])
# 
# #Create loop for distance calculations.
# d_split = split(dat5, dat5$ID)
# outList = list()
# for(i in 1:length(d_split)){
#   
#   ds = d_split[[i]]
#   xy = data.frame(coordinates(ds[,1:2]))
#   xy_t1 = data.frame(rbind(xy[-1,],c(NA,NA)))
#   xy.ppp = as.ppp(xy, W = win, check=FALSE) #Create 'ppp' object
#   
#   #measure angle between three points: feature --> animal (AB) and animal --> t+1 (BC)
#   
#   #animal movement vectors
#   bc = as.matrix(xy_t1 - xy) #observed change in animal position
#   step_dist = apply(bc,1,function(z)sqrt(sum(z^2))) #movement rate
#   
#   #road response angle calculations
#   roads_dist = nncross(xy.ppp, as.ppp(roads_sp))
#   roads_xy = data.frame(coordinates(roads_sp)[roads_dist$which,])
#   ab = as.matrix(xy - roads_xy) #changes relative to feature.
#   dot_prod = apply(cbind(ab,bc),1, function(z) sum(z[1:2]*z[3:4]))
#   norm_ab = apply(ab,1,function(z) norm(as.matrix(z)))
#   norm_bc = apply(bc,1,function(z) norm(as.matrix(z)))
#   roads_ra <- acos(dot_prod/(norm_ab*norm_bc))
#   
#   #glade response angle calculations
#   glades_dist = nncross(xy.ppp, as.ppp(glades_sp))
#   glades_xy = data.frame(coordinates(glades_sp)[glades_dist$which,])
#   bc = as.matrix(glades_xy - xy) #changes relative to feature.
#   dot_prod = apply(cbind(ab,bc),1, function(z) sum(z[1:2]*z[3:4]))
#   norm_ab = apply(ab,1,function(z) norm(as.matrix(z)))
#   norm_bc = apply(bc,1,function(z) norm(as.matrix(z)))
#   glades_ra <- acos(dot_prod/(norm_ab*norm_bc))
#   
#   #human response angle calculations
#   humans_dist = nncross(xy.ppp, as.ppp(humans_sp))
#   humans_xy = data.frame(coordinates(humans_sp)[humans_dist$which,])
#   bc = as.matrix(humans_xy - xy) #changes relative to feature.
#   dot_prod = apply(cbind(ab,bc),1, function(z) sum(z[1:2]*z[3:4]))
#   norm_ab = apply(ab,1,function(z) norm(as.matrix(z)))
#   norm_bc = apply(bc,1,function(z) norm(as.matrix(z)))
#   humans_ra <- acos(dot_prod/(norm_ab*norm_bc))
# 
#   outList[[i]] = data.frame(ds[,1:5], 
#                             xy_t1, 
#                             step_dist,
#                             roads_xy, roads_dist, roads_ra,
#                             glades_xy, glades_dist, glades_ra,
#                             humans_xy, humans_dist, humans_ra)
#   names(outList[[i]]) = c("leop_xt","leop_yt","time","Name","state", #'ds' object
#                 "leop_xt1","leop_yt1", #'xy_t1' object
#                 "step_dist",
#                 "roads_xt", "roads_yt", "roads_dist","roads_cell_id", "roads_ra", #roads_xy + roads_dist + roads_ra
#                 "glades_xt", "glades_yt", "glades_dist","glades_cell_id", "glades_ra",#glades_xy + glades_dist + glades_ra
#                 "humans_xt", "humans_yt", "humans_dist","humans_cell_id", "humans_ra") #humans_xy + humans_dist + humans_ra
# 
# if(i==length(d_split)){
#   dat = do.call("rbind",outList)
#   rm(outList)
#   }
# }
# 
# crds. = SpatialPoints(dat[,1:2], 
#                       proj4string = CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#                       )
# dateTime. = dat$time
# pos = solarpos(spTransform(crds.,CRS("+proj=longlat")), dateTime.)[,2]
# dat$diel_D = ifelse(pos>=0,0,1)
# 
# #save complete data file to hard disk
# saveRDS(dat, file = "Workspace/leop_moverate+response_angles.Rdata")

data0 = readRDS("Workspace/leop_moverate+response_angles.Rdata")

#----------------Responses to Human habitation----------------------------------

## !! Need to subset obs where individual is 'active' / 'moving'
# step_dist > 50?

# load necessary packages 
library("R2jags")
library("coda")
library("emdbook")

data = data0[complete.cases(data0),]

ids = levels(data$Name)  # identify unique individuals  
anid = c() # create a number vector of  animal identities for JAGS

for(i in 1:length(ids)){anid[data$Name==ids[i]]=i}  # for every anid assign the unique animal code

# prepare variables for JAGS analysis (in this case we look at 7 animals)
nind <- 7
id <- anid [anid<7] 
step <- data$step_dist [anid<7]
ra_h <- abs(data$humans_ra [anid<7])
ra_g <- abs(data$glades_ra [anid<7])
ra_r <- abs(data$roads_ra [anid<7])
nobs <- length(step)
n_kappas <- 100
kappas <- 1:n_kappas
IO_s   <- sapply(kappas, function(z)besselI(z,0))

diel_D <- data$diel_D [anid<7]
humn <- log(data$humans_dist [anid<7])
glde <- log(data$glades_dist [anid<7])
road <- log(data$roads_dist [anid<7])


#JAGS code (JAGS_risk_allocation.txt) that is called by the "Supplemental R Code.r"

#-1) No response model

# create data vector
data.hu_nr <- list(step="step",
                   ra_h="ra_h",
                   nobs="nobs",
                   nind="nind",
                   id="id",
                   n_kappas="n_kappas",
                   kappas="kappas",
                   IO_s="IO_s")

# generate the initials for the Bayesian approach
inits.hu_nr <- function() list(b=runif(nind,0,4), 
                               ba0=rnorm(nind,0,.01),                           
                               theta0=runif(nind, 0,100),
                               mb=1,mba0=0,
                               sb=.01, sba0=.01)

# parameters to retrieve from JAGS
params.hu_nr <- c("b", 
                  "ba0",
                  "theta0",                   
                  "IO_hat", "kappa_hat", "chi.int", "chi.alpha",
                  "mb", "mba0",
                  "sb", "sba0")
NR_model <- jags(data.hu_nr, inits.hu_nr, params.hu_nr, model.file="Behavioral Responses to ALFs/JAGS Models/JAGS_no_response.txt",
                 n.chains=2, n.iter=2.5e5, n.burnin=1e3, n.thin=250)
saveRDS(NR_model, file = "Workspace/NoResponse_Human_Model.Rdata")
#-2) Risk-allocation model

# create data vector
data.hu_ra <- list(step="step",
                ra_h="ra_h",
                nobs="nobs",
                diel_D="diel_D",
                humn="humn",
                nind="nind",
                id="id",
                n_kappas="n_kappas",
                kappas="kappas",
                IO_s="IO_s")

# generate the initials for the Bayesian approach
inits.hu_ra <- function() list(b=runif(nind,0,4), 
                               ba0=rnorm(nind,0,.01), ba1=rnorm(nind,0,.01), ba2=rnorm(nind,0,.01),
                               theta0=runif(nind,0,10), theta1=runif(nind,0,10), theta2=runif(nind,0,10),
                               mb=1,mba0=0, mba1=0, mba2=0,
                               sb=.01, sba0=.01, sba1=.01, sba2=.01)

# parameters to retrieve from JAGS
params.hu_ra <- c("b", "br0",
                  "bm0", 
                  "ba0", "ba1", "ba2",
                  "mu",
                  "theta0","theta1","theta2",
                  "IO_hat", "kappa_hat",
                  "mb", "mba0", "mba1", "mba2",
                  "sb", "sba0", "sba1", "sba2")

RA_model <- jags(data.hu_ra, inits.hu_ra, params.hu_ra, 
	model.file="Behavioral Responses to ALFs/JAGS Models/JAGS_risk_allocation.txt", 
	n.chains=2, n.iter=2.5e5, n.burnin=1e3, n.thin=250
	)
saveRDS(RA_model, file = "Workspace/RiskAllocation_Human_Model.Rdata")

# view results of model                
plot(RA_model) 
print(RA_model)
RA_model$summary 
b <- mean(RA_model$summary[1:max(anid)]) # b is the mean shape parameter for the Weibull distribution

attach.all(RA_model$sims.list)
trellis.par.set(canonical.theme(color=F))
s1 = as.mcmc(RA_model)
s2 = s1[,c("mba0","mba1","mba2","mbr0","mbr1","mbr2","mbm0","mbm1","mbm2")]
print(densityplot(s2,trace=F))

library("coda")
CI = HPDinterval(as.mcmc(RA_model$sims.matrix), prob = 0.80) # calculates the 80% HPD
which(CI[,1]>0  & CI[,2]>0)
which(CI[,1]<0 & CI[,2]<0)
CI

#-3) Risky times model

# create data vector
data.hu_rt <- list(step="step",
                   ra_h="ra_h",
                   nobs="nobs",
                   diel_D="diel_D",
                   humn="humn",
                   nind="nind",
                   id="id",
                   n_kappas="n_kappas",
                   kappas="kappas",
                   IO_s="IO_s")

# generate the initials for the Bayesian approach
inits.hu_rt <- function() list(b=runif(nind,0,4), 
                               ba0=rnorm(nind,0,.01), ba1=rnorm(nind,0,.01), ba2=rnorm(nind,0,.01), ba3=rnorm(nind,0,.01),
                               theta0=runif(nind,0,10), theta1=runif(nind,0,10), theta2=runif(nind,0,10), theta3=runif(nind,0,10),
                               mb=1,mba0=0, mba1=0, mba2=0, mba3=0,
                               sb=.01, sba0=.01, sba1=.01, sba2=.01, sba3=.01)

# parameters to retrieve from JAGS
params.hu_rt <- c("b", "br0",
                  "bm0", 
                  "ba0", "ba1", "ba2","ba3",
                  "theta0","theta1","theta2","theta3",
                  "IO_hat", "kappa_hat",
                  "mb", "mba0", "mba1", "mba2","mba3",
                  "sb", "sba0", "sba1", "sba2","sba3")

RT_model <- jags(data.hu_rt, inits.hu_rt, params.hu_rt, model.file="Behavioral Responses to ALFs/JAGS Models/JAGS_risky_times.txt",
                       n.chains=2, n.iter=3e3, n.burnin=1e3, n.thin=250)
#update(RT_model, 1e5)

# view results of model                
plot(RT_model) 
print(RT_model)
RT_model$summary 
b <- mean(RT_model$summary[1:max(anid)]) # b is the mean shape parameter for the Weibull distribution

attach.all(RT_model$sims.list)
trellis.par.set(canonical.theme(color=F))
s1 = as.mcmc(RT_model)
print(densityplot(s2,trace=F))

library("coda")
CI = HPDinterval(as.mcmc(RT_model$sims.matrix), prob = 0.80) # calculates the 80% HPD
which(CI[,1]>0  & CI[,2]>0)
which(CI[,1]<0 & CI[,2]<0)
CI

#-4) Risky places model

# create data vector
data.hu_rp <- list(step="step",
                   ra_h="ra_h",
                   nobs="nobs",
                   humn="humn",
                   nind="nind",
                   id="id",
                   n_kappas="n_kappas",
                   kappas="kappas",
                   IO_s="IO_s")

# generate the initials for the Bayesian approach
inits.hu_rp <- function() list(b=runif(nind,0,4), 
                               ba0=rnorm(nind,0,.01), ba2=rnorm(nind,0,.01),                               
                               br0=rnorm(nind,0,.01), 
                               bm0=rnorm(nind,0,.01), 
                               theta0=0.1, theta2=0.1,
                               mb=1,mba0=0, mba2=0,
                               sb=.01, sba0=.01, sba2=.01)

# parameters to retrieve from JAGS
params.hu_rp <- c("b", "br0",
                  "bm0", 
                  "ba0", "ba1", "ba2",
                  "theta0","theta2",                   
                  "IO_hat", "kappa_hat",
                  "mb", "mba0", "mba2",
                  "sb", "sba0", "sba2")

RP_model <- jags(data.hu_rp, inits.hu_rp, params.hu_rp, model.file="Behavioral Responses to ALFs/JAGS Models/JAGS_risky_places.txt",
                       n.chains=2, n.iter=3e3, n.burnin=1e3, n.thin=250)
