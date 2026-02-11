#Setup packages
packages <- c("here")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#Add functions from function file
source("func_part1.R")

#load data
load("BIXI.RData") 
rawdata <- Bixi_data
sloc <- Spatial_positions

remove("Bixi_data","Spatial_positions")

set.seed(2112026)

preproc <- preprocessing(rawdata)

#aggregate data by station (essentially removing days)
aggdata <- aggregate_by_station(preproc$data, na_zeros = FALSE)
