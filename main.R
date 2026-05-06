###############
#Selecting analyses to carry
###############
data_explo <- FALSE     # Whether to run data exploration
part1 <- FALSE         # Whether to run part 1 (non-spatial) models
part2 <- TRUE         # Whether to run part 2 (spatial) models



###############
#Setup packages
###############
packages <- c("here","MASS","glmnet","rpart","party","randomForest","gbm","car","mboost","nlme","dplyr","ranger","gstat","sp","sf","spdep")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#Add functions from function file
source("functions.R")

###########
#load data
##########
load("BIXI.RData") 
rawdata <- Bixi_data
sloc <- Spatial_positions

remove("Bixi_data","Spatial_positions")

set.seed(2112026)

preproc <- preprocessing(rawdata,sloc)
aggre <- aggregate_by_station(preproc)

########################
#Equation forthe model 
######################
baseq <- "area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_university + num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity +  humidity + mean_temp_c +holiday + total_precip_mm + days + months + latitude + longitude+ latitude_dist + longitude_dist"

targetvar <- "nb_departure ~"
inteq <- get_inteq(preproc$data,yvar = "nb_departure")
polyeq <- get_polymeq(preproc$data,2,yvar = "nb_departure") 

eqbase <- paste(targetvar,baseq)
eqint <- paste(targetvar,baseq,"+",inteq)
eqpolint <- paste(targetvar,baseq,"+",inteq,"+",polyeq)
eqpol <- paste(targetvar,baseq,"+",polyeq)

#equations for spatial model
sbaseq <- "area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity"# + latitude + longitude"
sinteq <- get_inteq(aggre,yvar = "nb_departure")
spolyeq <- polyeq <- get_polymeq(aggre,2,yvar = "nb_departure")

spateqbase <- paste(targetvar,sbaseq)
spateqint <- paste(targetvar,sbaseq,"+",sinteq)
spateqpolint <- paste(targetvar,sbaseq,"+",sinteq,"+",spolyeq)
spateqpol <- paste(targetvar,sbaseq,"+",spolyeq)

########################
# Data exploration
#######################
if(data_explo){
    print(summary(rawdata))
    get_heatmap(preproc)
    print(paste("Variance Inflation Factor (celcius vs Farenheit):",as.character(vif(lm(nb_departure ~ mean_temp_c + max_temp_f, data=rawdata))[1])))
}

####################
#Split data into sets
#####################

if (data_explo){
  #Keep only limited amount of data for running models to save time
  props = c(0.1,0.1,0.01,0.7)
  n_split = 4
} else {
  #full dataset used for training
  props = c(0.1,0.1,0.8)
  n_split = 3
}
temp <- train_test_ids(preproc$data,n_split=n_split,prop = props)
train <- data.frame(preproc$data[temp[[3]],])
valid <- data.frame(preproc$data[temp[[2]],])
test <- data.frame(preproc$data[temp[[1]],])

#for spatially aggregated data
stemp <- train_test_ids(aggre,n_split=2,prop = c(0.1,0.9))
spatrain <- data.frame(aggre[stemp[[2]],])
#spavalid <- data.frame(aggre[stemp[[2]],])
spavalid <- list()
spatest <- data.frame(aggre[stemp[[1]],])

#################
# Train models
#################
if (part1){
    model_list <- list(#model.name                  model.type    ,equation    ,opts                      , train,     test,    valid
                           c("baseline"         , "baseline"      , ""         , ""                        , "train"   ,   "test",    "valid")
                          ,c("poisson"          ,  "glm_poisson"  ,  eqpolint  , ""                         , "train"   ,   "test",    "valid")
                          ,c("linear"           ,  "lm"           ,  eqpolint  , ""                         , "train"   ,   "test",    "valid")
                          ,c("elasticnet1se"    ,  "elasticnet"   ,  eqpolint  , "1se"                      , "train"   ,   "test",    "valid")
                          ,c("elasticnetmin"    ,  "elasticnet"   ,  eqpolint  , ""                         , "train"   ,   "test",    "valid")
                          ,c("relaxlasso"       ,  "relaxlasso"   ,  eqpolint  , ""                         , "train"   ,   "test",    "valid")
                          ,c("regressiontree1se",  "singletree"   ,  eqbase    , "method=anova, hyper=1se"  , "train"   ,   "test",    "valid")
                          ,c("poissontree1se"   ,  "singletree"   ,  eqbase    , "method=poisson, hyper=1se", "train"   ,   "test",    "valid")
                          ,c("regressiontree"   ,  "singletree"   ,  eqbase    , "method=anova, hyper=min"  , "train"   ,   "test",    "valid")
                          ,c("poissontree"      ,  "singletree"   ,  eqbase    , "method=poisson, hyper=min", "train"   ,   "test",    "valid")
                          ,c("conditional_tree" ,  "condtree"     ,  eqbase    , ""                         , "train"   ,   "test",    "valid")
                          #,c("rangerforest"     ,  "rangerforest" ,  eqbase    , ""                         , "train"   ,   "test",    "valid")
                          ,c("boosttree"        ,  "boosttree"    ,  eqbase    , ""                         , "train"   ,   "test",    "valid")
                          ,c("forward_regr"     ,  "forward"      ,  eqpolint  , ""                         , "train"   ,   "test",    "valid")
                          ,c("randintercept"   ,  "randintercept",  eqpol      , ""                         , "train"   ,  "test" ,    "valid")
                         )

    dataset_list <- list(train = train, test = test, valid = valid, spatrain = spatrain, spatest=spatest, spavalid = spavalid)

    results <- train_models(model_list,dataset_list)
    print(results)
}


if(part2){
    model_list <- list(#model.name                  model.type    ,equation    ,opts                      , train,     test,    valid
                           c("baseline"         , "baseline"      , ""         , ""                       , "train"   ,   "test",    "valid")
                          ,c("baselinespat"    ,   "baseline"     ,""          , ""                       , "spatrain"   ,   "spatest",    "spatest")
                          ,c("kriging"         ,  "kriging"       , spateqbase  , ""                      , "spatrain"   ,   "spatest",    "spatest")
                          , c("boostkriging"   ,   "boostkriging" , spateqbase  , ""                      , "spatrain"   ,   "spatest",    "spatest")
                          ,c("boosttreespat"   ,  "boosttree"     , spateqbase  , ""                      , "spatrain"   ,   "spatest",    "spatest")
                          ,c("linearkriging"    , "linearkriging"  , spateqbase  , ""                     , "spatrain"   ,   "spatest",    "spatest")
                          ,c("linearspat"       ,  "lm"           , spateqbase  , ""                      , "spatrain"   ,   "spatest",    "spatest")
                         )

    dataset_list <- list(train = train, test = test, valid = valid, spatrain = spatrain, spatest=spatest, spavalid = spavalid)

    results <- train_models(model_list,dataset_list)
    print(results)
}

