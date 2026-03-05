###############
#Setup packages
###############
packages <- c("here","MASS","glmnet")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#Add functions from function file
source("func_part1.R")

###########
#load data
##########
load("BIXI.RData") 
rawdata <- Bixi_data
sloc <- Spatial_positions

remove("Bixi_data","Spatial_positions")

set.seed(2112026)

preproc <- preprocessing(rawdata)

unique(preproc$data$time)



########################
#Equation forthe model 
######################
equation <- "nb_departure ~ (area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_metro_stations +  
                                num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity + 
                                humidity + mean_temp_c +holiday + total_precip_mm )^2 + 
                                (area_park + len_cycle_path + len_major_road + len_minor_road +num_metro_stations + num_metro_stations +  
                                num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity + 
                                humidity + mean_temp_c + total_precip_mm + days)^2"

#Shortened equation for testing
equation <- "nb_departure ~ (area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_metro_stations +  
                                num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity + 
                                humidity + mean_temp_c +holiday + total_precip_mm ) +(area_park + len_cycle_path + len_major_road)^2"

####################
#Split data into sets
#####################
prun <- TRUE # set practice run

if (prun){
  props = c(0.1,0.1,0.01)
  n_split = 4
} else {
  props = c(0.1,0.1)
  n_split = 3
}

temp <- train_test_split(preproc$data,n_split = n_split,prop = props)
strain <- data.frame(temp[[3]])                 #small training set
#btrain <- data.frame(rbind(temp[[1]]),temp[[2]])#big training set that includes validation data
valid <-  data.frame(temp[[2]])                 #validation set
test <- data.frame(temp[[1]])                   #test set
remove(list = "temp")

################################################
# Generate interaction Xs and matrix for models#
################################################

#xstrain <- model.matrix(equatio,strain)
#xbtrain <- model.matrix(equation,btrain)
#xvalid <- model.matrix(equation,valid)
#xtest <- model.matrix(equation,test)


#####################################################################################################################
#models 
#function       (model.type ,equation ,opts                        ,training_data,valid_data)
#fit_predict_err("glm"      ,""       ,"family=poisson(link='log')",strain        ,valid)    #poisson
#fit_predict_err("lm"       ,""       ,""                          ,strain        ,valid)    #linear
#fit_predict_err("glm.nb"   ,""       ,""                          ,strain        ,valid)    # neg binom (var > mean)
#fit_predict_err("cv.glmnet",equation ,"alpha=0"                   ,strain        ,valid)    #ridge
#fit_predict_err("cv.glmnet",equation ,"alpha=1"                   ,strain        ,valid)    #lasso
#fit_predict_err("cv.glmnet",equation ,"family=poisson,alpha=0"    ,strain        ,valid)    #ridge poisson
#fit_predict_err("cv.glmnet",equation ,"family=poisson,alpha=1"    ,strain        ,valid)    #lasso poisson



#summary(lm(nb_departure ~ ((area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_metro_stations +  num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity + humidity + mean_temp_c +holiday + total_precip_mm + days)^2) , data = preproc$data))

#removers days with holiday interation due to missing ones
#summary(lm(nb_departure ~ ( (area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_metro_stations +  num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity + humidity + mean_temp_c +holiday + total_precip_mm )^2 +
#                            (area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_metro_stations +  num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity + humidity + mean_temp_c + total_precip_mm + days)^2                      
#                            )
#                            , data = preproc$data))
