###############
#Setup packages
###############
packages <- c("here","MASS","glmnet","rpart","party","randomForest","gbm","car")#,"glmboost")

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


########################
#Equation forthe model 
######################
baseq <- "area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_university + num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity +  humidity + mean_temp_c +holiday + total_precip_mm + days"

targetvar <- "nb_departure ~"
inteq <- get_inteq(preproc$data,yvar = "nb_departure")
polyeq <- get_polymeq(preproc$data,3,yvar = "nb_departure") 

eqbase <- paste(targetvar,baseq)
eqint <- paste(targetvar,baseq,"+",inteq)
eqpolint <- paste(targetvar,baseq,"+",inteq,"+",polyeq)
eqpol <- paste(targetvar,baseq,"+",polyeq)

########################
# Preprocessing
#######################
print(summary(rawdata))
get_heatmap(preproc)
print(paste("Variance Inflation Factor (celcius vs Farenheit):",as.character(vif(lm(nb_departure ~ mean_temp_c + max_temp_f, data=rawdata))[1])))

####################
#Split data into sets
#####################
prun <- FALSE # set practice run

if (prun){#####################################################################################change to 2
  props = c(0.1,0.1,0.01,0.7)
  n_split = 4
} else {
  props = c(0.1,0.1,0.8)
  n_split = 3
}
temp <- train_test_ids(preproc$data,n_split=n_split,prop = props)


train <- data.frame(preproc$data[temp[[3]],])                 #small training set
valid <- data.frame(preproc$data[temp[[2]],])                #validation set#########################################remove
test <- data.frame(preproc$data[temp[[3]],])                    #test set
#remove(list = "temp")

lmodel <- data.frame(   #model.name           model.type    ,equation    ,opts  
                     c("baseline"          , "baseline"   , ""         , "")
                     ,c("poisson"          ,  "glm_poisson", eqpolint  , ""                           )
                     ,c("linear"           ,  "lm"        ,  eqpolint  , ""                           )
                     #,c("binom"           ,  "glm.nb"    ,  eqpolint  , ""                           )
                     ,c("elasticnet"       ,  "elasticnet",  eqpolint  , "1se"                           )
                     ,c("relaxlasso"       ,  "relaxlasso",  eqpolint  , ""                           )
                     ,c("regressiontree"   ,  "singletree",  eqbase     , "method=anova, hyper=1se"         )
                     ,c("poissontree"      ,  "singletree",  eqbase     , "method=poisson, hyper=1se"         )
                     ,c("conditional_tree" ,  "condtree"   , eqbase         , ""                           )
                     ,c("baseforest"       ,  "baseforest",  ""        , ""                           )
                     ,c("boosttree"            ,  "boost"     ,   ""       , ""                           )
                     )

resultsdf <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(resultsdf) <- c("Method","Validation_RMSE", "Validation_MAE","Test_RMSE", "Test_MAE", "Parameters")

for (mm in lmodel){
  print(paste("Running model: ",mm[1], " ", format(Sys.time(), "%H:%M:%S")))
  tout <- fit_predict_err(mm[2],mm[3],mm[4],train,valid,test)
  resultsdf[nrow(resultsdf) + 1,] = c(mm[1],tout$rmse,tout$mae,tout$testrmse,tout$testmae,tout$opstr)
}
