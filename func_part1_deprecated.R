first_element <- function(ll){
  return(ll[1])
}
first_elements <- function(ll,n){
  return(ll[1:n])
}

preprocessing <- function(rawdata,remove.na = TRUE){
  # Replace long station names solely by their numbers, provides a dictionary to go from one to the other. 
  # Finds minimum for nu of departures and rescales the variable to whol numbers. 
  # Replaces dates with day of the week. 
  # For days, casts as factor and sets Monday as reference cat
  # For holiday, casts as factor and sets non-holidays as reference
  
  dat <- rawdata
  
  #Find number for each station and only keep that value. 
  #Generate a dictionary linky station number to name. 
  split <- function(stri){
    return(strsplit(stri," -")[[1]][1])
  }
  
  ori <- unique(rawdata$location)
  new <- sapply(ori,split, USE.NAMES = FALSE)
  
  dat$location <- sapply(rawdata$location,split, USE.NAMES = FALSE)
  
  name_dict <- ori
  names(name_dict) <- new
  
  preproc_data <- list()
  preproc_data$data <- dat
  preproc_data$name_dict <- name_dict
  
  #remove all rows with instances of NA since they are linked to malfunctions. 
  if (remove.na == TRUE){
    preproc_data$data <- preproc_data$data[! is.na(preproc_data$data$nb_departure), ]
  }
  
  #put back departures in the original scale
  mindeparture <- min(rawdata$nb_departure[!is.na(rawdata$nb_departure)])
  
  preproc_data$data$nb_departure <- as.integer(preproc_data$data$nb_departure/mindeparture)
  
  #Get days 
  days <- weekdays(preproc_data$data$time,abbreviate = TRUE)
  preproc_data$data$days <- days
  
  #Cast some vars as factors
  preproc_data$data$days <- relevel(as.factor(preproc_data$data$days), ref = "Mon")
  preproc_data$data$holiday <- relevel(as.factor(preproc_data$data$holiday), ref = "0")
  preproc_data$data$num_university <- relevel(as.factor(preproc_data$data$num_university), ref = "0")
  
  #Return only relevant columns 
  preproc_data$data <- preproc_data$data[-c(1,2,18)] #add 11 if want to remove universities
  
  return(preproc_data)
}

aggregate_by_station <- function(dat, na_zeros = TRUE){
  #Aggreaate data, if missing data for departures, replaces with 0. 
  
  mindeparture <- min(dat$nb_departure[!is.na(dat$nb_departure)])
  
  #if (na_zeros){
  #  dat$nb_departure[is.na(dat$nb_departure)] <- 0
  #}
  
  agg_nb_departure <- aggregate(dat["nb_departure"]
                                , by = list(dat$location)
                                , FUN = sum
  )
  
  agg_nb_departure$nb_departure <- agg_nb_departure$nb_departure/mindeparture 
  
  #Make sure all values regarding environment of station are the same across entries
  print("Inspecting data entries for: ")
  cols <- c("area_park","len_cycle_path","len_major_road","len_minor_road","num_metro_stations","num_other_commercial","num_restaurants","num_university","num_pop","num_bus_stations","num_bus_routes","walkscore","capacity")
  for (vv in cols){  
    print(vv)
    for (ll in unique(dat$location)){
      if (length(unique(dat[vv])) != 1){
        print(paste("there are more than one values for ",vv , " in " , ll))
      }
    }
  }
  
  agg_other <- aggregate(dat[cols]
                         , by = list(dat$location)
                         , FUN = first_element
  )
  
  aggdata <- merge(agg_nb_departure, agg_other, by = "Group.1", all.x=TRUE, all.y=TRUE)
  
  return(aggdata)
}

train_test_split <- function(dat,n_split = 2,prop = c(0.8,0.2)){
  #Splits the dataset into n_split number of sub datasets. 
  #The proportion of data in each is specified by prop. Note that the
  #last element of prop is not used since we want fully use the data set. 
  
  ids <- {}
  nobs <- length(dat[,1])
  
  for (ii in 1:n_split){
    if (ii < n_split){
      ids <- c(ids,rep(ii,floor(prop[ii]*nobs)))
    } else {
      nleft <- nobs - length(ids)
      ids <- c(ids,rep(ii,nleft))
    }
  }
  
  #reorder ids randomly
  ids <- sample(ids)
  
  datasets <- vector("list", n_split)
  
  for (ii in 1:n_split){
    datasets[[ii]] <- dat[ids == ii,]
  }
  
  return(datasets)
}

train_test_ids <- function(dat,n_split = 2,prop = c(0.8,0.2)){
  #Finds ids to split the dataset into n_split number of sub datasets.
  #The proportion of data in each is specified by prop. Note that the
  #last element of prop is not used since we want fully use the data set.
  
  ndat <- dim(dat)[1]
  tids <- sample(1:ndat)
  fids <- {}
  
  begs <- 1
  ends <- floor(prop[1]*ndat)
  for (ii in 1:n_split){
    if (ii < n_split){
      fids[[ii]] <- tids[begs:ends]
    } else {
      ends <- ndat
      fids[[ii]] <- tids[begs:ends]
    }
    begs <- ends + 1
    ends <- begs + floor(prop[ii+1]*ndat)
  }
  
  return(fids)
}

get_heatmap <- function(preproc){
  my_colors <- colorRampPalette(c("white","darkred", "yellow"))(5)
  
  heatmap(abs(cor(preproc$data[,-c(1,9,18,19)])))
  heatmap(abs(cor(preproc$data[,-c(1,9,18,19)])),
          col = my_colors,
          breaks = seq(0.5, 1, length.out = 6),
          scale = "none")
}

compute_error <- function(true_y,predicted_y, errtype = "all",minval=0){
  #Specifcy what type of error to use, choices are:
  # RMSE, 
  nn <- length(true_y)
  err <- {}
  
  if(minval != ""){
    predicted_y[predicted_y<minval] <- minval
  }
  
  if (errtype == "RMSE" | errtype == "all"){
    err$rmse <- sqrt(sum((true_y-predicted_y)^2)/nn)
  }
  
  if (errtype == "MSE" | errtype == "all"){
    err$mse <- sum((true_y-predicted_y)^2)/nn
  }
  
  if (errtype == "MAE" | errtype == "all"){
    err$mae <- sum(abs(true_y-predicted_y))/nn
  }
  
  return(err)
}

get_polymeq <- function(df,np,yvar = "nb_departure"){
  #takes input as data frame and gets the n polynoms for
  #each varialble that is not a factor.
  
  tnam <- colnames(df)
  cnam <- tnam[-which(tnam == "nb_departure")]
  
  eq <-""
  for (ii in 1:length(cnam)){
    if(ii != 1){
      eq <- paste(eq,"+",sep="")
    }
    if (np > 1 & !is.factor(df[[cnam[ii]]]) ){
      eq <- paste(eq,"poly(",cnam[ii],",",np,",raw=TRUE)",sep="")
    } else {
      eq <- paste(eq,cnam[ii],sep="")
    }
  }
  return(eq)
}

fit_model_equation <- function(model.type,equation,options,training_data){
  #Takes as imput strings only, will put the stings together and run the model.
  
  if (equation == ""){
    equation <- "nb_departure ~ (area_park + len_cycle_path + len_major_road + len_minor_road + num_metro_stations + num_metro_stations +
                                num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity +
                                humidity + mean_temp_c +holiday + total_precip_mm )^2 +
                                (area_park + len_cycle_path + len_major_road + len_minor_road +num_metro_stations + num_metro_stations +
                                num_other_commercial + num_restaurants + num_pop + num_bus_stations + num_bus_routes + walkscore + capacity +
                                humidity + mean_temp_c + total_precip_mm + days)^2"
  }
  
  if (options != ""){
    options <- paste(",",options)
  }
  
  #fit the model
  tmodel <- eval(parse(text=paste(
    model.type, "(",equation, options,", data = training_data)"
    ,sep = "")))
  
  return(tmodel)
}

fit_model_noeq <- function(model.type,xs,ys,options){
  #Takes as imput x (covariates) and ys to fit a model
  
  if (options != ""){
    options <- paste(",",options)
  }
  
  #fit the model
  if (model.type == "cv.glmnet"){
    tmodel <- eval(parse(text=paste(
      model.type, "(x=xs,y=ys", options,")"
      ,sep = "")))
  }
  #cv.glmnet(x=xtrain,y=ytrain, alpha=alpha)
  
  return(tmodel)
}

############################
# Main function
#############################
fit_predict_err <-  function(model.type,equation,opts,training_data,valid_data,test_data,min_zero = TRUE){###### remove valid
  opstr <- ""
  ################ begin model space #############################
  
  if (model.type == "baseline"){
    #############################################################################################Add validation model creation
    yhat <- rep(mean(training_data$nb_departure),length(valid_data$nb_departure))
    yhattest <- rep(mean(training_data$nb_departure),length(test_data$nb_departure))
  }
  
  if (model.type == "lm" | model.type == "glm"){
    #############################################################################################Add validation model creation
    fitted_model <- fit_model_equation(model.type,equation,opts,training_data)
    yhat <- predict(fitted_model,newdata = valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
    
  }
  
  if (model.type == "cv.glmnet"){#########################################################################creation of one more validation???
    formu <- as.formula(equation)
    xs <- model.matrix(formu,training_data)[,-1]
    xvalid <- model.matrix(formu,valid_data)[,-1]
    ys <- training_data$nb_departure
    
    fitted_model <- fit_model_noeq(model.type,xs,ys,opts)
    
    yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
    opstr <- fitted_model$lambda.1se
  }
  
  if (model.type == "elasticnet"){
    lalpha <- seq(0,1,by=0.25)
    
    #Split to get a validation set to select best alpha
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))
    
    formu <- as.formula(equation)
    
    xs <- model.matrix(formu,training_data[ids[[1]],])[,-1]
    ys <- training_data$nb_departure[ids[[1]]]
    xv <- model.matrix(formu,training_data[ids[[2]],])[,-1]
    yv <- training_data$nb_departure[ids[[2]]]
    
    
    errv <- numeric(length(lalpha))
    for (ii in 1:length(lalpha)){
      alpha <- lalpha[ii]
      fitted_model <- cv.glmnet(xs,ys,alpha=alpha)
      
      yhatv <- predict(fitted_model,new=xv, s = "lambda.1se")
      errv[ii] <- compute_error(yv,yhatv, errtype = "RMSE")$rmse
    }
    #find best alpha
    best_alpha <- lalpha[which.min(errv)]
    
    fitted_model <- cv.glmnet(xs,ys,alpha=best_alpha)
    
    xvalid <- model.matrix(formu,valid_data)[,-1]
    xtest <-  model.matrix(formu,test_data)[,-1]
    
    yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
    yhattest <- predict(fitted_model, new=xtest, s = "lambda.1se")
    opstr <- paste("alpha=",best_alpha,"lambda=",fitted_model$lambda.1se,sep="")
  }
  
  if (model.type == "relaxlasso"){
    formu <- as.formula(equation)
    xs <- model.matrix(formu,training_data)[,-1]
    xvalid <- model.matrix(formu,valid_data)[,-1]
    xtest <- model.matrix(formu,test_data)[,-1]
    ys <- training_data$nb_departure
    
    fitted_model <- cv.glmnet(xs,ys, alpha=1,relax=TRUE)
    
    yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
    yhattest <- predict(fitted_model, new=xtest, s = "lambda.1se")
  }
  
  if (model.type == "singletree"){
    #fits a single tree while varying minsplit, minbucket.
    ms <- c(5,10,15,20,25,30)
    mb <-  c(3,6,9,12,15)
    
    #Split to get a validation set to select best alpha
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))
    
    errv <- matrix(0,nrow = length(ms), ncol = length(mb))
    for (ii in 1:length(ms)){
      for (jj in 1:length(mb)){
        fitted_tree=rpart(as.formula(equation),
                          data=training_data[ids[[1]],],
                          method="anova",
                          control = rpart.control(xval = 10, minsplit=ms[ii], minbucket = mb[jj], cp = 0))
        
        ##############################################################################################################The following line assumes that we should first take cp param and find lowest error before selecting the hyperparam. Doing them together might make more sense.
        fitted_model <- prune(fitted_tree,cp=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"CP"])
        
        yhatv <- predict(fitted_model,new = training_data[ids[[2]],])
        errv[ii,jj] <- compute_error(training_data$nb_departure[ids[[2]]],yhatv, errtype = "RMSE")$rmse
      }
    }
    
    #find best minsplit and minbucket
    best_ms <- ms[which(errv == min(errv), arr.ind = TRUE)[1]]
    best_mb <- mb[which(errv == min(errv), arr.ind = TRUE)[2]]
    
    #Refit the model using the best hyperparams
    fitted_tree=rpart(as.formula(equation),
                      data=training_data,
                      method="anova",
                      control = rpart.control(xval = 10, minsplit=best_ms, minbucket = best_mb, cp = 0))
    
    fitted_model <- prune(fitted_tree,cp=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"CP"])
    
    yhat <- predict(fitted_model,new=valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
    opstr <- paste("minsplit=", best_ms, " minbucket=", best_mb, " cp=", fitted_tree$cp[which.min(fitted_tree$cp[, "xerror"]), "CP"], sep="")
  }
  
  if (model.type == "condtree"){
    #fits a single tree
    fitted_model <- ctree(as.formula(equation), data = training_data)
    yhat <- predict(fitted_model,newdata=valid_data,type="response")
    yhattest <- predict(fitted_model, newdata=test_data, type = "response")
  }
  
  if (model.type == "baseforest"){
    #fits a forest
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))
    n_train <- length(ids[[1]])
    ntree = c(200, 300, 400)
    nodesize = c(10, 50, 100)#c(2,5, 10)
    maxnodes = c(100,200)#c(100,200,400)
    mtry = floor(c(0.33, 0.5,0.83)* dim(training_data)[2])
    
    terr <- 1000000
    for (tt in ntree){for (nn in nodesize){for (mm in maxnodes){for (mt in mtry){
      print(paste("     Trying model with: ", tt, " trees, ",nn," node size, ", mm, " max nodes,", mt, " variables"))
      temp_model <- randomForest(nb_departure~.
                                 ,data=training_data[ids[[1]],]
                                 ,ntree=tt
                                 ,nodesize=nn
                                 ,maxnodes=mm
                                 ,mtry=mt
      )
      yhatv <- predict(temp_model,newdata=training_data[ids[[2]],])
      temp_err <- compute_error(training_data$nb_departure[ids[[2]]],yhatv, errtype = "RMSE")$rmse
      if (temp_err < terr){
        terr <- temp_err
        hyperpar <- c(tt,nn,mm,mt)
      }
    }}}
    }
    fitted_model <- (randomForest(nb_departure~.
                                  ,data=training_data
                                  ,ntree=hyperpar[1]
                                  ,nodesize=hyperpar[2]
                                  ,maxnodes=hyperpar[3]
                                  ,mtry=hyperpar[4]
    )
    )
    yhat <- predict(fitted_model,newdata=valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
    opstr <- paste("ntree=", hyperpar[1], " nodesize=", hyperpar[2], " maxnodes=", hyperpar[3], "mtry=",hyperpar[4], sep="")
  }
  
  if (model.type == "boost"){
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))
    
    n.trees = c(50,100,200)
    interaction.depth = 5#c(5,7,16)
    shrinkage = 0.5#c(0.02,0.1,0.5)
    
    terr <- 1000000
    for (tt in n.trees){for (id in interaction.depth){for (sh in shrinkage){
      print(paste("     Trying model with: ", tt, " trees, ", id ," max depth, ", sh, " shrinkage."))
      temp_model=gbm(nb_departure~.
                     ,data=training_data[ids[[1]],]
                     ,distribution="gaussian"
                     ,n.trees=tt
                     ,interaction.depth = id
                     ,shrinkage =sh
                     ,verbose = FALSE
      )
      
      yhatv <- predict(temp_model, newdata=training_data[ids[[2]],])
      temp_err <- compute_error(training_data$nb_departure[ids[[2]]],yhatv, errtype = "RMSE")$rmse
      if (temp_err < terr){
        terr <- temp_err
        hyperpar <- c(tt,id,sh)
      }
    }}}
    
    fitted_model=gbm(nb_departure~.
                     ,data=training_data[ids[[1]],]
                     ,distribution="gaussian"
                     ,n.trees=hyperpar[1]
                     ,interaction.depth = hyperpar[2]
                     ,shrinkage = hyperpar[3]
    )
    
    yhat <- predict(fitted_model, newdata=valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
    opstr <- paste("ntree=", hyperpar[1], " treedepth=", hyperpar[2], " epsilon=", hyperpar[3], sep="")
  }
  
  ############################ end model space #############################
  #if (min(yhat)<0 & !min_zero){
  #  print(paste("model", model.type, "predicted negative values down to :",as.character(min(yhat))))
  #}
  
  #if (min_zero){
  #  yhat[yhat<0] <- 0
  #A}
  
  err <- compute_error(valid_data$nb_departure,yhat)
  errtest <- compute_error(test_data$nb_departure,yhattest)
  err$opstr <- opstr
  err$testrmse <- errtest$rmse
  err$testmae <- errtest$mae
  return(err)
}