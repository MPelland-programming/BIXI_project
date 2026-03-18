first_element <- function(ll){
  return(ll[1])
}
first_elements <- function(ll,n){
  return(ll[1:n])
}

# preprocessing function
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

#train_test_ids
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

#get_heatmap
get_heatmap <- function(preproc){
  my_colors <- colorRampPalette(c("white","darkred", "yellow"))(5)
  
  heatmap(abs(cor(preproc$data[,-c(1,9,18,19)])))
  heatmap(abs(cor(preproc$data[,-c(1,9,18,19)])),
         col = my_colors,
         breaks = seq(0.5, 1, length.out = 6),
         scale = "none")
}

#compute_error
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

#get_polymeq
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

#fit_model_equation DEPRECATE
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
                        model.type, "(",equation,", data = training_data)"
                        ,sep = "")))

  return(tmodel)
}

#fit_model_noeq DEPRECATED
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

  return(tmodel)
}

#get_model_matrix
get_model_matrix <- function(equation, data){
  formu <- as.formula(equation)
  x <- model.matrix(formu,data)[,-1]
  return(x)
}

#fit_lm
fit_lm <-  function(equation,training_data){
    fitted_model = lm(equation, data = training_data)
    return(fitted_model)
}

#fit_poisson glm
fit_glm_poisson <-  function(equation,training_data){
    fitted_model = glm(equation, family=poisson(link='log'), data = training_data)
    return(fitted_model)
}

#fit lasso
fit_lasso <- function(xs,ys){
  fitted_model <- cv.glmnet(xs,ys,alpha=1)
  return(fitted_model)
}

#fit ridge
fit_ridge <- function(xs,ys){
  fitted_model <- cv.glmnet(xs,ys,alpha=0)
  return(fitted_model)
}

#fit elasticnet with alpha search
fit_elasticnet <- function(training_data,lalpha){
    #Split to get a validation set to select best alpha
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))

    xs <- get_model_matrix(equation, training_data[ids[[1]],])[,-1]
    ys <- training_data$nb_departure[ids[[1]]]

    xv <- get_model_matrix(equation, training_data[ids[[2]],])[,-1]
    yv <- training_data$nb_departure[ids[[2]]]

    #get id for folds to have always the same.
    foldid <- rep(seq(1,10,by=1),length.out = length(ys))

    terr <- 1000000
    best_alpha = 0
    for (ii in 1:length(lalpha)){
      alpha <- lalpha[ii]
      fitted_model <- cv.glmnet(xs,ys,alpha=alpha,foldid = foldid)

      yhatv <- predict(fitted_model,new=xv, s = "lambda.min")
      temp_err <- compute_error(yv,yhatv, errtype = "RMSE")$rmse
      if (temp_err < terr){
          terr <- temp_err
          best_alpha <- alpha
      }
    }

    #train on all training data
    xfull <- get_model_matrix(equation, training_data)[,-1]
    yfull <- training_data$nb_departure
    fitted_model <- cv.glmnet(xfull,yfull,alpha=best_alpha)####################################################### Should I force the lambad here?
    m.fitted_model <- fitted_model
    m.best_alpha <- best_alpha
    return(m)
}

#fit relaxed elasticnet
fit_relaxednet <- function(training_data){
    formu <- as.formula(equation)
    xs <- model.matrix(formu,training_data)[,-1]
    ys <- training_data$nb_departure

    fitted_model <- cv.glmnet(xs,ys, alpha=1,relax=TRUE)
    return(fitted_model)
}

#fit tree
fit_singletree <- function(equation,training_data,ms,mb,options){
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))

     #hyperparameter search
     terr <- 1000000
     best_mb <- 0
     best_ms <- 0
     for (ii in ms){
       for (jj in mb){
         if (ms*2 > mb){ #avoids making trees that are impossible.
             fitted_tree=rpart(as.formula(equation),
                                data=training_data[ids[[1]],],
                                method=options$method,
                                control = rpart.control(xval = 10, minsplit=ms, minbucket = mb, cp = 0))

             if (options$best == "1se"){
                bstd=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"xstd"]
                berr=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"xerror"]
                minloc <- min(which(fitted_tree$cp[,"xerror"]<berr+bstd))

                fitted_model <- prune(fitted_tree,cp=fitted_tree$cp[minloc,"CP"],"CP"])
             } else {
                fitted_model <- prune(fitted_tree,cp=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"CP"])
             }

             yhatv <- predict(fitted_model,new = training_data[ids[[2]],])
             temp_err <- compute_error(training_data$nb_departure[ids[[2]]],yhatv, errtype = "RMSE")$rmse
             if (temp_err < terr){
               terr <- temp_err
               best_mb <- jj
               best_ms <- ii
             }
         }
       }
     }

    #Refit the model using the best hyperparams
    fitted_tree=rpart(as.formula(equation),
                    data=training_data,
                    method=options$method,
                    control = rpart.control(xval = 10, minsplit=best_ms, minbucket = best_mb, cp = 0))

    if (options$best == "1se"){
       bstd=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"xstd"]
       berr=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"xerror"]
       minloc <- min(which(fitted_tree$cp[,"xerror"]<berr+bstd))

       cp <- fitted_tree$cp[minloc,"CP"]
       fitted_model <- prune(fitted_tree,cp=cp)
    } else {
        cp <- fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"CP"]
        fitted_model <- prune(fitted_tree,cp=cp)
    }
    m.fitted_model <- fitted_model
    m$best_mb <- best_mb
    m$best_ms <- best_ms
    m$cp <- cp
    return(m)
}

#fit randomForest
fit_randomForest <- function(training_data,ntree,nodesize,maxnodes,mtry){
    terr <- 1000000
    for (tt in ntree){for (nn in nodesize){for (mm in maxnodes){for (mt in mtry){
        print(paste("     Trying model with: ", tt, " trees, ",nn," node size, ", mm, " max nodes,", mt, " variables"))
        temp_model <- randomForest(nb_departure~.
                                    ,data=training_data
                                    ,ntree=tt
                                    ,nodesize=nn
                                    ,maxnodes=mm
                                    ,mtry=mt
                                    )

        temp_err <- temp_model$mse[length(rf$mse)] #get the oob mse.
        if (temp_err < terr){
          fitted_model = temp_model
          terr <- temp_err
          hyperpar <- c(tt,nn,mm,mt)
        }
    }}}
    }
    m$fitted_model <- fitted_model
    m$hyperpar <- hyperpar
    return(m)
}

#fit_boosted_tree
fit_boosted_tree <- function(training_data,n.trees,interaction.depth,shrinkage){
    #Split to get a validation set to select best alpha
    ids <- train_test_ids(training_data,n_split = 2,prop = c(0.8,0.2))

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

    m$fitted_model <- fitted_model
    m$hyperpar <- hyperpar
    return(m)
}

############################
# Main function
#############################
fit_predict_err <-  function(model.type,equation,opts,training_data,valid_data,test_data,min_zero = TRUE){

  opstr <- ""
  ################ begin model space #############################
  # For each model, specify hyperparameter space and launch the fitting part.
  #baseline
  if (model.type == "baseline"){
    yhat <- rep(mean(training_data$nb_departure),length(valid_data$nb_departure))
    yhattest <- rep(mean(training_data$nb_departure),length(test_data$nb_departure))
  }

  #linear model
  if (model.type == "lm"){
    fitted_model <- fit_lm(equation,training_data)
    yhat <- predict(fitted_model,newdata = valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
  }

  #poisson glm
  if (model.type == "glm_poisson"){
    fitted_model <- fit_glm_poisson(equation,training_data)
    yhat <- predict(fitted_model,newdata = valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
  }

  #lasso
  if (model.type == "lasso"){
    xs <- get_model_matrix(equation, training_data)
    ys <- training_data$nb_departure
    xvalid <- get_model_matrix(equation, valid_data)

    fitted_model <- fit_lasso(xs,ys)

    if (opts != "1se"){
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.min")
      opstr <- fitted_model$lambda.min
    } else {
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
      opstr <- fitted_model$lambda.1se
    }
  }

  #ridge
  if (model.type == "ridge"){
    xs <- get_model_matrix(equation, training_data)
    ys <- training_data$nb_departure
    xvalid <- get_model_matrix(equation, valid_data)

    fitted_model <- fit_ridge(xs,ys)

    if (opts != "1se"){
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.min")
      opstr <- fitted_model$lambda.min
    } else {
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
      opstr <- fitted_model$lambda.1se
    }
  }

  #elasticnet
  if (model.type == "elasticnet"){
    lalpha <- seq(0,1,by=0.25)

    xvalid <- model.matrix(formu,valid_data)[,-1]
    xtest <-  model.matrix(formu,test_data)[,-1]

    m <- fit_elasticnet(training_data,lalpha)
    fitted_model <- m$fitted_model
    best_alpha <- m$best_alpha

    if (opts != "1se"){
        yhat <- predict(fitted_model,new=xvalid, s = "lambda.min")
        yhattest <- predict(fitted_model, new=xtest, s = "lambda.min")
        opstr <- paste("alpha=",best_alpha,"lambda=",fitted_model$lambda.min,sep="")
    } else {
        yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
        yhattest <- predict(fitted_model, new=xtest, s = "lambda.1se")
        opstr <- paste("alpha=",best_alpha,"lambda=",fitted_model$lambda.1se,sep="")
    }
  }

  #Relaxed net
  if (model.type == "relaxlasso"){
    #https://www.jstatsoft.org/article/view/v106i01 for why not using other alphas.
    #while the relaxation can be applied for α values smaller than 1, we do not recommend
    #doing this. Relaxation is typically applied to obtain sparser models. It achieves this
    #by undoing shrinkage of coefficients in the active set toward zero... Selecting α smaller
    #than 1 results in a larger active set than that for the lasso, working against the goal of obtaining a sparser model.

    xvalid <- model.matrix(formu,valid_data)[,-1]
    xtest <- model.matrix(formu,test_data)[,-1]

    fitted_model <- fit_relaxednet(training_data)

    if (opts == "1se"){
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se", gamma = "gamma.1se")
      opstr <- paste("Lambda = ",fitted_model$lambda.1se, ", gamma = ", fitted_model$relaxed$gamma.1se, sep = "")
    } else {
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.min", gamma = "gamma.min")
      opstr <- paste("Lambda = ",fitted_model$lambda.min, ", gamma = ", fitted_model$relaxed$gamma.min, sep = "")
    }
  }

  #single tree regression and poisson
  if (model.type == "singletree"){
    #fits a single tree while varying minsplit, minbucket.
    ms <- c(5,10,15,20,25,30)
    mb <-  c(3,6,9,12,15)

    params <- strsplit(trimws(strsplit(opts, ",")[[1]]), "=")
    treeopts <- list()
    treeopts$method <- params[[1]][2]
    treeopts$best  <- params[[2]][2]

    #Split to get a validation set to select best alpha
    m <- fit_singletree(equation,training_data,ms,mb,treeopts)

    yhat <- predict(m.fitted_model,new=valid_data)
    yhattest <- predict(m.fitted_model, newdata=test_data)
    opstr <- paste("minsplit=", m.best_ms, " minbucket=", m.best_mb, " cp=", m.cp, sep="")
  }

  if (model.type == "condtree"){
    #fits a single tree
    fitted_model <- ctree(as.formula(equation), data = training_data)
    yhat <- predict(fitted_model,newdata=valid_data,type="response")
    yhattest <- predict(fitted_model, newdata=test_data, type = "response")
  }

  #baseforest
  if (model.type == "baseforest"){
    #fits a forest
    ntree = c(200, 300, 400)
    nodesize = c(10, 50, 100)
    maxnodes = c(100,200)
    mtry = floor(c(0.33, 0.5,0.83)* dim(training_data)[2])

    m <- fit_randomForest(training_data,ntree,nodesize,maxnodes,mtry)
    fitted_model <- m$model
    hyperpar <- m$hyperpar

    yhat <- predict(fitted_model,newdata=valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
    opstr <- paste("ntree=", hyperpar[1], " nodesize=", hyperpar[2], " maxnodes=", hyperpar[3], "mtry=",hyperpar[4], sep="")
  }

  #boosted tree
  if (model.type == "boosttree"){

    n.trees = c(50,100,200)
    interaction.depth = 5
    shrinkage = 0.5

    m <- fit_boosted_tree(training_data,n.trees,interaction.depth,shrinkage)

    yhat <- predict(m.fitted_model, newdata=valid_data)
    yhattest <- predict(m.fitted_model, newdata=test_data)
    opstr <- paste("ntree=", m.hyperpar[1], " treedepth=", m.hyperpar[2], " epsilon=", m.hyperpar[3], sep="")
  }

  ############################ end model space #############################S

  err <- compute_error(valid_data$nb_departure,yhat)
  errtest <- compute_error(test_data$nb_departure,yhattest)
  err$opstr <- opstr
  err$testrmse <- errtest$rmse
  err$testmae <- errtest$mae
  return(err)
}