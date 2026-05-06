first_element <- function(ll){
  return(ll[1])
}
first_elements <- function(ll,n){
  return(ll[1:n])
}

# preprocessing function
preprocessing <- function(rawdata,sloc,remove.na = TRUE){
  # Replace long station names solely by their numbers, provides a dictionary to go from one to the other. 
  # Finds minimum for nu of departures and rescales the variable to whol numbers. 
  # Replaces dates with day of the week. 
  # For days, casts as factor and sets Monday as reference cat
  # For holiday, casts as factor and sets non-holidays as reference

  #First, we need to transform the coordinates to meters since the distances are a bit off wth latitude and longitude
  pts <- st_as_sf(sloc, coords = c("longitude", "latitude"), crs = 4326)
  pts_m <- data.frame(st_coordinates(st_transform(pts, crs = 32618))) # get distances in meters
  names(pts_m) <- c("longitude","latitude")
  pts_m$longitude <- pts_m$longitude - min(pts_m$longitude) + 10000
  pts_m$latitude <- pts_m$latitude - min(pts_m$latitude) + 10000
  pts_m$location <- sloc$location

  rawdata <- inner_join(rawdata,pts_m,by="location")


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

  #Get months
  month <- months(preproc_data$data$time)
  preproc_data$data$months <- month

  #Add distance to centre
  dist <- abs(preproc_data$data$latitude - mean(preproc_data$data$latitude))
  preproc_data$data$latitude_dist <- dist/max(dist)

  dist <- abs(preproc_data$data$longitude - mean(preproc_data$data$longitude))
  preproc_data$data$longitude_dist <- dist/max(dist)
  
  #Cast some vars as factors
  preproc_data$data$months <- relevel(as.factor(preproc_data$data$months), ref = "April")
  preproc_data$data$days <- relevel(as.factor(preproc_data$data$days), ref = "Mon")
  preproc_data$data$holiday <- relevel(as.factor(preproc_data$data$holiday), ref = "0")
  preproc_data$data$num_university <- relevel(as.factor(preproc_data$data$num_university), ref = "0")
  preproc_data$station_id <- as.factor(preproc_data$station_id)
  #Get station ids
  preproc_data$data$stationid <- as.factor(preproc_data$data$location)
  
  #Return only relevant columns

  preproc_data$data <- preproc_data$data[-c(1,2,18)] #add 11 if want to remove universities

  
  return(preproc_data)
}

#aggregate data
aggregate_by_station <- function(indata){
  #In data must contain a dictionary of station id.
  #sloc containt latitude and longitude
  name_dict <- indata$name_dict
  dat <- indata$data

  agg_nb_departure <- aggregate(dat["nb_departure"]
                                , by = list(dat$stationid)
                                , FUN = mean
  )


  #Make sure all values regarding environment of station are the same across entries
  print("Inspecting data entries for: ")
  cols <- c("area_park","len_cycle_path","len_major_road","len_minor_road","num_metro_stations","num_other_commercial","num_restaurants","num_university","num_pop","num_bus_stations","num_bus_routes","walkscore","capacity")
  for (vv in cols){
    print(vv)
    for (ll in unique(dat$stationid)){
      if (length(unique(dat[vv])) != 1){
        print(paste("there are more than one values for ",vv , " in " , ll))
      }
    }
  }

  agg_other <- aggregate(dat[c(cols,"latitude","longitude")]
                         , by = list(dat$stationid)
                         , FUN = first_element
  )

  aggdata <- merge(agg_nb_departure, agg_other, by = "Group.1", all.x=TRUE, all.y=TRUE)

  names(aggdata)[1]<-"stationid"

  aggdata$walkscore <- aggdata$walkscore^2

  return(aggdata)
}

#train_test_ids
train_test_ids <- function(dat,n_split = 2,prop = c(0.8,0.2),strata_df = NULL){
  #Finds ids to split the dataset into n_split number of sub datasets.
  #The proportion of data in each is specified by prop. Note that the
  #last element of prop is not used since we want fully use the data set.

  if(is.null(strata_df)){
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
  }else{
    return("No stratification implemented yet")
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
compute_error <- function(true_y,predicted_y, errtype = "all",minval=0,model_name = ""){
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

  if(model_name != ""){
    #print graph of residuals.
    lpath <- "/home/hereinlies/Documents/Documents/Ecole/HEC/Math70611_AdvancedMetho/Projet/code/BIXI_project"
    mlocal <- here() == lpath #only print graphs if on local machine of creator

      if (mlocal){
        png(file = paste(lpath,"/",model_name,".png",sep=""),
            width = 4, # The width of the plot in inches
            height = 4,
            units = "in",
            res = 300)
        plot(true_y, predicted_y)
        dev.off()
  }}

  return(err)
}

#get_polymeq
get_polymeq <- function(df,np,yvar = "nb_departure"){
    #takes input as data frame and gets the n polynoms for
    #each varialble that is not a factor.
    lvar <- vector()

    tnam <- colnames(df)[!colnames(df) %in% c("stationid","latitude","longitude")]
    cnam <- tnam[-which(tnam == "nb_departure")]

    for (ii in 1:length(cnam)){
      for (jj in c(0.5,2:np )){
         if (!is.factor(df[[cnam[ii]]])){
            lvar <- c(lvar,paste("I(",cnam[ii],"^",jj,")",sep=""))
         }
      }
    }
    eq <- paste(lvar, collapse = " + ")
    return(eq)
}

#get interactions
get_inteq <- function(df,yvar = "nb_departure",exceptpairs=""){
    #takes input as ddata and writes down interactions
    #Pairs can be excluded with the three lines below
    if (exceptpairs == ""){
      exceptpairs <- c("days","holiday","months")
    }

    tnam <- colnames(df)[!colnames(df) %in% c("stationid","latitude","longitude")]
    lvar <- tnam[-which(tnam == "nb_departure")]

    interactions <- vector()
    for (ii in 1:(length(lvar)-1)){
      for (jj in (ii+1):length(lvar)){
         if (!any(sapply(exceptpairs, grepl, lvar[ii])) | !any(sapply(exceptpairs, grepl, lvar[jj]))){
           interactions <- c(interactions, paste(lvar[ii], lvar[jj], sep=":"))
         }
      }
    }
    eq <- paste(interactions, collapse = " + ")
    return(eq)
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
fit_elasticnet <- function(equation,training_data,lalpha){
    xs <- get_model_matrix(equation, training_data)
    ys <- training_data$nb_departure

    #get id for folds to have always the same.
    foldid <- sample(rep(seq(1,10,by=1),length.out = length(ys)))

    terr <- 1000000
    best_alpha = 0

    for (ii in 1:length(lalpha)){
      alpha <- lalpha[ii]
      fitted_model <- cv.glmnet(xs,ys,alpha=alpha,foldid = foldid)

      temp_err <- min(fitted_model$cvm)
      if (temp_err < terr){
          terr <- temp_err
          best_alpha <- alpha
          best_model <- fitted_model
      }
    }

    m <- list()
    m$fitted_model <- best_model
    m$best_alpha <- best_alpha
    return(m)
}

#fit relaxed elasticnet
fit_relaxednet <- function(equation,training_data,lalpha=1){
    xs <- get_model_matrix(equation, training_data)
    ys <- training_data$nb_departure

    #get id for folds to have always the same.
    foldid <- sample(rep(seq(1,10,by=1),length.out = length(ys)))

    terr <- 1000000
    best_alpha = 1

    for (ii in 1:length(lalpha)){
      alpha <- lalpha[ii]
      fitted_model <- cv.glmnet(xs,ys,alpha=alpha,foldid = foldid, relax = TRUE)

      temp_err <- min(fitted_model$cvm)
      if (temp_err < terr){
          terr <- temp_err
          best_alpha <- alpha
          best_model <- fitted_model
      }
    }

    m <- list()
    m$fitted_model <- best_model
    m$best_alpha <- best_alpha
    return(m)
}

#fit forward stepwise regression
fit_forward <- function(equation,training_data,valid_data, maxpass = 200, min_improv = 0.01,initeq = ""){
    #Get intercept only model as baseline
    current_model <- lm(nb_departure ~ 1, data = training_data)
    currerr <- compute_error(valid_data$nb_departure, predict(current_model, newdata = valid_data), errtype = "RMSE")$rmse

    equation <- gsub(" ", "", equation, fixed = TRUE)
    vars <- strsplit(equation,"\\+|\\~")[[1]]

    yvar <- vars[1]

    if (initeq != ""){
        initeq <- gsub(" ", "", initeq, fixed = TRUE)
        initvars <- strsplit(initeq,"\\+|\\~")[[1]]
        curreq <- paste(yvar, "~", paste(initvars, collapse="+"), sep="")
        varout <- vars[! vars %in% initvars & vars != yvar]
    } else {
        curreq <- paste(yvar,"~1",sep="")
        varout <- vars[-1]
    }

    imp <- TRUE
    endloop <- FALSE
    npass <- 1
    while(imp | !length(valid)==0){
        npass <- npass + 1

        print(paste("Now fitting model with ", npass ," parameters",sep=""))

        bestvar <- NULL
        besterr <- currerr
        for(nn in varout){
            tempeq <- paste(curreq, nn, sep="+")
            temp_model <- lm(tempeq, data = training_data)

            temp_err <- compute_error(valid_data$nb_departure
                                       , predict(temp_model, newdata = valid_data)
                                       , errtype = "RMSE")$rmse

            if(temp_err < besterr){
                besterr <- temp_err
                bestvar <- nn
            }
        }

        if (besterr < currerr-min_improv & npass < maxpass){
            currerr <- besterr
            curreq <- paste(curreq, bestvar, sep="+")
            varout <- varout[! varout %in% bestvar]
            currmod <- temp_model

            if(length(varout) == 0){
               endloop <- TRUE
            }
        } else {endloop <- TRUE}

        if(endloop){
            imp <- FALSE
            m <- list()
            m$best_model <- currmod
            m$nvars <- length(strsplit(curreq,"\\+|\\~")[[1]])-1
            m$eq <- curreq
            m$err <- currerr
            return(m)
        }

    }
}

#fit tree
fit_singletree <- function(equation,training_data,valid_data,ms,mb,options){

     #hyperparameter search
     terr <- 1000000
     best_mb <- 0
     best_ms <- 0
     for (ii in ms){
       for (jj in mb){
         if (ii*2 > jj){ #avoids making trees that are impossible.

             fitted_tree=rpart(as.formula(equation),
                                data=training_data,
                                method=options$method,
                                control = rpart.control(xval = 10, minsplit=ii, minbucket = jj, cp = 0))

             if (options$best == "1se"){
                bstd=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"xstd"]
                berr=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"xerror"]
                minloc <- min(which(fitted_tree$cp[,"xerror"]<berr+bstd))

                fitted_model <- prune(fitted_tree,cp=fitted_tree$cp[minloc,"CP"])
             } else {
                fitted_model <- prune(fitted_tree,cp=fitted_tree$cp[which.min(fitted_tree$cp[,"xerror"]),"CP"])
             }

             yhatv <- predict(fitted_model,new = valid_data)
             temp_err <- compute_error(valid_data$nb_departure,yhatv, errtype = "RMSE")$rmse

             if (temp_err < terr){
               terr <- temp_err
               best_mb <- jj
               best_ms <- ii
               best_model <- fitted_model
             }
         }
       }
     }

    if (options$best == "1se"){
       bstd=fitted_model$cp[which.min(fitted_model$cp[,"xerror"]),"xstd"]
       berr=fitted_model$cp[which.min(fitted_model$cp[,"xerror"]),"xerror"]
       minloc <- min(which(fitted_model$cp[,"xerror"]<berr+bstd))

       cp <- fitted_model$cp[minloc,"CP"]
    } else {
        cp <- fitted_model$cp[which.min(fitted_model$cp[,"xerror"]),"CP"]
    }
    m <- list()
    m$fitted_model <- fitted_model
    m$best_mb <- best_mb
    m$best_ms <- best_ms
    m$cp <- cp
    return(m)
}

#fit randomForest
fit_randomForest <- function(equation,training_data,ntree,nodesize,maxnodes,mtry){
    terr <- 1000000
    for (tt in ntree){for (nn in nodesize){for (mm in maxnodes){for (mt in mtry){
        print(paste("     Trying model with: ", tt, " trees, ",nn," node size, ", mm, " max nodes,", mt, " variables"))
        temp_model <- randomForest(as.formula(equation)
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
    m <- list()
    m$fitted_model <- fitted_model
    m$hyperpar <- hyperpar
    return(m)
}

#Random forest version 2
fit_randomForestRanger <- function(equation,training_data,ntree,nodesize,minbucket,mtry,maxdepth){
    terr <- 1000000
    for (tt in ntree){for (nn in nodesize)
        print(paste("     Trying model with: ", tt, " trees and ",nn," node size"))
    {for (mm in minbucket){for (mt in mtry){for(md in maxdepth){

        temp_model <- ranger(as.formula(equation)
                                    ,data=training_data
                                    ,num.trees=tt
                                    ,min.node.size=nn
                                    ,min.bucket=mm
                                    ,mtry=mt
                                    ,max.depth=md
                                    )

        temp_err <- temp_model$prediction.error #get the oob mse.
        if (temp_err < terr){
          fitted_model <- temp_model
          terr <- temp_err
          hyperpar <- c(tt,nn,mm,mt,md)
        }
    }}}}
    }
    m <- list()
    m$model <- fitted_model
    m$hyperpar <- hyperpar
    return(m)
}

#fit_boosted_tree
fit_boosted_tree <- function(equation,training_data,valid_data,n.trees,interaction.depth,shrinkage){
    #Split to get a validation set to select best alpha
    terr <- 1000000
    for (tt in n.trees){for (id in interaction.depth){for (sh in shrinkage){
      print(paste("     Trying model with: ", tt, " trees, ", id ," max depth, ", sh, " shrinkage."))
      temp_model=gbm(as.formula(equation)
                      ,data=training_data
                      ,distribution="gaussian"
                      ,n.trees=tt
                      ,interaction.depth = id
                      ,shrinkage =sh
                      ,verbose = FALSE
                      )

      yhatv <- predict(temp_model, newdata=valid_data)
      temp_err <- compute_error(valid_data$nb_departure,yhatv, errtype = "RMSE")$rmse
      if (temp_err < terr){
        terr <- temp_err
        hyperpar <- c(tt,id,sh)
        fitted_model <- temp_model
      }
    }}}
    m <- list()
    m$fitted_model <- fitted_model
    m$hyperpar <- hyperpar
    return(m)
}

#fit boostreg
fit_boostreg <- function(equation,training_data,niter,shrink){
    #Split to get a validation set to select best alpha
    #We can extract the MSE from the cvrisk output which essentially lists MSE of OOB (25 bootstrap samples)
    #Technically, the doc says they are weighted, but they are all weighted to 1 (I checked manually)

    terr <- 1000000
    for (sh in shrink){for(ni in niter){
      print(paste("     Trying model with: ", ni, " iterations, ", sh ," shrinkage."))
      temp_model=glmboost(as.formula(equation),
                      data=training_data,
                      control=boost_control(mstop=ni,nu=sh)
                      )

      cv_error=cvrisk(temp_model,grid = 1:floor(ni/50)*50)
      temp_err <- min(cv_error)
      best_iter=mstop(cv_error)

      if (temp_err < terr){
        terr <- temp_err
        fmstop <- ni
        fshrink <- sh
        fncoef <- length(coef(temp_model[best_iter]))
        fitted_model <- temp_model[best_iter]
      }
    }}
    m <- list()
    m$fitted_model <- fitted_model
    m$n_iter <- fmstop
    m$shrink <- fshrink
    m$ncoef <- fncoef
    return(m)

}

#fit randomintercept
fit_randintercept <- function(equation,training_data){
    fitted_model <- lme(as.formula(equation), data = training_data, random = ~1|stationid)
}

#fit kriging
fit_kriging <- function(equation,training_data,valid_data,n_fold = 5,vgmodel = "Sph"){
  dat <- training_data
  folds <- train_test_ids(dat,n_fold,rep(1/n_fold,n_fold))

  coordinates(dat) <- ~longitude+latitude
  formu <- as.formula(equation)

  best_mse <- 10000000
  list_err <- list()
  for(vg in vgmodel){
      cumul_mse <- 0
      cumul_mae <- 0

      for(ff in folds){
        ff <- unlist(ff)
        v_emp = variogram(formu, dat[-ff,])
        v_fit <- fit.variogram(v_emp, vgm(vg),debug.level = 0)
        k <- gstat(formula = formu, data = dat[-ff,], model = v_fit)
        kpred <- predict(k, dat[ff,],debug.level = 0)$var1.pred

        #update error term
        temp_err <- compute_error(kpred,dat$nb_departure[ff])
        cumul_mse <- cumul_mse + temp_err$mse
        cumul_mae <- cumul_mae + temp_err$mae

      }

      if (cumul_mse < best_mse){
        best_mse <- cumul_mse
        best_mae <- cumul_mae
        best_vgmodel <- vg
      }
  }
  m <- list()
  m$best_vg <- best_vgmodel
  m$valid_rmse <- sqrt(best_mse/n_fold)
  m$valid_mae <- best_mae/n_fold
  m$k <- gstat(formula = formu, data = dat, model = v_fit)

  lpath <- "/home/hereinlies/Documents/Documents/Ecole/HEC/Math70611_AdvancedMetho/Projet/code/BIXI_project"
  mlocal <- here() == lpath #only print graphs if on local machine of creator

  if (mlocal){
    v_emp = variogram(formu, dat)
    v_fit <- fit.variogram(v_emp, vgm(best_vgmodel) ,debug.level = 0)

    fnam <- paste("variogram_",best_vgmodel,".png",sep="")
    print(paste(lpath,"/",fnam,sep=""))
    png(file = paste(lpath,"/",fnam,sep=""),
        width = 4, # The width of the plot in inches
        height = 4,
        units = "in",
        res = 300)

    print(plot(v_emp,model=v_fit))

    dev.off()

  }

  return(m)
}

#fit forest kriging
fit_boost_kriging <- function(equation,training_data,valid_data,n_fold = 5,vgmodel = "Sph"){
  dat <- training_data
  dat_df <- as.data.frame(dat)

  folds <- train_test_ids(dat,n_fold,rep(1/n_fold,n_fold))

  coordinates(dat) <- ~longitude+latitude
  formu <- as.formula(equation)

  best_mse <- 10000000
  list_err <- list()
  for(vg in vgmodel){
      cumul_mse <- 0
      cumul_mae <- 0

      for(ff in folds){
        ff <- unlist(ff)

        m <- fit_boosted_tree(equation,dat_df[-ff,],valid_data,100,1,0.5)

        ############### make model#################
        #Get predicted mean
        yhat <- predict(m$fitted_model,newdata=dat_df[-ff,])
        res <- dat$nb_departure[-ff] - yhat
        resid_sp <- dat[-ff,]  # keep spatial info
        resid_sp$residuals <- res

        #fit variogramme to residuals
        v_emp <- variogram(residuals ~ 1, resid_sp)
        v_fit <- fit.variogram(v_emp, vgm(vg),debug.level = 0)

        ################# predict data ###################
        #get predicted mean
        mpred <- predict(m$fitted_model,newdata=dat_df[ff,])
        #res <- dat$nb_departure[ff] - mpred
        #resid_pred <- dat[ff,]  # keep spatial info
        #resid_pred$residuals <- res

        k <- gstat(formula = residuals ~ 1, data = resid_sp, model = v_fit)
        vpred <- predict(k, dat[ff,],debug.level = 0)$var1.pred

        kpred <- mpred + vpred

        #update error term
        temp_err <- compute_error(kpred,dat$nb_departure[ff])
        cumul_mse <- cumul_mse + temp_err$mse
        cumul_mae <- cumul_mae + temp_err$mae

      }

      if (cumul_mse < best_mse){
        best_mse <- cumul_mse
        best_mae <- cumul_mae
        best_vgmodel <- vg
      }
  }

  ff <- fit_boosted_tree(equation,dat_df,valid_data,100,1,0.5)
  mpred <- predict(ff$fitted_model,newdata=dat_df)
  res <- dat$nb_departure - mpred
  resid_pred <- dat  # keep spatial info
  resid_pred$residuals <- res

  vv <- variogram(residuals ~ 1, resid_pred)
  v_fit <- fit.variogram(vv, vgm(best_vgmodel),debug.level = 0)

  k <- gstat(formula = residuals ~ 1, data = resid_pred, model = v_fit)

  m <- list()
  m$best_vg <- best_vgmodel
  m$valid_rmse <- sqrt(best_mse/n_fold)
  m$valid_mae <- best_mae/n_fold
  m$mean <- ff$fitted_model
  m$var <- k

  lpath <- "/home/hereinlies/Documents/Documents/Ecole/HEC/Math70611_AdvancedMetho/Projet/code/BIXI_project"
  mlocal <- here() == lpath #only print graphs if on local machine of creator

  if (mlocal){

    fnam <- paste("forest_variogram_",best_vgmodel,".png",sep="")
    print(paste(lpath,"/",fnam,sep=""))
    png(file = paste(lpath,"/",fnam,sep=""),
        width = 4, # The width of the plot in inches
        height = 4,
        units = "in",
        res = 300)

    print(plot(v_emp,model=v_fit))

    dev.off()

  }

  return(m)
}

#fit linear kriging
fit_linear_kriging <- function(equation,training_data,valid_data,n_fold = 5,vgmodel = "Sph"){
  dat <- training_data
  dat_df <- as.data.frame(dat)

  folds <- train_test_ids(dat,n_fold,rep(1/n_fold,n_fold))

  coordinates(dat) <- ~longitude+latitude
  formu <- as.formula(equation)

  best_mse <- 10000000
  list_err <- list()
  for(vg in vgmodel){
      cumul_mse <- 0
      cumul_mae <- 0

      for(ff in folds){
        ff <- unlist(ff)

        m<-list()
        m$fitted_model <- fit_lm(equation,dat_df[-ff,])

        ############### make model#################
        #Get predicted mean
        yhat <- predict(m$fitted_model,newdata=dat_df[-ff,])
        res <- dat$nb_departure[-ff] - yhat
        resid_sp <- dat[-ff,]  # keep spatial info
        resid_sp$residuals <- res

        #fit variogramme to residuals
        v_emp <- variogram(residuals ~ 1, resid_sp)
        v_fit <- fit.variogram(v_emp, vgm(vg),debug.level = 0)

        ################# predict data ###################
        #get predicted mean
        mpred <- predict(m$fitted_model,newdata=dat_df[ff,])

        k <- gstat(formula = residuals ~ 1, data = resid_sp, model = v_fit)
        vpred <- predict(k, dat[ff,],debug.level = 0)$var1.pred

        kpred <- mpred + vpred

        #update error term
        temp_err <- compute_error(kpred,dat$nb_departure[ff])
        cumul_mse <- cumul_mse + temp_err$mse
        cumul_mae <- cumul_mae + temp_err$mae

      }

      if (cumul_mse < best_mse){
        best_mse <- cumul_mse
        best_mae <- cumul_mae
        best_vgmodel <- vg
      }
  }

  ff <- list()
  ff$fitted_model <- fit_lm(equation,dat_df)
  mpred <- predict(ff$fitted_model,newdata=dat_df)
  res <- dat$nb_departure - mpred
  resid_pred <- dat  # keep spatial info
  resid_pred$residuals <- res

  vv <- variogram(residuals ~ 1, resid_pred)
  v_fit <- fit.variogram(vv, vgm(best_vgmodel),debug.level = 0)

  k <- gstat(formula = residuals ~ 1, data = resid_pred, model = v_fit)

  m <- list()
  m$best_vg <- best_vgmodel
  m$valid_rmse <- sqrt(best_mse/n_fold)
  m$valid_mae <- best_mae/n_fold
  m$mean <- ff$fitted_model
  m$var <- k

  lpath <- "/home/hereinlies/Documents/Documents/Ecole/HEC/Math70611_AdvancedMetho/Projet/code/BIXI_project"
  mlocal <- here() == lpath #only print graphs if on local machine of creator

  if (mlocal){

    fnam <- paste("linear_variogram_",best_vgmodel,".png",sep="")
    print(paste(lpath,"/",fnam,sep=""))
    png(file = paste(lpath,"/",fnam,sep=""),
        width = 4, # The width of the plot in inches
        height = 4,
        units = "in",
        res = 300)

    print(plot(v_emp,model=v_fit))

    dev.off()

  }

  return(m)
}

############################
# Main function
#############################
fit_predict_err <-  function(model.name,model.type,equation,opts,training_data,valid_data,test_data,min_zero = TRUE){
  err <- NULL
  opstr <- ""
  ################ begin model space ########################<- #####
  # For each model, specify hyperparameter space and launch the fitting part.
  #baseline
  if (model.type == "baseline"){
    yhat <- rep(mean(training_data$nb_departure),length(valid_data$nb_departure))
    yhattest <- rep(mean(training_data$nb_departure),length(test_data$nb_departure))
  }

  #kriging
  if(model.type == "kriging"){
    vgmodel <- c("Nug", "Exp", "Sph")#, "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Wav", "Pow", "Leg")

    m <- fit_kriging(equation,training_data,valid_data,n_fold = 5,vgmodel = vgmodel)

    coordinates(test_data) <- ~longitude+latitude

    err <- list()
    err$rmse <- m$valid_rmse
    err$mae <-  m$valid_mae
    yhattest <- predict(m$k, test_data)$var1.pred
    opstr <- paste("Best variogram model: ", m$best_vg, sep="")
  }

  #kringing boost
  if(model.type == "boostkriging"){
    vgmodel <- c("Nug", "Exp", "Sph")#, "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Wav", "Pow", "Leg")

    coordtest <- test_data
    m <- fit_boost_kriging(equation,training_data,valid_data,n_fold = 5,vgmodel = vgmodel)

    coordinates(coordtest) <- ~longitude+latitude

    err <- list()
    err$rmse <- m$valid_rmse
    err$mae <-  m$valid_mae

    meanest <- predict(m$mean,newdata=test_data)
    varest <- predict(m$var, coordtest)$var1.pred

    yhattest <- meanest+varest
    opstr <- paste("Best variogram model: ", m$best_vg, sep="")

  }
  #kringing linear
  if(model.type == "linearkriging"){
    vgmodel <- c("Nug", "Exp", "Sph")#, "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Wav", "Pow", "Leg")

    coordtest <- test_data
    m <- fit_linear_kriging(equation,training_data,valid_data,n_fold = 5,vgmodel = vgmodel)

    coordinates(coordtest) <- ~longitude+latitude

    err <- list()
    err$rmse <- m$valid_rmse
    err$mae <-  m$valid_mae

    meanest <- predict(m$mean,newdata=test_data)
    varest <- predict(m$var, coordtest)$var1.pred

    yhattest <- meanest+varest
    opstr <- paste("Best variogram model: ", m$best_vg, sep="")

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

    xvalid <- get_model_matrix(equation,valid_data)
    xtest <-  get_model_matrix(equation,test_data)

    m <- fit_elasticnet(equation,training_data,lalpha)
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

    lalpha <- c(0.7, 1)

    xvalid <- get_model_matrix(as.formula(equation),valid_data)
    xtest <- get_model_matrix(as.formula(equation),test_data)

    m <- fit_relaxednet(equation,training_data,lalpha = lalpha)
    fitted_model <- m$fitted_model
    alpha <- m$best_alpha

    if (opts == "1se"){
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se", gamma = "gamma.1se")
      yhattest <- predict(fitted_model,new=xtest, s = "lambda.1se", gamma = "gamma.1se")
      opstr <- paste("Lambda = ",fitted_model$lambda.1se, ", gamma = ", fitted_model$relaxed$gamma.1se, sep = "")
    } else {
      yhat <- predict(fitted_model,new=xvalid, s = "lambda.min", gamma = "gamma.min")
      yhattest <- predict(fitted_model,new=xtest, s = "lambda.min", gamma = "gamma.min")
      opstr <- paste("Lambda = ",fitted_model$lambda.min, ", gamma = ", fitted_model$relaxed$gamma.min, ", alpha = ", alpha, sep = "")
    }
  }

  #Forward
  if(model.type=="forward"){
    m <- fit_forward(equation,training_data,valid_data, maxpass = 200, min_improv = 0.1,initeq = "")
    fitted_model <- m$best_model
    eq <- m$eq
    nvar <- m$nvars

    opstr <- paste("number of predictors =",nvar,sep="")

    yhat <- predict(fitted_model,newdata = valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
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


    m <- fit_singletree(equation,training_data,valid_data,ms,mb,treeopts)

    yhat <- predict(m$fitted_model,new=valid_data)
    yhattest <- predict(m$fitted_model, newdata=test_data)
    opstr <- paste("minsplit=", m$best_ms, " minbucket=", m$best_mb, " cp=", m$cp, sep="")
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

    m <- fit_randomForest(equation,training_data,ntree,nodesize,maxnodes,mtry)
    fitted_model <- m$model
    hyperpar <- m$hyperpar

    yhat <- predict(fitted_model,newdata=valid_data)
    yhattest <- predict(fitted_model, newdata=test_data)
    opstr <- paste("ntree=", hyperpar[1], " nodesize=", hyperpar[2], " maxnodes=", hyperpar[3], "mtry=",hyperpar[4], sep="")
  }

  #rangerforest
  if (model.type == "rangerforest"){
    #fits a forest
    ntree = c(400, 500, 600)
    nodesize = c(5, 10, 50)
    minbucket = c(1, 4, 8)
    maxdepth = c(0, 0.10, 0.25)
    nx <- length(strsplit(equation,"\\+|\\~")[[1]])
    mtry = c(floor(sqrt(nx)),floor(c(0.3, 0.4, 0.5)*nx))

    m <- fit_randomForestRanger(equation,training_data,ntree,nodesize,minbucket,mtry,maxdepth)
    fitted_model <- m$model
    hyperpar <- m$hyperpar

    yhat <- predict(fitted_model,data=valid_data)$predictions
    yhattest <- predict(fitted_model, data=test_data)$predictions
    opstr <- paste("ntree=", hyperpar[1], " nodesize=", hyperpar[2], " maxnodes=", hyperpar[3], "mtry=",hyperpar[4]," maxdepth =",hyperpar[4], sep="")
  }
  #boosted regression
  if (model.type =="boostreg"){
    niter <- 3000
    shrink <- c(0.2,0.1, 0.05)

    m <- fit_boostreg(equation,training_data,niter,shrink)

    yhat <- predict(m$fitted_model,new=valid_data)
    yhattest <- predict(m$fitted_model, new=test_data)
    opstr <- paste("number_iterations=", m$n_iter, " shrinkage=", m$shrink, " number_coefficients=", m$ncoef, sep="")
  }

  #boosted tree
  if (model.type == "boosttree"){
    n.trees = c(100,200,300)
    interaction.depth = c(1,2,3)
    shrinkage = 0.5

    m <- fit_boosted_tree(equation,training_data, valid_data, n.trees,interaction.depth,shrinkage)

    yhat <- predict(m$fitted_model, newdata=valid_data)
    yhattest <- predict(m$fitted_model, newdata=test_data)
    opstr <- paste("ntree=", m$hyperpar[1], " treedepth=", m$hyperpar[2], " epsilon=", m$hyperpar[3], sep="")
  }

  #random intercept
 if (model.type == "randintercept"){
    #fits a single tree
    fitted_model <- fit_randintercept(equation,training_data)
    yhat <- predict(fitted_model,newdata=valid_data,type="response")
    yhattest <- predict(fitted_model, newdata=test_data, type = "response")
 }


  ############################ end model space #############################S

  if(is.null(err)){err <- compute_error(valid_data$nb_departure,yhat)}
  errtest <- compute_error(test_data$nb_departure,yhattest,model_name = model.name)
  err$opstr <- opstr
  err$testrmse <- errtest$rmse
  err$testmae <- errtest$mae

  return(err)
}

#Train models
train_models <- function(model_list,dataset_list){
    resultsdf <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(resultsdf) <- c("Method","Validation_RMSE", "Validation_MAE","Test_RMSE", "Test_MAE", "Parameters")

    for (mm in model_list){
      print(paste("Running model: ",mm[1], " ", format(Sys.time(), "%H:%M:%S")))
      tout <- fit_predict_err(mm[1],mm[2],mm[3],mm[4],dataset_list[[mm[5]]],dataset_list[[mm[6]]],dataset_list[[mm[7]]])
      resultsdf[nrow(resultsdf) + 1,] = c(mm[1],tout$rmse,tout$mae,tout$testrmse,tout$testmae,tout$opstr)
    }
    return(resultsdf)
}
