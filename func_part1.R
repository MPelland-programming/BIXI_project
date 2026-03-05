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
  # Removes numb of universities
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
  
  #Return only relevant columns 
  preproc_data$data <- preproc_data$data[-c(1,2,11,18)]
  
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

get_heatmap <- function(preproc){
  heatmap(abs(cor(preproc$data[,3:21])))
  heatmap(abs(cor(preproc$data[,3:21])),
         col = my_colors,
         breaks = seq(0.5, 1, length.out = 6),
         scale = "none")
}

compute_error <- function(true_y,predicted_y, errtype = "all"){
  #Specifcy what type of error to use, choices are:
  # RMSE, 
  nn <- length(true_y)
  err <- {}
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

fit_predict_err <-  function(model.type,equation,opts,training_data,valid_data,min_zero = TRUE){
  
  if (model.type == "lm" | model.type == "glm"){
    fitted_model <- fit_model_equation(model.type,equation,opts,training_data)
    yhat <- predict(fitted_model,new = valid_data)
  }
  
  if (model.type == "cv.glmnet"){
    formu <- as.formula(equation)
    xs <- model.matrix(formu,training_data)[,-1]
    xvalid <- model.matrix(formu,valid_data)[,-1]
    ys <- training_data$nb_departure
    
    fitted_model <- fit_model_noeq(model.type,xs,ys,opts)
    
    yhat <- predict(fitted_model,new=xvalid, s = "lambda.1se")
  }
  
  
  if (min(yhat)<0 & min_zero == FALSE){
    print(paste("model", model.type, "predicted negative values down to :",as.character(min(yhat))))
  }

  if (min_zero == TRUE){
    yhat[yhat<0] <- 0
  }
  
  err <- compute_error(valid_data$nb_departure,yhat)
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

