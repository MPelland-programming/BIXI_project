first_element <- function(ll){
  return(ll[1])
}

preprocessing <- function(rawdata,remove.na = TRUE){
  # Replace long station names solely by their numbers, provides a dictionary to go from one to the other. 
  # Finds minimum for 
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

compute_error <- function(true_y,predicted_y, errtype = "RMSE"){
  #Specifcy what type of error to use, choices are:
  # RMSE, 
  nn <- length(true_y)
  err <- {}
  if (errtype == "RMSE"|"all"){
    err$rmse <- sqrt(sum((aa-bb)^2)/nn)
  }

  if (errtype == "MSE"|"all"){
    err$mse <- sum((aa-bb)^2)/nn
  }

  if (errtype == "MAE"|"all"){
    err$mae <- sum(abs(aa-bb))/nn
  }
  
  return(err)
}

