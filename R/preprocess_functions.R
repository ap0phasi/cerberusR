# Functions for preprocessing timeseries data for Cerberus model

##############################################################################
#' Determine Feature Min and Max
#'
#' This function calculates the minimum and maximum of each feature in a
#' timeseries data set
#'
#' @param data timeseries dataframe
#' @return dataframe of min and max by feature
#' @export
calculate_min_max <- function(data) {
  min_max_df <- data.frame(
    feature = names(data)[-1],
    min_value = apply(data[, -1], 2, min),
    max_value = apply(data[, -1], 2, max)
  )
  return(min_max_df)
}

##############################################################################
#' Scale Features to Range
#'
#' This function scales timeseries data to a user specified range provided the
#' min and max of each feature
#'
#' @param data timeseries dataframe
#' @param min_max_df dataframe of min and max by feature
#' @param min_range minimum desired value of scaled data
#' @param max_range maximum desired value of scaled data
#' @return scaled timeseries dataframe
#' @export
scale_features <- function(data, min_max_df, min_range = -1, max_range = 1) {
  scaled_data <- data
  for (feature in min_max_df$feature) {
    min_value <- min_max_df$min_value[min_max_df$feature == feature]
    max_value <- min_max_df$max_value[min_max_df$feature == feature]
    scaled_data[, feature] <- min_range + ((data[, feature] - min_value) * (max_range - min_range)) / (max_value - min_value)
  }
  return(scaled_data)
}

##############################################################################
#' Resample Timeseries to New Timestep
#'
#' This function resamples timeseries data by averaging over a window size
#'
#' @param data timeseries dataframe
#' @param window_size integer for window size to average timeseries over
#' @return resampled timeseries dataframe
#' @export
resample_timeseries <- function(data, window_size) {
  data %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp)) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::group_by(group = ceiling(row_number() / window_size)) %>%
    dplyr::summarise(timestamp = dplyr::last(timestamp), dplyr::across(-group,  ~mean(.)))
}

##############################################################################
#' Allocate Data to Head
#'
#' This function populates a matrix with the selected data at a point in time
#'
#' @param data timeseries dataframe
#' @param timestamp POSIXct time from which to populate matrix
#' @param desired_size integer for size of matrix to populate
#' @param features timeseries features for assignment
#' @return matrix populated with data
assign_call <- function(data,timestamp,desired_size,features){
  in_head <- matrix(0,nrow = desired_size,ncol = length(features))
  end_points <- which(data$timestamp<timestamp)
  if (length(end_points)>0){
    assign_index = (max(end_points)-desired_size+1):max(end_points)
    #Sometimes this will cause assign_index to drop below 1. We want to not include those values
    in_head[(1:desired_size)[assign_index>0],]=as.matrix(data[assign_index[assign_index>0],features])
    return(in_head)
  }else{
    return(in_head)
  }
}

##############################################################################
#' Cerberus Config Presets
#'
#' This function provides some presets for Cerberus configuration.
#' The _indx arguments expect integer arrays for the column indices in the 
#' timeseries data.
#'
#' @param call_feature_index integer array of column indices for call features
#' @param res_feature_index integer array of column indices for response features
#' @param context_feature_index integer array of column indices for context features
#' @param ts_units character string for unit of timeseries, referenced by size
#' @param call_ts integer. Resolution timesteps of ts_units for call head
#' @param call_size integer. Number of timesteps for call head
#' @param res_ts integer. Resolution timesteps of ts_units for response head
#' @param res_size integer. Number of timesteps for response head
#' @param context_ts integer array. Resolution timesteps of ts_units for context heads
#' @param context_size integer array. Number of timesteps for context heads
#' @return config list
#' @export
cerberus_config <- function(
    call_feature_indx,
    res_feature_indx,
    context_feature_indx,
    ts_units = "hours",
    call_ts = 2,
    call_size = 48,
    res_ts = 2,
    res_size = 12,
    context_ts = c(6),
    context_size = c(12),
    scale_range = c(0,1)
){
  return(
    list(
      "call_feature_indx" = call_feature_indx,
      "res_feature_indx" = res_feature_indx,
      "context_feature_indx" = context_feature_indx,
      "ts_units" = ts_units,
      "call_ts" = call_ts,
      "call_size" = call_size,
      "res_ts" = res_ts,
      "res_size" = res_size,
      "context_ts" = context_ts,
      "context_size" = context_size,
      "scale_range" = scale_range
    )
  )
}

##############################################################################
#' Prepare Input Data for Cerberus
#'
#' This function prepares a timeseries dataframe into the array structures
#' required for cerberus
#'
#' @param time_series_data timeseries dataframe
#' @param config cerberus configuration list
#' @param min_max_df 
#' @param max_range dataframe of min and max by feature
#' @return list of prepared data for Cerberus
#' @export
prepare_inputs <- function(time_series_data,config = cerberus_config(),min_max_df = NULL){
  # Load Configurations
  call_feature_indx = config$call_feature_indx
  res_feature_indx = config$res_feature_indx
  context_feature_indx = config$context_feature_indx
  ts_units = config$ts_units
  call_ts = config$call_ts
  call_size = config$call_size
  res_ts = config$res_ts
  res_size = config$res_size
  context_ts = config$context_ts
  context_size = config$context_size
  
  # Calculate dt and window sizes of time_series_data
  timestep = diff(time_series_data[[1]][1:2])
  units(timestep)=ts_units
  timestep = as.numeric(timestep)
  call_resamp = call_ts/timestep
  res_resamp = res_ts/timestep
  context_resamp = context_ts/timestep
  
  #min max calculation if not provided
  if (is.null(min_max_df)){
    min_max_df <- calculate_min_max(time_series_data)
  }
  scaled_data <- scale_features(time_series_data,min_max_df,config$scale_range[1],config$scale_range[2])
  
  # feature assignment
  features = names(scaled_data)
  call_features <- features[call_feature_indx]
  res_features <- features[res_feature_indx]
  context_features <- features[context_feature_indx]
  
  # timeseries resampling
  resampled_time_series <- list()
  resampled_time_series[[paste("call_resamp")]]<-resample_timeseries(scaled_data,call_resamp)
  resampled_time_series[[paste("res_resamp")]]<-resample_timeseries(scaled_data,res_resamp)
  for (icontext in 1:length(context_resamp)){
    resampled_time_series[[sprintf("context_%s_resamp",icontext)]]<-resample_timeseries(scaled_data,context_resamp[icontext])
  }
  
  #Response events:
  skip_size = 1
  event_size = res_size
  start_index = seq(1,(dim(resampled_time_series$res_resamp)[1]-event_size),skip_size)
  end_index = seq(event_size,(dim(resampled_time_series$res_resamp)[1]),skip_size)
  end_index = end_index[1:length(start_index)]
  
  training_dat <- list()
  for (ires in 1:length(start_index)){
    train_in <- list()
    #Find timestamp that corresponds with the start_index
    timestamp <- resampled_time_series$res_resamp$timestamp[ires]
    res_head_base <- as.matrix(resampled_time_series$res_resamp[start_index[ires]:end_index[ires],res_features])
    
    call_head <- assign_call(resampled_time_series$call_resamp,timestamp,call_size,call_features)
    context_heads = list()
    for (icontext in 1:length(context_size)){
      context_heads[[paste0("context_",icontext)]]<-  assign_call(resampled_time_series[[sprintf("context_%s_resamp",icontext)]],
                                                                  timestamp,
                                                                  context_size[icontext],
                                                                  context_features)
    }
    
    for (iresponse in 1:res_size){
      #Apply masking
      res_head <- res_head_base
      res_head[iresponse:res_size,]=0
      train_in <- c(list(call = call_head),
                    list(res = res_head),
                    context_heads,
                    list(answer = res_head_base[iresponse,]))
      training_dat = c(training_dat,list(train_in))
    }
  }
  x_call=abind(lapply(training_dat,function(x)array_reshape(x$call,c(1,call_size,length(call_features),1))),along = 1)
  x_res=abind(lapply(training_dat,function(x)array_reshape(x$res,c(1,res_size,length(res_features),1))),along = 1)
  x_cont=list()
  context_indx = 1
  for (icl in context_size){
    x_cont[[paste0("context",context_indx)]]=abind(lapply(training_dat,function(x)array_reshape(x[[paste0("context_",context_indx)]],c(1,icl,length(context_features),1))),along = 1)
    context_indx = context_indx + 1
  }
  names(x_cont)=NULL
  y_outputs = do.call(rbind,lapply(training_dat,function(x)x$answer))
  
  formatted_all <-list(inputs=list("x_call"=x_call,
                                   "x_cont"=x_cont,
                                   "x_res"=x_res),
                       outputs = y_outputs)
  
  return(list("formatted_all"=formatted_all,
              "min_max_df"=min_max_df,
              "resampled_time_series"=resampled_time_series,
              "scaled_data"=scaled_data))
}

##############################################################################
#' Retrieve Training Data Samples
#'
#' This function retrieves training data from our prepared Cerberus data list
#' based on the desired proportion
#'
#' @param formatted_all list of prepared Cerberus arrays.
#' @param ratio proportion of data to be used for training.
#' @return list of training Cerberus arrays.
#' @export
grab_train_samples <- function(formatted_all,ratio = 0.7){
  N <- round(dim(formatted_all$inputs[[1]])[1]*ratio)
  train_sel <- 1:N
  test_sel <- (N+1):(dim(formatted_all$inputs[[1]])[1])
  
  y_train <- formatted_all$outputs[train_sel,]
  train_call <- formatted_all$inputs$x_call[train_sel,,,]
  train_res <- formatted_all$inputs$x_res[train_sel,,,]
  train_cont <- lapply(formatted_all$inputs$x_cont,function(x) x[train_sel,,,])
  
  training_all <- list(inputs=list("train_call"=train_call,
                                   "train_cont"=train_cont,
                                   "train_res"=train_res),
                       outputs = y_train)
  
  return(training_all)
}

