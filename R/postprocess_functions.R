# Functions for Post-Processing Cerberus Results

##############################################################################
#' Create Cerberus Inputs for Generative Predictions
#'
#' This function creates the inputs required for predictive generation by a 
#' trained Cerberus model
#'
#' @param responses generated Cerberus responses
#' @param timestamp POSIXct timestamp from which to generate predictions
#' @param resampled_time_series prepared Ceberus resampled timeseries data list
#' @param features character array. Timeseries features used in Cerberus
#' @param config Cerberus configuration list.
#' @return generated Cerberus heads
generate_in <-function(responses,timestamp,resampled_time_series,features,config){
  #Pull in training data for reference
  call_size = config$call_size
  call_features = config$call_features
  resampled_time_series = resampled_time_series
  context_size = config$context_size
  context_feature_indx = config$context_feature_indx
  call_feature_indx = config$call_feature_indx
  res_feature_indx = config$res_feature_indx
  context_features <- features[context_feature_indx]
  call_features <- features[call_feature_indx]
  res_features <- features[res_feature_indx]
  context_len <- length(context_size)
  res_size <- config$res_size
  
  res_head_base <- responses
  
  call_head <- assign_call(resampled_time_series$call_resamp,timestamp,call_size,call_features)
  context_heads = list()
  for (icontext in 1:length(context_size)){
    context_heads[[paste0("context_",icontext)]]<-  assign_call(resampled_time_series[[sprintf("context_%s_resamp",icontext)]],
                                                                timestamp,
                                                                context_size[icontext],
                                                                context_features)
  }
  test_in <- list(c(list(call = call_head),
                    list(res = res_head_base),
                    context_heads))
  test_call=abind(lapply(test_in,function(x)array_reshape(x$call,c(1,call_size,length(call_features),1))),along = 1)
  test_res=abind(lapply(test_in,function(x)array_reshape(x$res,c(1,res_size,length(res_features),1))),along = 1)
  test_cont=list()
  for (icl in 1:context_len){
    test_cont[[paste0("context",icl)]]=abind(lapply(test_in,function(x)array_reshape(x[[paste0("context_",icl)]],c(1,context_size[icl],length(context_features),1))),along = 1)
  }
  names(test_cont)=NULL
  return(c(list(test_call),test_cont,list(test_res)))
}

##############################################################################
#' Generate Response Predictions
#'
#' This function uses a trained cerberus model to generatively predict responses
#'
#' @param model trained Cerberus keras model object
#' @param timestamp POSIXct timestamp from which to generate predictions
#' @param resampled_time_series prepared Ceberus resampled timeseries data list
#' @param features character array. Timeseries features used in Cerberus
#' @param config Cerberus configuration list.
#' @return generated response predictions
#' @export
generate_prediction <- function(model,timestamp,resampled_time_series,features,config){
  responses = matrix(0,nrow=config$res_size,ncol=length(config$res_feature_indx))
  
  for (icount in 1:config$res_size){
    genres=(model %>% predict(generate_in(responses,timestamp,resampled_time_series,features,config),verbose = F))
    responses[icount,]=genres
  }
  
  return(responses)
}

##############################################################################
#' Descale Generated Response
#'
#' This function returns a generated response from Cerberus to the original
#' range of the timeseries data. 
#'
#' @param responses generated response predictions
#' @param min_max_df dataframe of min and max by feature
#' @param config Cerberus configuration list.
#' @param features character array. Timeseries features used in Cerberus
#' @param timestamp POSIXct timestamp from which to generate predictions
#' @return timeseries data frame of descaled generated response
#' @export
descale_response <- function(responses,min_max_df,config,features,timestamp){
  descaled_df = data.frame(timestamp = seq.POSIXt(from = timestamp, 
                                                  by = paste(config$res_ts,config$ts_units), 
                                                  length.out = config$res_size))
  responses = data.frame(responses)
  names(responses)=features[config$res_feature_indx]
  for (ifeat in names(responses)){
    minmaxrange = min_max_df[ifeat,-1]
    descaled_df[[ifeat]]=(responses[[ifeat]]-config$scale_range[1])/(config$scale_range[2]-config$scale_range[1])*(max(minmaxrange)-min(minmaxrange))+min(minmaxrange)
  }
  
  return(descaled_df)
}