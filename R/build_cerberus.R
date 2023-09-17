# Functions for building Cerberus model

##############################################################################
#' Determine appropriate neural network hidden layer size
#'
#'
#' @param len input size
#' @return integer hidden layer size
ksize <- function(len){
  return(max(2,round(len/9)))
}

##############################################################################
#' Form Single Cerberus Head
#'
#' This function forms a single head (either call, response, or context) for
#' Cerberus model
#' 
#'
#' @param layerinput keras layer input object
#' @param size integer layer input size
#' @param featurelen integer layer feature length
#' @param csize integer relative dimension of hidden layers
#' @return keras Cerberus head object
#' @export
form_head <- function(layerinput,size,featurelen,csize = 64){
  headout <- layerinput%>%
    keras::layer_conv_2d(filters=csize,kernel_size = c(ksize(size),ksize(featurelen)))%>%
    keras::layer_activation_leaky_relu()%>%
    keras::layer_max_pooling_2d(pool_size = c(2,2))%>%
    keras::layer_flatten()%>%
    keras::layer_dense(units=csize)%>%
    keras::layer_activation_leaky_relu()
  
  return(headout)
}

##############################################################################
#' Build Cerberus model
#'
#' This function takes the call, response, and context heads, and combines them
#' into a full Cerberus model.
#'
#' @param training_data list of formatted call, response, context data
#' @param csize integer relative dimension of hidden layers
#' @return keras Cerberus model
#' @export
build_cerberus <- function(training_data, csize = 64){
  train_call <- training_data$inputs$train_call
  train_cont <- training_data$inputs$train_cont
  train_res <- training_data$inputs$train_res
  y_train <- training_data$outputs
  
  # extract dimensions from provided data
  context_len <- length(train_cont)
  context_dims <- lapply(train_cont,dim)
  call_size <- dim(train_call)[2]
  call_fl <- dim(train_call)[3]
  res_size <- dim(train_res)[2]
  res_fl <- dim(train_res)[3]
  
  # build call head
  in_call = keras::layer_input(shape=c(call_size,call_fl,1))
  call_head = form_head(in_call,call_size,call_fl,csize)
  
  # include last known call
  last_known = in_call[ ,call_size, ,1]
  
  # build ontext(s) head
  in_conts=list()
  cont_heads=list()
  cont_index = 1
  for (icl in context_dims){
    in_conts[[cont_index]] = keras::layer_input(shape=c(icl[2],icl[3],1))
    cont_heads[[cont_index]] = form_head(in_conts[[cont_index]],icl[2],icl[3],csize)
    cont_index = cont_index+1
  }
  
  # build response head
  in_res = keras::layer_input(shape=c(res_size,res_fl,1))
  res_head = form_head(in_res,res_size,res_fl,csize)
  
  # combine heads with necks
  necks = keras::layer_reshape(keras::layer_concatenate(c(
                                                          list(call_head),
                                                          cont_heads,
                                                          list(res_head)),
                                                        axis=1),
                               target_shape=c(csize,context_len+2,1))%>%
    keras::layer_conv_2d(filters=csize,kernel_size = c(2,2))%>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_max_pooling_2d(pool_size = c(2,2))%>%
    keras::layer_flatten()
  
  # construct body
  body = keras::layer_concatenate(last_known,necks,axis=1) %>%
    keras::layer_dense(units=csize*4)%>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dense(units=csize)%>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dense(units=csize/2)%>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dense(units=dim(y_train)[2],activation="linear")
  
  model = keras::keras_model(c(list(in_call),in_conts,list(in_res)),body)
  
  model %>% keras::compile(loss=loss_mean_squared_error,optimizer=keras::optimizer_adam())
  
  return(model)
}

##############################################################################
#' Train Cerberus Model
#'
#' This function takes a constructed Cerberus model and trains it to provided 
#' timeseries data using ADAM.
#'
#' @param model keras Cerberus model
#' @param training_data list of formatted call, response, context data for training
#' @param epochs desired number of epochs integer
#' @return trained keras Cerberus model
#' @export
train_cerberus <- function(model,training_data,epochs){
  
  train_call <- training_data$inputs$train_call
  train_cont <- training_data$inputs$train_cont
  train_res <- training_data$inputs$train_res
  y_train <- training_data$outputs
  
  history <- model %>% fit(
    c(list(train_call),train_cont,list(train_res)),
    y_train,
    epochs=epochs
  )
  
  return(model)
}