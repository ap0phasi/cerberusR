# CerberusTS for R
> Multivariate Timeseries Forecasting using Multi-Headed Generative Convolutional Transformers

## Table of Contents
* [Installation](#install)
* [Background](#background)
* [Architecture](#architecture)
* [Usage](#usage)

## Installation
```
library(devtools)
install_github("ap0phasi/cerberusR")
```

## Background

CerberusTS is a neural network architecture and framework for performing multivariate timeseries forecasting in a generative manner. 
The name is inspired by the fact that this architecture uses call, response, and context(s) heads, inspired by the transformers 
prevalent in modern language models. In this approach, the "call" head is a window of timeseries data prior to some point of prediction,
the "response" head is a window of generated timeseries predictions, and the "context" heads are windows of timeseries data at different
resolutions to aid in the prediction. The windows are processed using 2D convolutional layers. The size and resolution of these windows 
are completely customizeable by the user, with this package offering some supporting pre- and post- processing functions to do so. 

## Architecture

![CerberusTS Architecture](https://github.com/ap0phasi/cerberusR/blob/main/img/cerberusTS_architecture.png?raw=true)

Cerberus processes the call, context, and response heads with 2D convolutional layers. As the model predicts a response for a single
point in time, the response head is masked in training, where for each prediction timestamp, the call window is repeated for the response 
size, with each having a response window populated up to prior the point in time of prediction. After training, given call and context heads
the response head starts with an empty matrix and a single response prediction is generated. This is added into the response head and the 
process is repeated until the response matrix is populated. 

As the convolutional layers extract patterns in the timeseries data, we also want to make sure the model sees the last known data points in
the call, so these values are passed to the dense portion of the model - the "body". 

## Usage
Please refer to the vignettes for usage and examples:

1. [Climate Data Forecasting]

[Climate Data Forecasting]: http://htmlpreview.github.io/?https://github.com/ap0phasi/cerberusR/blob/main/vignettes/climate_data_example.html
