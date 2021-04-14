library(RSiteCatalyst)
library(tidyverse)

#### Start of function ####

getAnomaly <- function(type = '', datefrom = (Sys.Date() - 366), dateto = (Sys.Date() - 1),  m = 'pageviews', rs = ''){
  
  master_df <- data.frame(stringsAsFactors = FALSE)
  
  if(type == "metrics"){
    
    # get list of metrics
    events <- GetSuccessEvents(rs)
    events <- events[!(grepl('disabled', events$type)),]
    parameter <- events$id
    
    # for metrics & success metrics
    for (i in 1:length(parameter)){
      
      param_name <- parameter[i]
      
      try(
        {
          df <- try(QueueTrended(rs,
                                 datefrom,
                                 dateto,
                                 metrics = param_name,
                                 elements = c(),
                                 anomaly.detection = TRUE
          ))
          
          df <- df[,c(1,8,9,10,11)]
          colnames(df) <- c("date", "value", "forecast", "upper", "lower")
          df$metric <- param_name
          
          master_df <- rbind(master_df, df)
          print(sprintf("%s%% completed", format(round(i/length(parameter)*100, 2), nsmall = 2)))
        }
      )
    }
  } else if(type == "calculated metrics"){
    
    # get list of calculated metrics
    calc_metrics <- GetCalculatedMetrics(rs)
    calc_metrics <- calc_metrics$formula_id
    parameter <- calc_metrics
    
    for (i in 1:length(parameter)){
      
      param_name <- parameter[i]
      
      try(
        {
          df <- QueueTrended(rs,
                             datefrom,
                             dateto,
                             metrics = param_name,
                             elements = c(),
                             anomaly.detection = TRUE)
          
          df <- df[,c(1,8,9,10,11)]
          colnames(df) <- c("date", "value", "forecast", "upper", "lower")
          df$metric <- param_name
          
          master_df <- rbind(master_df, df)
          print(sprintf("%s%% completed", format(round(i/length(parameter)*100, 2), nsmall = 2)))
        }
      )
    }
    
  } else if (type == "segments"){
    
    # get segments
    segments <- GetSegments(rs)
    segments <- segments$id
    parameter <- segments
    
    for (i in 1:length(parameter)){
      
      param_name <- parameter[i]
      
      try(
        {
          df <- QueueTrended(rs,
                             datefrom,
                             dateto,
                             metrics = m,
                             elements = c(),
                             segment.id = param_name,
                             anomaly.detection = TRUE)
          
          df <- df[,c(1,7,8,9,10,11)]
          colnames(df) <- c("date", "name", "value", "forecast", "upper", "lower")
          df$metric <- param_name
          
          master_df <- rbind(master_df, df)
          print(sprintf("%s%% completed", format(round(i/length(parameter)*100, 2), nsmall = 2)))
        }
      )
    }
    
  } else if (type == "elements"){
    
    # get elements
    elements <- GetElements(rs)
    elements <- elements$id
    parameter <- elements
    
    for (i in 1:length(parameter)){
      
      param_name <- parameter[i]
      
      try(
        {
          df <- QueueTrended(rs,
                             datefrom,
                             dateto,
                             metrics = m,
                             elements = param_name,
                             anomaly.detection = TRUE)
          
          df <- df[,c(1,2,4,5,6,7)]
          colnames(df) <- c("date", "name", "value", "forecast", "upper", "lower")
          df$metric <- param_name
          
          master_df <- rbind(master_df, df)
          print(sprintf("%s%% completed", format(round(i/length(parameter)*100, 2), nsmall = 2)))
        }
      )
    }
  } else {print("please either select metrics, 'calculated metrics', 'segments', or 'elements'")}
  
  master_df$anomaly <- ifelse(master_df$value > master_df$upper | master_df$value < master_df$lower,
                              'TRUE',
                              'FALSE')
  
  master_df$anomaly_direction <- ifelse(master_df$anomaly == 'TRUE' & master_df$value > master_df$upper,
                                        'UPPER',
                                        ifelse(master_df$anomaly == 'TRUE' & master_df$value < master_df$lower,
                                               'LOWER',
                                               '')
  )
  
  master_df$anomaly_size <- ifelse(master_df$anomaly == 'TRUE' & master_df$value > master_df$upper,
                                   format(round((master_df$value/master_df$upper-1),4),nsmall=4),
                                   ifelse(master_df$anomaly == 'TRUE' & master_df$value < master_df$lower,
                                          format(round((master_df$value/master_df$lower-1),4),nsmall=4),
                                          '')
  )
  
  return(master_df)
  
}

#### end of function ####

# Test Run
get_metrics <- getAnomaly("metrics")
