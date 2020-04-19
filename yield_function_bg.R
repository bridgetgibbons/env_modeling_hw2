yield_function_bg = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28, climate){
  
  # organize the climate data by month and year
  # create summary of the mean max and min temperatures, and the mean precipitation by month
  
  clim_month = climate %>% 
    group_by(month, year) %>% 
    summarize(meantmax = mean(tmax_c), meantmin = mean(tmin_c), precip=mean(precip)) 
  
  # filter for months of interest for almonds
  # january - precipitation
  # february - minimum temperature
  
  jan_precip <- clim_month %>% 
    dplyr::filter(month == 1)
  
  feb_temp <- clim_month %>% 
    dplyr::filter(month == 2)
  
  # create a dataframe to store the yield anomaly values; use year values from one of the existing data frames 
  
  output_df <- data.frame(year = feb_temp$year, YA = NA)
  
  # use a for loop to have the function run for each row [i] using the columns of interest, and store the output in the YA column of the output data frame
  
  for(i in 1:nrow(output_df)){
    output_df$YA[i] <- x1*feb_temp$meantmin[i] + x2*feb_temp$meantmin[i]^2 + x3*jan_precip$precip[i] + x4*jan_precip$precip[i]^2 + intercept
  }
  
  # pull out the year and yield anomaly to include in the function output
  
  yield_anomaly_year <- dplyr::select(output_df, c(year, YA))
  
  # calculate the minimum and maximum anomaly values from the YA column, give a title so they can be called in the list
  
  min_value <- yield_anomaly_year[yield_anomaly_year$YA == min(yield_anomaly_year$YA),]
  max_value <- yield_anomaly_year[yield_anomaly_year$YA == max(yield_anomaly_year$YA),]
  
  # error checking
  
  # make sure precipitation values are non-negative
  if (precip < 0)
    return(NA)
  
  # make sure anomaly values are non-negative???
  if (clim$year != class(integer))
    return(NA)
  
  
  # set the function to return a list of the values of interest
  
  return(list(yield_anomaly_year, min_value, max_value))
  
}