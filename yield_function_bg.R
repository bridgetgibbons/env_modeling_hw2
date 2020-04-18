yield_function_bg = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28, climate){
  
  clim_month = climate %>% 
    group_by(month, year) %>% 
    summarize(meantmax = mean(tmax_c), meantmin = mean(tmin_c), precip=mean(precip)) 
  
  jan_precip <- clim_month %>% 
    dplyr::filter(month == 1)
  
  feb_temp <- clim_month %>% 
    dplyr::filter(month == 2)
  
  output_df <- as.data.frame(feb_temp) %>% mutate(YA = NA)
  
  for(i in 1:nrow(output_df)){
    output_df$YA[i] <- x1*feb_temp$meantmin[i] + x2*feb_temp$meantmin[i]^2 + x3*jan_precip$precip[i] + x4*jan_precip$precip[i]^2 + intercept
  }
  
  yield_anomaly_year <- dplyr::select(output_df, c(year, YA))
  
  min_value <- yield_anomaly_year[yield_anomaly_year$YA == min(yield_anomaly_year$YA),]
  max_value <- yield_anomaly_year[yield_anomaly_year$YA == max(yield_anomaly_year$YA),]
  
  return(list(yield_anomaly_year, min_value, max_value))
  
}