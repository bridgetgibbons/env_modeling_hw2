# almond yield function

almond_yield = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28, min_monthly_temp, precipitation){
  
  yield_anomaly = (x1*min_monthly_temp) + (x2*(min_monthly_temp^2)) + (x3*(precipitation^2)) + intercept
  
  return(yield_anomaly)
  
}


##############


almond_yield_take2 = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, interceptt = 0.28, climate){
  
  # sort for the temperature and precipitation data based on month
  
  if(month="1"){
    climate$jan_precip = climate$precip
  }
  if(month="2"){
    climate$feb_mintemp = climate$meantmin
  }
  
  climate$yield_anomaly = (x1*climate$feb_mintemp) + (x2*(climate$feb_mintemp^2)) + (x3*(climate$jan_precip^2)) + intercept
  
  min_anomaly = min(climate$yield_anomaly)
  
  max_anomaly = max(climate$yield_anomaly)
  
  return(list(annual_anomaly = climate[,c("year", "yield_anomaly")], max = max_anomaly, min = min_anomaly))
  
}