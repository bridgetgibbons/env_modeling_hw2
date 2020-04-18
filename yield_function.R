# almond yield function (Kaili's version)

# 1. and 2.
## Implementing a model of almond yield anomaly (difference from average) in R based on Lobell et al. (2006)


almond_yield_take3 = function(climate, x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28){
  
  climate = clim_joined_YA
  
  for(i in 1:nrow(climate)){
    climate$yield_anomaly[i] = (x1*climate$meantmin[i]) + (x2*((climate$meantmin[i])^2)) + (x3*((climate$precip[i])^2)) + intercept
    
  }
  
  min_anomaly = min(climate$yield_anomaly)
  
  max_anomaly = max(climate$yield_anomaly)
  
  
  # make sure precipitation values are non-negative
  if (meantmin < 0)
    return(NA)
  
  # make sure anomaly values are non-negative???
  if (climate$year != class(integer))
    return(NA)
  
  
  return(list(annual_anomaly = climate[,c("year", "yield_anomaly")], max_anomaly, min_anomaly))
  
}



