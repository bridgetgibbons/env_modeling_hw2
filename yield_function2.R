# almond yield function (Kaili's version)

# 1. and 2.
## Implementing a model of almond yield anomaly (difference from average) in R based on Lobell et al. (2006)

  
  almond_yield_take3 = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28, climate){
    
    
    clim_joined$yield_anomaly = (x1*clim_joined$meantmin) + (x2*(clim_joined$meantmin^2)) + (x3*(clim_joined$precip^2)) + intercept
    
    min_anomaly = min(clim_joined$yield_anomaly)
    
    max_anomaly = max(clim_joined$yield_anomaly)
    
    
    # make sure precipitation values are non-negative
    if (meantmin < 0)
      return(NA)
    
    # make sure anomaly values are non-negative???
    if (yield_anomaly < 0)
      return(NA)
    
    
    return(list(annual_anomaly = clim_joined[,c("year", "yield_anomaly")], max = max_anomaly, min = min_anomaly))
    
  }

  # then we use this function and apply it to the clim_joined data frame?

  mapply(climate, almond_yield_take_2)
  

