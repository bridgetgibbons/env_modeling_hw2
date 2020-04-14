# almond yield function (Kaili's version)

# 1.
## Implementing a model of almond yield anomaly (difference from average) in R based on Lobell et al. (2006)

almond_yield = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28, min_monthly_temp, precipitation){
  
  yield_anomaly = (x1*min_monthly_temp) + (x2*(min_monthly_temp^2)) + (x3*(precipitation^2)) + intercept
  
  return(yield_anomaly)
  
  # error checking for when min_monthly_temp or precipitation are negative, or when anomaly value is negative?
  
  # we need Year to be an input, and have it draw from two separate data frames: year-mintemp and year-precip where only those values (month 1 or 2) are available next to each year name
  min_monthly_temp = clim_mintemp$meantmin
  
  precipitation = clim_precip$precip
  
  
  # then an sapply() here
  
}


# 2. 
## Having the function return almond yield anomaly for each year, and max and minimum yields over a time series of multiple year inputs.


