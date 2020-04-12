# almond yield function

almond_yield = function(x1 = -0.015, x2 = -0.0046, x3 = -0.07, x4 = 0.0043, intercept = 0.28, min_monthly_temp, precipitation){
  
  yield_anomaly = (x1*min_monthly_temp) + (x2*(min_monthly_temp^2)) + (x3*(precipitation^2)) + intercept
  
  return(yield_anomaly)
  
}