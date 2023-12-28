compute.rmse = function(statistic){ 
  
  round(sqrt(mean(statistic, na.rm = T)), 4)  
  
}