my_theil = function(realized, predicted){ 
  
  round(TheilU(realized, predicted, na.rm = T, type = 2),4)
  
}