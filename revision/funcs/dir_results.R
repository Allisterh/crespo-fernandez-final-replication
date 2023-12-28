dir.results = function(my.model){ 
  
  rmse = compute.rmse(my.model$res_sq)
  
  theil = my_theil(my.model$synch, my.model$f_synch)
  
  current.table = table(my.model$f_synch_direction, my.model$synch_direction)
  
  da = round(sum(diag(current.table)) / sum(current.table), 4)
  
  hr = current.table[4]/(current.table[4]+current.table[3])
  
  fa = current.table[2]/(current.table[2]+current.table[1])
  
  ks =  hr - fa
  
  paste0(rmse, " ", theil, " ", round(da,4), " ", round(hr,4), " ", round(fa,4), " ", round(ks,4))
  
  
}