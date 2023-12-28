without.2020 = function(model_object){ 
  
  model_object %>% 
    filter(!date == "2018-10-01", date < "2020-01-01")
  
}