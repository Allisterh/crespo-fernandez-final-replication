plot.acf <- function(my_country, model_resids){ 
  
  data.residuals %>% 
    filter(country == my_country) %>% 
    pull(model_resids) %>% 
    acf(main = str_to_title(my_country))
  
}