plot.fcast <- function(my.model){ 
  
  my.model %>% 
    na.omit() %>% 
    mutate(country = str_to_title(country)) %>% 
    ggplot(aes(x = date, y = f_synch)) +
    geom_line(col = "darkred") +
    facet_wrap(~country, scales = "free") + 
    theme_bw() + 
    xlab("Date") + 
    ylab("Forecasted synch.") 
  
  
}