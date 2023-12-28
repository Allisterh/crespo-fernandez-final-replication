my.ttest = function(mycountry, myvar){ 
  
  
  
  t.test(synch.data %>% 
           filter(country == mycountry, {{myvar}} == 1) %>% 
           pull(synch), 
         synch.data %>% 
           filter(country == mycountry, {{myvar}} == 0) %>% 
           pull(synch)
  )
  
  
  
  
}