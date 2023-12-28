get.cds = function(my_country){ 
  
  read_excel(cds.file, sheet = my_country) %>% 
    filter(!date=="45087", !is.na(date)) %>% 
    mutate(date = as.Date(date, format = "%m/%d/%Y"), 
           cds = as.numeric(cds),
           year = year(date), 
           quarter = quarter(date)) %>% 
    group_by(year,quarter) %>% 
    summarise(cds = mean(cds)) %>% 
    ungroup() %>% 
    mutate(date = as.Date(as.yearqtr(paste0(year,"q",quarter))), 
           country = my_country, 
           cdsgr = 100*log(cds/lag(cds,n=1)), 
           cds_lag1 = lag(cdsgr, n = 1), 
           cds_lag2 = lag(cdsgr, n = 2), 
           cds_lag3 = lag(cdsgr, n = 3), 
           cds_lag4 = lag(cdsgr, n = 4)) %>% 
    select(-year,-quarter,-cds) %>% 
    arrange(date,country,cds_lag1,cds_lag2,cds_lag3,cds_lag4) %>% 
    na.omit()
  
}