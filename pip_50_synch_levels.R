
# Code to reproduce Table 1 in the paper #

# Clean working space and load necessary libraries #

rm(list = ls())

# Load the necessary functions, packages, and specifications

func.files <- base::list.files(path = "./", pattern = "^~", full.names = TRUE)

for (file in func.files) {
  
  base::source(file)
  
}


# Create variables for year because we need to include different
# year fixed effects at certain forecasting horizons (note: 2001 is the baseline)

synch_levels_data_pip = read_excel(file.path(data_path, "synch_levels.xlsx")) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date <= as.Date("2020-10-01")) %>% 
  add.draghi.synch() %>% 
  dummy_cols(select_columns = "year", remove_first_dummy = T) 
  

year_dummy_names = c("year_2002","year_2003","year_2004","year_2005","year_2006","year_2007","year_2008",
                     "year_2009","year_2010","year_2011","year_2012","year_2013","year_2014","year_2015",
                     "year_2016","year_2017","year_2018","year_2019", "year_2020")



####################################################################################################
#                                           AR SYNCH LEVELS (PIP>50%)                              #
####################################################################################################


# Subset the data to forecast and complete with NA for some countries are missing some dates
# Keep the observation for 2018Q4 because we will need it to forecast
# We have 9 observations per country: 8 to forecast + 1 the initial one 2018Q4

synch_levels_pip_forecast_data = synch_levels_data_pip %>% 
  filter(date >= as.Date("2018-10-01")) 

synch_levels_pip_forecast_data = complete(synch_levels_pip_forecast_data, country, date) %>% 
  mutate(f_synch = NA)

# Create all the AR models #
# We need in total 8 models (we use up until the last_obs - 1 to forecast)

ar.synch.levels.pip50.1 = lm(synch ~ country  + draghisynch_lag1+ draghisynch_lag2+ recsynch_lag1 +
                               recsynch_lag2 +synch_lag1+ zlbsynch_lag1+ zlbsynch_lag2+ zlbsynch_lag3+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018, data = synch_levels_data_pip %>% 
                   filter(date <= as.Date("2018-10-01"))) 

ar.synch.levels.pip50.2 = lm(synch ~  country  + draghiinflation_lag3+ draghiinflation_lag4+ draghisynch_lag1+
                               draghisynch_lag2 +draghisynch_lag3+ recsynch_lag1 +synch_lag1 +zlbsynch_lag1+
                               zlbsynch_lag2+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2019-01-01"))) 

ar.synch.levels.pip50.3 = lm(synch ~ country + draghisynch_lag1 +draghisynch_lag2+ recsynch_lag1 +recsynch_lag2+
                               synch_lag1+ zlbsynch_lag1 +zlbsynch_lag2 +zlbsynch_lag3+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2019-04-01")))

ar.synch.levels.pip50.4 = lm(synch ~  country  + draghisynch_lag1+ draghisynch_lag2 +draghisynch_lag3+ recsynch_lag1 +
                               synch_lag1 +zlbsynch_lag1 +zlbsynch_lag2+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2019-07-01")))

ar.synch.levels.pip50.5 = lm(synch ~ country +draghisynch_lag1 +draghisynch_lag2 +draghisynch_lag3+ recsynch_lag1+
                               recsynch_lag2 +synch_lag1 +zlbsynch_lag1+ zlbsynch_lag2+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                               year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2019-10-01")))

ar.synch.levels.pip50.6 = lm(synch ~ country + draghisynch_lag1+ draghisynch_lag2+ draghisynch_lag3+ recsynch_lag1+
                               recsynch_lag2 +synch_lag1 +zlbsynch_lag1+ zlbsynch_lag2+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019+year_2020, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2020-01-01")))

ar.synch.levels.pip50.7 = lm(synch ~ country +draghisynch_lag1+ draghisynch_lag2+ draghisynch_lag3+ recsynch_lag1+
                               recsynch_lag2+ synch_lag1+ zlbsynch_lag1+ zlbsynch_lag2+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019+year_2020, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2020-04-01"))) 

ar.synch.levels.pip50.8 = lm(synch ~ country + draghisynch_lag1+ draghisynch_lag2+ draghisynch_lag3+ recsynch_lag1+
                               recsynch_lag2+ synch_lag1 +zlbsynch_lag1 +zlbsynch_lag2+year_2002+ year_2003+ year_2004+ year_2005+ year_2006+ year_2007+
                   year_2008+ year_2009+ year_2010+ year_2011+ year_2012+ year_2013+ year_2014+ year_2015+ year_2016+ year_2017 +year_2018+year_2019+year_2020, data = synch_levels_data_pip %>% 
                   mutate(date = as.Date(date)) %>% 
                   filter(date <= as.Date("2020-07-01")))



# Now, create the forecasts #
# Note that the first observation is the last observation used in the models
# Hence, it is the first one used to build the forecast


f_synch_index = which( colnames(synch_levels_pip_forecast_data)=="f_synch" )


for (mm in 1:dim(synch_levels_pip_forecast_data)[1]) {
  
  if (mm %in% seq(2,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.1, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2",
                                                                                     "recsynch_lag1", "recsynch_lag2", "synch_lag1",
                                                                                     "zlbsynch_lag1", "zlbsynch_lag2", "zlbsynch_lag3", "country", year_dummy_names[1:17])])
    
  }
  
  if (mm %in% seq(3,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.2, newdata = synch_levels_pip_forecast_data[mm,c("draghiinflation_lag3", "draghiinflation_lag4", "draghisynch_lag1",
                                                                                     "draghisynch_lag2", "draghisynch_lag3", "recsynch_lag1", "synch_lag1",
                                                                                     "zlbsynch_lag1", "zlbsynch_lag2", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(4,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.3, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2", "recsynch_lag1",
                                                                                     "recsynch_lag2", "synch_lag1", "zlbsynch_lag1", "zlbsynch_lag2",
                                                                                     "zlbsynch_lag3", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(5,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.4, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2", "draghisynch_lag3",
                                                                                     "recsynch_lag1", "synch_lag1", "zlbsynch_lag1", "zlbsynch_lag2", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(6,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.5, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2", "draghisynch_lag3", "recsynch_lag1",
                                                                                     "recsynch_lag2", "synch_lag1", "zlbsynch_lag1", "zlbsynch_lag2", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(7,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.6, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2", "draghisynch_lag3", "recsynch_lag1",
                                                                                     "recsynch_lag2", "synch_lag1", "zlbsynch_lag1", "zlbsynch_lag2", "country", year_dummy_names)])
  }
  
  if (mm %in% seq(8,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.7, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2", "draghisynch_lag3",
                                                                                     "recsynch_lag1", "recsynch_lag2", "synch_lag1", "zlbsynch_lag1",
                                                                                     "zlbsynch_lag2", "country", year_dummy_names)])
  }
  
  if (mm %in% seq(9,135,9)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.8, newdata = synch_levels_pip_forecast_data[mm,c("draghisynch_lag1", "draghisynch_lag2", "draghisynch_lag3",
                                                                                     "recsynch_lag1", "recsynch_lag2", "synch_lag1", "zlbsynch_lag1",
                                                                                     "zlbsynch_lag2", "country", year_dummy_names)])
    
  }
  
  
  
}

# Create variable for directional accuracy

synch_levels_pip_forecast_data = synch_levels_pip_forecast_data %>% 
  group_by(country) %>% 
  mutate(synch_diff = c(NA, diff(synch)), 
         f_synch_diff = c(NA, diff(f_synch)), 
         synch_direction = ifelse(synch_diff > 0, "up", "down"), 
         f_synch_direction = ifelse(f_synch_diff > 0, "up", "down")) %>% 
  select(-synch_diff, -f_synch_diff)



# Create squared difference between realization and forecast #

synch_levels_pip_forecast_data = synch_levels_pip_forecast_data %>% 
  mutate(res_sq = (synch-f_synch)^2)

# Do the same for PIIGS countries

synch_levels_pip_forecast_data.piigs = synch_levels_pip_forecast_data %>% 
  filter(country %in% piigs_subset)

# Do the same for non-PIIGS countries

synch_levels_pip_forecast_data.nopiigs = synch_levels_pip_forecast_data %>% 
  filter(!country %in% piigs_subset)

# Print the forecast results for all the sample (part of Table 3 in the paper)

cat(dir.results(synch_levels_pip_forecast_data), "\n")
cat(dir.results(synch_levels_pip_forecast_data.piigs), "\n")
cat(dir.results(synch_levels_pip_forecast_data.nopiigs), "\n")

# Print the forecast results without 2020 (part of Table A.5 in the paper)

cat(dir.results(without.2020(synch_levels_pip_forecast_data)), "\n")
cat(dir.results(without.2020(synch_levels_pip_forecast_data.piigs)), "\n")
cat(dir.results(without.2020(synch_levels_pip_forecast_data.nopiigs)), "\n")