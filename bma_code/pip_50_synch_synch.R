
# Code to reproduce Table 1 in the paper #

# Clean working space and load necessary libraries #

rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(BMS)
library(Cairo)
library(reshape2)
library(ggplot2)
library(writexl)
library(lubridate)

# Set seed and number of iterations + burnin phase

set.seed(14091998)
n.iter = 500000
n.burn = 10000

# Read the dataset, set WD #

data_path = "../bma_data"


# Create PIIGS subset # 

piigs_subset = c("portugal", "ireland", "italy", "greece", "spain")

# Create variables for year because we need to include different
# year fixed effects at certain forecasting horizons (note: 2001 is the baseline)

synch_synch_data_pip = read_excel(file.path(data_path, "synch_synch.xlsx")) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date <= as.Date("2020-10-01")) %>% 
  mutate(draghi = ifelse(date >= '2012-07-01', 1, 0), 
         draghisynch_lag1 = draghi * synch_lag1, 
         draghisynch_lag2 = draghi * synch_lag2, 
         draghisynch_lag3 = draghi * synch_lag3, 
         draghisynch_lag4 = draghi * synch_lag4, 
         draghiuncert_lag1 = draghi * uncert_lag1,
         draghiuncert_lag2 = draghi * uncert_lag2,
         draghiuncert_lag3 = draghi * uncert_lag3,
         draghiuncert_lag4 = draghi * uncert_lag4, 
         draghigdp_lag1 = draghi * gdp_lag1, 
         draghigdp_lag2 = draghi * gdp_lag2,
         draghigdp_lag3 = draghi * gdp_lag3,
         draghigdp_lag4 = draghi * gdp_lag4, 
         draghibop_lag1 = draghi * bop_lag1, 
         draghibop_lag2 = draghi * bop_lag2,
         draghibop_lag3 = draghi * bop_lag3,
         draghibop_lag4 = draghi * bop_lag4, 
         draghidebttogdp_lag1 = draghi * debttogdp_lag1, 
         draghidebttogdp_lag2 = draghi * debttogdp_lag2, 
         draghidebttogdp_lag3 = draghi * debttogdp_lag3, 
         draghidebttogdp_lag4 = draghi * debttogdp_lag4, 
         draghiinflation_lag1 = draghi * inflation_lag1, 
         draghiinflation_lag2 = draghi * inflation_lag2,
         draghiinflation_lag3 = draghi * inflation_lag3,
         draghiinflation_lag4 = draghi * inflation_lag4) %>% 
  mutate(d_2002 = ifelse(year == 2002, 1 , 0),
         d_2003 = ifelse(year == 2003, 1 , 0),
         d_2004 = ifelse(year == 2004, 1 , 0),
         d_2005 = ifelse(year == 2005, 1 , 0),
         d_2006 = ifelse(year == 2006, 1 , 0),
         d_2007 = ifelse(year == 2007, 1 , 0),
         d_2008 = ifelse(year == 2008, 1 , 0),
         d_2009 = ifelse(year == 2009, 1 , 0),
         d_2010 = ifelse(year == 2010, 1 , 0),
         d_2011 = ifelse(year == 2011, 1 , 0),
         d_2012 = ifelse(year == 2012, 1 , 0),
         d_2013 = ifelse(year == 2013, 1 , 0),
         d_2014 = ifelse(year == 2014, 1 , 0),
         d_2015 = ifelse(year == 2015, 1 , 0),
         d_2016 = ifelse(year == 2016, 1 , 0),
         d_2017 = ifelse(year == 2017, 1 , 0),
         d_2018 = ifelse(year == 2018, 1 , 0),
         d_2019 = ifelse(year == 2019, 1 , 0),
         d_2020 = ifelse(year == 2020, 1 , 0))

year_dummy_names = c("d_2002","d_2003","d_2004","d_2005","d_2006","d_2007","d_2008",
                     "d_2009","d_2010","d_2011","d_2012","d_2013","d_2014","d_2015",
                     "d_2016","d_2017","d_2018","d_2019", "d_2020")


####################################################################################################
#                                           AR SYNCH SYNCH (PIP>50%)                               #
####################################################################################################


# Subset the data to forecast and complete with NA for some countries are missing some dates
# Keep the observation for 2018Q4 because we will need it to forecast
# We have 9 observations per country: 8 to forecast + 1 the initial one 2018Q4

synch_synch_pip_forecast_data = synch_synch_data_pip %>% 
  filter(date >= as.Date("2018-10-01")) 

synch_synch_pip_forecast_data = complete(synch_synch_pip_forecast_data, country, date) %>% 
  mutate(f_synch = NA)

# Create all the AR models #
# We need in total 8 models (we use up until the last_obs - 1 to forecast)

ar.synch.synch.pip50.1 = lm(synch ~ country+	draghiinflation_lag4 +draghisynch_lag1+ draghisynch_lag2+
                              draghisynch_lag3 +draghisynch_lag4+ pigsdebttogdp_lag2 +pigsinflation_lag3 +pigsinflation_lag4 +
                              recsynch_lag1+ recsynch_lag2 +recsynch_lag4+ synch_lag1+ zlbdebttogdp_lag3 +zlbdebttogdp_lag4 +zlbinflation_lag2 +
                              zlbinflation_lag3+ zlbsynch_lag1+ zlbsynch_lag2+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018, data = synch_synch_data_pip %>% 
                               filter(date <= as.Date("2018-10-01"))) 

ar.synch.synch.pip50.2 = lm(synch ~ country+draghiinflation_lag4+ draghisynch_lag1 +draghisynch_lag2+
                              draghisynch_lag3+ draghisynch_lag4 +pigsdebttogdp_lag2+ pigsinflation_lag3+
                              pigsinflation_lag4+ recsynch_lag1 +recsynch_lag2+ recsynch_lag4+ synch_lag1+
                              zlbdebttogdp_lag3 +d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2019-01-01"))) 

ar.synch.synch.pip50.3 = lm(synch ~ country +draghiinflation_lag4 +draghisynch_lag1+ draghisynch_lag2 +draghisynch_lag4 +
                              pigsdebttogdp_lag2 +pigsinflation_lag3+ pigsinflation_lag4+ recsynch_lag1+ recsynch_lag2+
                              recsynch_lag4 +synch_lag1 +zlbdebttogdp_lag3+ zlbdebttogdp_lag4 +zlbinflation_lag3 +zlbsynch_lag1 +
                              zlbsynch_lag2+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2019-04-01")))

ar.synch.synch.pip50.4 = lm(synch ~  country  +draghiinflation_lag4+ draghisynch_lag1+ draghisynch_lag2+ draghisynch_lag4+
                              pigsdebttogdp_lag2 +pigsinflation_lag3 +pigsinflation_lag4+ recsynch_lag1 +recsynch_lag2 +recsynch_lag4 +
                              synch_lag1+ zlbdebttogdp_lag3+ zlbdebttogdp_lag4+ zlbinflation_lag3+ zlbsynch_lag1+ zlbsynch_lag2+ zlbsynch_lag3+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2019-07-01")))

ar.synch.synch.pip50.5 = lm(synch ~ country +draghiinflation_lag4 +draghisynch_lag1 +draghisynch_lag2+ pigsinflation_lag3 +pigsinflation_lag4 +
                              recsynch_lag1+ recsynch_lag2+ synch_lag1+ zlbdebttogdp_lag3+ zlbdebttogdp_lag4+ zlbinflation_lag2+
                              zlbinflation_lag3 +zlbsynch_lag1 +zlbsynch_lag2 +zlbsynch_lag3+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2019-10-01")))

ar.synch.synch.pip50.6 = lm(synch ~ country +draghiinflation_lag4+ draghisynch_lag1+ draghisynch_lag2+ pigsinflation_lag3+ pigsinflation_lag4+
                              recsynch_lag1 +recsynch_lag2 +synch_lag1 +zlbinflation_lag3+ zlbsynch_lag1+ zlbsynch_lag2 +zlbsynch_lag3+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019+d_2020, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2020-01-01")))

ar.synch.synch.pip50.7 = lm(synch ~ country +draghiinflation_lag4 +draghisynch_lag1+ draghisynch_lag2+ pigsinflation_lag3+ pigsinflation_lag4 +recsynch_lag1+
                              recsynch_lag2+ synch_lag1+ zlbinflation_lag2+ zlbinflation_lag3+ zlbsynch_lag1+ zlbsynch_lag2+ zlbsynch_lag3+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019+d_2020, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2020-04-01"))) 

ar.synch.synch.pip50.8 = lm(synch ~ country + draghiinflation_lag4+ draghisynch_lag1 +draghisynch_lag2+ pigsinflation_lag3 +pigsinflation_lag4 +
                              recsynch_lag1 +recsynch_lag2 +synch_lag1 +zlbinflation_lag2+ zlbinflation_lag3+ zlbsynch_lag1+ zlbsynch_lag2+
                              zlbsynch_lag3+d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010+ d_2011+ d_2012+ d_2013+ d_2014+ d_2015+ d_2016+ d_2017 +d_2018+d_2019+d_2020, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2020-07-01")))



# Now, create the forecasts #
# Note that the first observation is the last observation used in the models
# Hence, it is the first one used to build the forecast


f_synch_index = which( colnames(synch_synch_pip_forecast_data)=="f_synch" )


for (mm in 1:dim(synch_synch_pip_forecast_data)[1]) {
  
  if (mm %in% seq(2,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.1, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "draghisynch_lag3", "draghisynch_lag4", "pigsdebttogdp_lag2",
                                                                                   "pigsinflation_lag3", "pigsinflation_lag4", "recsynch_lag1",
                                                                                   "recsynch_lag2", "recsynch_lag4" ,"synch_lag1" ,"zlbdebttogdp_lag3" , 
                                                                                   "zlbdebttogdp_lag4", "zlbinflation_lag2", "zlbinflation_lag3", "zlbsynch_lag1",
                                                                                   "zlbsynch_lag2", "country", year_dummy_names[1:17])])
    
  }
  
  if (mm %in% seq(3,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.2, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "draghisynch_lag3", "draghisynch_lag4", "pigsdebttogdp_lag2", 
                                                                                   "pigsinflation_lag3", "pigsinflation_lag4", "recsynch_lag1", 
                                                                                   "recsynch_lag2", "recsynch_lag4", "synch_lag1", "zlbdebttogdp_lag3",
                                                                                   "zlbdebttogdp_lag4", "zlbinflation_lag2", "zlbinflation_lag3",
                                                                                   "zlbsynch_lag1", "zlbsynch_lag2", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(4,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.3, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "draghisynch_lag4", "pigsdebttogdp_lag2", "pigsinflation_lag3", 
                                                                                   "pigsinflation_lag4", "recsynch_lag1", "recsynch_lag2", "recsynch_lag4",
                                                                                   "synch_lag1", "zlbdebttogdp_lag3", "zlbdebttogdp_lag4", "zlbinflation_lag3",
                                                                                   "zlbsynch_lag1", "zlbsynch_lag2", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(5,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.4, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "draghisynch_lag4", "pigsdebttogdp_lag2", "pigsinflation_lag3",
                                                                                   "pigsinflation_lag4", "recsynch_lag1", "recsynch_lag2", "recsynch_lag4",
                                                                                   "synch_lag1", "zlbdebttogdp_lag3", "zlbdebttogdp_lag4", "zlbinflation_lag3",
                                                                                   "zlbsynch_lag1", "zlbsynch_lag2", "zlbsynch_lag3", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(6,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.5, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "pigsinflation_lag3", "pigsinflation_lag4", "recsynch_lag1", 
                                                                                   "recsynch_lag2", "synch_lag1", "zlbdebttogdp_lag3", "zlbdebttogdp_lag4",
                                                                                   "zlbinflation_lag2", "zlbinflation_lag3", "zlbsynch_lag1",
                                                                                   "zlbsynch_lag2", "zlbsynch_lag3", "country", year_dummy_names[1:18])])
    
  }
  
  if (mm %in% seq(7,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.6, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "pigsinflation_lag3", "pigsinflation_lag4", "recsynch_lag1",
                                                                                   "recsynch_lag2", "synch_lag1", "zlbinflation_lag3", "zlbsynch_lag1",
                                                                                   "zlbsynch_lag2", "zlbsynch_lag3", "country", year_dummy_names)])
  }
  
  if (mm %in% seq(8,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.7, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "pigsinflation_lag3", "pigsinflation_lag4", "recsynch_lag1",
                                                                                   "recsynch_lag2", "synch_lag1", "zlbinflation_lag2", "zlbinflation_lag3",
                                                                                   "zlbsynch_lag1", "zlbsynch_lag2", "zlbsynch_lag3", "country", year_dummy_names)])
  }
  
  if (mm %in% seq(9,135,9)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.8, newdata = synch_synch_pip_forecast_data[mm,c("draghiinflation_lag4", "draghisynch_lag1", "draghisynch_lag2",
                                                                                   "pigsinflation_lag3", "pigsinflation_lag4", "recsynch_lag1",
                                                                                   "recsynch_lag2", "synch_lag1", "zlbinflation_lag2", "zlbinflation_lag3", 
                                                                                   "zlbsynch_lag1", "zlbsynch_lag2", "zlbsynch_lag3", "country", year_dummy_names)])
    
  }
  
  
  
}

# Create variable for directional accuracy

synch_synch_pip_forecast_data = synch_synch_pip_forecast_data %>% 
  group_by(country) %>% 
  mutate(synch_diff = c(NA, diff(synch)), 
         f_synch_diff = c(NA, diff(f_synch)), 
         synch_direction = ifelse(synch_diff > 0, "up", "down"), 
         f_synch_direction = ifelse(f_synch_diff > 0, "up", "down")) %>% 
  select(-synch_diff, -f_synch_diff)



# Create squared difference between realization and forecast #

synch_synch_pip_forecast_data = synch_synch_pip_forecast_data %>% 
  mutate(res_sq = (synch-f_synch)^2)



# Do the same for PIIGS countries

synch_synch_pip_forecast_data.piigs = synch_synch_pip_forecast_data %>% 
  filter(country %in% piigs_subset)

# Do the same for non-PIIGS countries

synch_synch_pip_forecast_data.nopiigs = synch_synch_pip_forecast_data %>% 
  filter(!country %in% piigs_subset)



# Compute the RMSE 

rmse.synch.synch.pip50 = synch_synch_pip_forecast_data %>% 
  ungroup() %>% 
  filter(!is.na(res_sq)) %>% 
  summarise(rmse = sqrt(sum(res_sq) / nrow(synch_synch_pip_forecast_data%>% 
                                             filter(!is.na(res_sq)))))

# Do the same for PIIGS countries #

rmse.synch.synch.pip50.piigs = synch_synch_pip_forecast_data.piigs %>% 
  ungroup() %>% 
  filter(!is.na(res_sq)) %>% 
  summarise(rmse = sqrt(sum(res_sq) / nrow(synch_synch_pip_forecast_data.piigs%>% 
                                             filter(!is.na(res_sq)))))

# Do the same for non-PIIGS countries


rmse.synch.synch.pip50.nopiigs = synch_synch_pip_forecast_data.nopiigs %>% 
  ungroup() %>% 
  filter(!is.na(res_sq)) %>% 
  summarise(rmse = sqrt(sum(res_sq) / nrow(synch_synch_pip_forecast_data.nopiigs%>% 
                                             filter(!is.na(res_sq)))))


# Compute the Directional Accuracy measure 

table.synch.synch.pip50 = table(synch_synch_pip_forecast_data$f_synch_direction, synch_synch_pip_forecast_data$synch_direction)

da.synch.synch.pip50 = round(sum(diag(table.synch.synch.pip50)) / sum(table.synch.synch.pip50), 4)

hr.synch.synch.pip50 = table.synch.synch.pip50[4]/(table.synch.synch.pip50[4]+table.synch.synch.pip50[3])

fa.synch.synch.pip50 = table.synch.synch.pip50[2]/(table.synch.synch.pip50[2]+table.synch.synch.pip50[1])

ks.synch.synch.pip50 =  hr.synch.synch.pip50 - fa.synch.synch.pip50



# Do the same for PIIGS countries #

table.synch.synch.pip50.piigs = table(synch_synch_pip_forecast_data.piigs$f_synch_direction, synch_synch_pip_forecast_data.piigs$synch_direction)

da.synch.synch.pip50.piigs = round(sum(diag(table.synch.synch.pip50.piigs)) / sum(table.synch.synch.pip50.piigs), 4)

hr.synch.synch.pip50.piigs = table.synch.synch.pip50.piigs[4]/(table.synch.synch.pip50.piigs[4]+table.synch.synch.pip50.piigs[3])

fa.synch.synch.pip50.piigs = table.synch.synch.pip50.piigs[2]/(table.synch.synch.pip50.piigs[2]+table.synch.synch.pip50.piigs[1])

ks.synch.synch.pip50.piigs =  hr.synch.synch.pip50.piigs - fa.synch.synch.pip50.piigs

# Do the same for non-PIIGS countries #

table.synch.synch.pip50.nopiigs = table(synch_synch_pip_forecast_data.nopiigs$f_synch_direction, synch_synch_pip_forecast_data.nopiigs$synch_direction)

da.synch.synch.pip50.nopiigs = round(sum(diag(table.synch.synch.pip50.nopiigs)) / sum(table.synch.synch.pip50.nopiigs), 4)

hr.synch.synch.pip50.nopiigs = table.synch.synch.pip50.nopiigs[4]/(table.synch.synch.pip50.nopiigs[4]+table.synch.synch.pip50.nopiigs[3])

fa.synch.synch.pip50.nopiigs = table.synch.synch.pip50.nopiigs[2]/(table.synch.synch.pip50.nopiigs[2]+table.synch.synch.pip50.nopiigs[1])

ks.synch.synch.pip50.nopiigs =  hr.synch.synch.pip50.nopiigs - fa.synch.synch.pip50.nopiigs



# Create the remaining rows for the final table

# All countries #

round(c(rmse.synch.synch.pip50$rmse, da.synch.synch.pip50, hr.synch.synch.pip50, fa.synch.synch.pip50,ks.synch.synch.pip50),4)

# PIIGS #

round(c(rmse.synch.synch.pip50.piigs$rmse, da.synch.synch.pip50.piigs, hr.synch.synch.pip50.piigs, fa.synch.synch.pip50.piigs,ks.synch.synch.pip50.piigs),4)


# Non-PIIGS #

round(c(rmse.synch.synch.pip50.nopiigs$rmse, da.synch.synch.pip50.nopiigs, hr.synch.synch.pip50.nopiigs, fa.synch.synch.pip50.nopiigs,ks.synch.synch.pip50.nopiigs),4)

