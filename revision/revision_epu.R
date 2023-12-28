

# Start the clock
rm(list=ls())
start.time <- Sys.time()
print(start.time)
# Clean working space and load necessary libraries #



if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, BMS, Cairo, reshape2, ggplot2, writexl, 
               xtable, stargazer, stringr,lubridate,zoo)

# Set seed and number of iterations + burnin phase

set.seed(14091998)
n.burn = 10000
n.iter = 1e7

# Read the dataset, set WD #

data_path = "../bma_data"

fulldata = read_excel(file.path(data_path, "synch_levels.xlsx"))

# Import the EPU index by Baker et al

epu.data = read_xlsx("data/epu_data.xlsx" ,range = "A1:AB463") 

names(epu.data) = str_to_lower(names(epu.data))
set.countries = unique(fulldata$country)

epu.data = epu.data %>% 
  pivot_longer(!c(year, month), names_to = "country", values_to = "epu") %>% 
  filter(country %in% set.countries) %>% 
  mutate(date = as.Date(as.yearmon(paste0(year, "m", month), format = "%Ym%m"))) %>% 
  select(-year,-month) %>% 
  relocate(date)

# Now, compute quarterly averages from monthly data 

epu.data = epu.data %>% 
  mutate(quarter = quarter(date), 
         year = year(date)) %>% 
  group_by(year, quarter, country) %>% 
  summarise(epu_q = mean(epu)) 

epu.data = epu.data %>% 
  mutate(date = as.Date(as.yearqtr(paste0(year, "q", quarter)))) %>% 
  ungroup() %>% 
  select(-year,-quarter) %>% 
  arrange(country, date) 

# Generate lags of the EPU variable 

epu.data = epu.data %>% 
  group_by(country) %>% 
  mutate(epu_lag1 = lag(epu_q, n = 1), 
         epu_lag2 = lag(epu_q, n = 2), 
         epu_lag3 = lag(epu_q, n = 3), 
         epu_lag4 = lag(epu_q, n = 4)) %>% 
  select(-epu_q)

# Add Draghi dummy + interactions

fulldata = fulldata %>% 
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
         draghiinflation_lag4 = draghi * inflation_lag4) 

# Time to merge both datasets before adding more modifications 

fulldata = merge(fulldata, epu.data) %>% 
  arrange(country,date)

# Delete variables at time t and the uncert variables

fulldata = fulldata %>% 
  select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation, 
         -contains("uncert"))

# Now, create the interactions with the epu index 

fulldata = fulldata %>% 
  mutate(recepu_lag1 = rec * epu_lag1, 
         recepu_lag2 = rec * epu_lag2, 
         recepu_lag3 = rec * epu_lag3, 
         recepu_lag4 = rec * epu_lag4, 
         zlbepu_lag1 = zlb * epu_lag1,
         zlbepu_lag2 = zlb * epu_lag2,
         zlbepu_lag3 = zlb * epu_lag3,
         zlbepu_lag4 = zlb * epu_lag4, 
         pigsepu_lag1 = pigs * epu_lag1, 
         pigsepu_lag2 = pigs * epu_lag2, 
         pigsepu_lag3 = pigs * epu_lag3, 
         pigsepu_lag4 = pigs * epu_lag4, 
         draghiepu_lag1 = draghi * epu_lag1, 
         draghiepu_lag2 = draghi * epu_lag2, 
         draghiepu_lag3 = draghi * epu_lag3, 
         draghiepu_lag4 = draghi * epu_lag4
         )

# Create dummy names (they are the same years as the original dataset)


year_dummy_names = c("d_2002",
                     "d_2003",
                     "d_2004",
                     "d_2005",
                     "d_2006",
                     "d_2007",
                     "d_2008",
                     "d_2009",
                     "d_2010",
                     "d_2011",
                     "d_2012",
                     "d_2013",
                     "d_2014",
                     "d_2015",
                     "d_2016",
                     "d_2017",
                     "d_2018",
                     "d_2019",
                     "d_2020", 
                     "d_2021")

# Note that the country sample size is smaller now

country_dummy_names = c("d_germany", 
                        "d_greece", 
                        "d_ireland", 
                        "d_italy", 
                        "d_netherlands", 
                        "d_spain")

# Model with Fixed Effects #

# Select appropriate variables #
# Create dummy variables to control for year and country fixed effects #
# Note: need to create dummies except for one country and year group! #
# We have in total 7 countries and 21 years #
# So in total, 6 country dummies + 20 year dummies = 34 FE #
# All countries except France, all years except for 2021 #
# Important: to avoid multicollinearity issues, do not estimate dummy for PI(I)GS #

# Create the Fixed Effects model dataset 

fe_data = fulldata %>% 
  
  mutate(d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0)) %>%
  
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
         d_2020 = ifelse(year == 2020, 1 , 0),
         d_2021 = ifelse(year == 2021, 1 , 0)) %>% 
  select(-year,-country)

# Create the standard BMA model dataset

nofe_data = fe_data %>% 
  select(-country_dummy_names)

# Create the model with a strong heredity prior 
# Need to change interaction variable names #
# For consistency with BMS package # 

heredity_data = nofe_data %>% 
  rename("rec#synch_lag1" = recsynch_lag1,
         "rec#synch_lag2" = recsynch_lag2,
         "rec#synch_lag3" = recsynch_lag3,
         "rec#synch_lag4" = recsynch_lag4,
         "rec#epu_lag1" = recepu_lag1,
         "rec#epu_lag2" = recepu_lag2,
         "rec#epu_lag3" = recepu_lag3,
         "rec#epu_lag4" = recepu_lag4,
         "rec#bop_lag1" = recbop_lag1,
         "rec#bop_lag2" = recbop_lag2,
         "rec#bop_lag3" = recbop_lag3,
         "rec#bop_lag4" = recbop_lag4, 
         "rec#debttogdp_lag1" = recdebttogdp_lag1,
         "rec#debttogdp_lag2" = recdebttogdp_lag2,
         "rec#debttogdp_lag3" = recdebttogdp_lag3,
         "rec#debttogdp_lag4" = recdebttogdp_lag4,
         "rec#gdp_lag1" = recgdp_lag1,
         "rec#gdp_lag2" = recgdp_lag2,
         "rec#gdp_lag3" = recgdp_lag3,
         "rec#gdp_lag4" = recgdp_lag4,
         "rec#inflation_lag1" = recinflation_lag1,
         "rec#inflation_lag2" = recinflation_lag2,
         "rec#inflation_lag3" = recinflation_lag3,
         "rec#inflation_lag4" = recinflation_lag4,
         "pigs#synch_lag1" = pigssynch_lag1,
         "pigs#synch_lag2" = pigssynch_lag2,
         "pigs#synch_lag3" = pigssynch_lag3,
         "pigs#synch_lag4" = pigssynch_lag4,
         "pigs#epu_lag1" = pigsepu_lag1,
         "pigs#epu_lag2" = pigsepu_lag2,
         "pigs#epu_lag3" = pigsepu_lag3,
         "pigs#epu_lag4" = pigsepu_lag4,
         "pigs#bop_lag1" = pigsbop_lag1,
         "pigs#bop_lag2" = pigsbop_lag2,
         "pigs#bop_lag3" = pigsbop_lag3,
         "pigs#bop_lag4" = pigsbop_lag4,
         "pigs#debttogdp_lag1" = pigsdebttogdp_lag1,
         "pigs#debttogdp_lag2" = pigsdebttogdp_lag2,
         "pigs#debttogdp_lag3" = pigsdebttogdp_lag3,
         "pigs#debttogdp_lag4" = pigsdebttogdp_lag4,
         "pigs#gdp_lag1" = pigsgdp_lag1,
         "pigs#gdp_lag2" = pigsgdp_lag2,
         "pigs#gdp_lag3" = pigsgdp_lag3,
         "pigs#gdp_lag4" = pigsgdp_lag4,
         "pigs#inflation_lag1" = pigsinflation_lag1,
         "pigs#inflation_lag2" = pigsinflation_lag2,
         "pigs#inflation_lag3" = pigsinflation_lag3,
         "pigs#inflation_lag4" = pigsinflation_lag4,
         "zlb#synch_lag1" = zlbsynch_lag1,
         "zlb#synch_lag2" = zlbsynch_lag2,
         "zlb#synch_lag3" = zlbsynch_lag3,
         "zlb#synch_lag4" = zlbsynch_lag4,
         "zlb#epu_lag1" = zlbepu_lag1,
         "zlb#epu_lag2" = zlbepu_lag2,
         "zlb#epu_lag3" = zlbepu_lag3,
         "zlb#epu_lag4" = zlbepu_lag4,
         "zlb#bop_lag1" = zlbbop_lag1,
         "zlb#bop_lag2" = zlbbop_lag2,
         "zlb#bop_lag3" = zlbbop_lag3,
         "zlb#bop_lag4" = zlbbop_lag4,
         "zlb#debttogdp_lag1" = zlbdebttogdp_lag1,
         "zlb#debttogdp_lag2" = zlbdebttogdp_lag2,
         "zlb#debttogdp_lag3" = zlbdebttogdp_lag3,
         "zlb#debttogdp_lag4" = zlbdebttogdp_lag4,
         "zlb#gdp_lag1" = zlbgdp_lag1,
         "zlb#gdp_lag2" = zlbgdp_lag2,
         "zlb#gdp_lag3" = zlbgdp_lag3,
         "zlb#gdp_lag4" = zlbgdp_lag4,
         "zlb#inflation_lag1" = zlbinflation_lag1,
         "zlb#inflation_lag2" = zlbinflation_lag2,
         "zlb#inflation_lag3" = zlbinflation_lag3,
         "zlb#inflation_lag4" = zlbinflation_lag4,
         
         "draghi#synch_lag1" = draghisynch_lag1,
         "draghi#synch_lag2" = draghisynch_lag2,
         "draghi#synch_lag3" = draghisynch_lag3,
         "draghi#synch_lag4" = draghisynch_lag4,
         "draghi#epu_lag1" = draghiepu_lag1,
         "draghi#epu_lag2" = draghiepu_lag2,
         "draghi#epu_lag3" = draghiepu_lag3,
         "draghi#epu_lag4" = draghiepu_lag4,
         "draghi#bop_lag1" = draghibop_lag1,
         "draghi#bop_lag2" = draghibop_lag2,
         "draghi#bop_lag3" = draghibop_lag3,
         "draghi#bop_lag4" = draghibop_lag4,
         "draghi#debttogdp_lag1" = draghidebttogdp_lag1,
         "draghi#debttogdp_lag2" = draghidebttogdp_lag2,
         "draghi#debttogdp_lag3" = draghidebttogdp_lag3,
         "draghi#debttogdp_lag4" = draghidebttogdp_lag4,
         "draghi#gdp_lag1" = draghigdp_lag1,
         "draghi#gdp_lag2" = draghigdp_lag2,
         "draghi#gdp_lag3" = draghigdp_lag3,
         "draghi#gdp_lag4" = draghigdp_lag4,
         "draghi#inflation_lag1" = draghiinflation_lag1,
         "draghi#inflation_lag2" = draghiinflation_lag2,
         "draghi#inflation_lag3" = draghiinflation_lag3,
         "draghi#inflation_lag4" = draghiinflation_lag4)

# Note: in total we have 150 independent variables: 
# 4 dummies (we can estimate PIIGS dummy because we are missing Portugal) + 4 lags * 4 dummies * 6 vars + 6 vars * 4 lags + 6 country dummies + 20 year dummies

model.epu.fe = bms(fe_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                 nmodel = 10000, mcmc = "bd", user.int = F, 
                 fixed.reg = c(year_dummy_names, country_dummy_names), randomizeTimer = F)


model.epu.nofe = bms(nofe_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                   nmodel = 10000, mcmc = "bd", user.int = F, 
                   fixed.reg = year_dummy_names, randomizeTimer = F)


model.epu.heredity = bms(heredity_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                     nmodel = 10000, mcmc = "bd.int", user.int = F, 
                     fixed.reg = year_dummy_names, randomizeTimer = F)



coefs.fe <- coef(model.epu.fe,  std.coefs = T, order.by.pip = F)
coefs.nofe <- coef(model.epu.nofe,  std.coefs = T, order.by.pip = F)
coefs.heredity <- coef(model.epu.heredity,  std.coefs = T, order.by.pip = F)

# Do not select the Fixed Effects results (year & country) #

coefs.fe = coefs.fe[!row.names(coefs.fe) %in% c(country_dummy_names, year_dummy_names),1:3]
coefs.nofe = coefs.nofe[!row.names(coefs.nofe) %in% c(year_dummy_names),1:3]
coefs.heredity = coefs.heredity[!row.names(coefs.heredity) %in% c(year_dummy_names),1:3]

# Stop the clock

current.time = Sys.time()
difftime(current.time, start.time, units = 'mins')

# Display the results 

coefs.fe
coefs.nofe
coefs.heredity
