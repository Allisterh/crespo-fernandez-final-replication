
# Clean working space #

rm(list=ls())
base::set.seed(14091998)

# Load the necessary functions, packages, and specifications

func.files <- base::list.files(path = "./", pattern = "^~", full.names = TRUE)

for (file in func.files) {
  
  base::source(file)
  
}

# Read the corresponding dataset

fulldata = readxl::read_excel(file.path(data_path, "spread_levels.xlsx"))

# Add Draghi dummy + interactions

fulldata = fulldata = add.draghi.spread(fulldata)


# Model with Fixed Effects #

# Select appropriate variables #
# Create dummy variables to control for year and country fixed effects #
# Note: need to create dummies except for one country and year group! #
# We have in total 15 countries and 21 years #
# So in total, 14 country dummies + 20 year dummies = 34 FE #
# All countries except Spain, all years except for 2021 #
# Important: to avoid multicollinearity issues, do not estimate dummy for PI(I)GS #


fe_data = fulldata %>% 
  dplyr::select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>% 
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  dummy_cols(select_columns = "country", remove_first_dummy = T) %>% 
  dplyr::select(-year,-country, -pigs)

model_fe <-  BMS::bms(fe_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                 nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                 fixed.reg = c(year_dummy_names, country_dummy_names))


coefs_fe <- stats::coef(model_fe,  std.coefs = T, order.by.pip = F)

# Do not select the Fixed Effects results (year & country) #

coefs_fe = coefs_fe[!row.names(coefs_fe) %in% c(country_dummy_names, year_dummy_names),1:3]


# Model without Fixed Effects: add only time (year) fixed effects  #

nofe_data = fulldata %>% 
  dplyr::select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  dplyr::select(-year,-country)



model_nofe <- BMS::bms(nofe_data, burn = n.burn, iter = n.iter,  g = "BRIC", mprior = "random", 
                  nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                  fixed.reg = year_dummy_names)


coefs_nofe <- stats::coef(model_nofe,  std.coefs = T, order.by.pip = F)[,1:3]

# Do not select year fixed effects #

coefs_nofe = coefs_nofe[!row.names(coefs_nofe) %in% c(year_dummy_names),1:3]

# Model with strong heredity prior: add only time (year) fixed effects #
# Note that this is the same dataset used for the no Fixed Effects model #

heredity_data = fulldata %>% 
  dplyr::select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>% 
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  dplyr::select(-year,-country)


# Need to change interaction variable names #
# For consistency with BMS package # 

heredity_data = 

model_heredity =  BMS::bms(heredity_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                      nmodel = 10000, mcmc = "bd.int", user.int = F, randomizeTimer = F, 
                      fixed.reg = year_dummy_names)


coefs_heredity <- stats::coef(model_heredity,  std.coefs = T, order.by.pip = F)

# Do not select the year fixed effects #

coefs_heredity = coefs_heredity[!row.names(coefs_heredity) %in% year_dummy_names,1:3]

# Reproduce all columns of Table A.4 in the paper 

base::round(coefs_fe, 4)
base::round(coefs_nofe, 4)
base::round(coefs_heredity, 4)
