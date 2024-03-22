# Functions that are called by all replication files 

# Produce summary statistics for each country

stats.country = function(my_data, my_country) {  
  
  sum.stats = my_data %>% 
    dplyr::filter(country == base::as.name(my_country)) %>% 
    dplyr::summarise(round(mean(synch), 3), round(sd(synch), 3), round(min(synch), 3), round(max(synch), 3),
              round(mean(uncert), 3), round(sd(uncert), 3), round(min(uncert), 3), round(max(uncert), 3), 
              round(mean(gdp), 3), round(sd(gdp), 3), round(min(gdp), 3), round(max(gdp), 3), 
              round(mean(debttogdp), 3), round(sd(debttogdp), 3), round(min(debttogdp), 3), round(max(debttogdp), 3), 
              round(mean(inflation), 3), round(sd(inflation), 3), round(min(inflation), 3), round(max(inflation), 3), 
              round(mean(rec), 3), round(sd(rec), 3), round(min(rec), 3), round(max(rec), 3))
  
  base::print(sum.stats[1:4])
  base::print(sum.stats[5:8])
  base::print(sum.stats[9:12])
  base::print(sum.stats[13:16])
  base::print(sum.stats[17:20])
  base::print(sum.stats[21:24])
  
}

# Compute RMSE 

compute.rmse = function(statistic){ 
  
  round(sqrt(mean(statistic, na.rm = T)), 4)  
  
}

# Directional results

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

# Jointness function 

jointness.score <- function(model_object, method) {
  
  j.pips <- coef ( model_object ) [ ,1:3]
  top.includes <- topmodels.bma ( model_object )
  cov.nam <- head (row.names ( top.includes ) , -2)
  cov.no <- length (cov.nam )
  pmps <- pmp.bma ( model_object ) [ ,2]
  pmps <- pmps /sum( pmps )
  jointness <- matrix (0 , cov.no , cov.no )
  for ( i in 1: cov.no ) {
    for ( j in 1: cov.no ) {
      selected.includes <- top.includes [c(i , j ) ,]
      tl <- sum ( pmps [ colSums ( selected.includes ) ==2])
      tr <- sum ( pmps [ colSums ( selected.includes ) ==0])
      bl <- sum ( pmps [ selected.includes [1 ,]==0 & selected.includes [2 ,]==1])
      br <- sum ( pmps [ selected.includes [1 ,]==1 & selected.includes [2 ,]==0])
      cr <- tl + br
      cl <- tl + bl
      if ( method =="Pure"){
        ## Raw jointness statistic 1
        jointness [i ,j ] <- tl }
      if ( method =="LS1") {
        ## Ley - Steel jointness statistic 1
        jointness [i ,j ] <- tl/( cl + cr - tl ) }
      if ( method =="LS2") {
        ## Lee - Steel jointness statistic 2
        jointness [i ,j ] <- tl/( bl + br ) }
      if ( method =="DW1") {
        ## Doppelhofer - Weeks jointness statistic 1
        jointness [i ,j ] <- log( tl/( cl*cr ) ) }
      if ( method =="DW2") {
        ## Doppelhofer - Weeks jointness statistic 2
        jointness [i ,j ] <- log (( tl*tr )/( bl*br ) ) }
      if ( method =="St") {
        ## Strachan jointness statistic
        jointness [i ,j ] <- cl*cr*log ( tl/( bl*br ) ) }
      if ( method =="YQ") {
        ## Hofmarcher et al. jointness statistic
        jointness [i ,j ] <- ( tl*tr - br*bl )/( tl*tr + br*bl ) }
      if ( method =="YQM") {
        ## Hofmarcher et al. modified jointness statistic
        jointness [i ,j ] <- (( tl +0.5) *( tr +0.5) -
                                ( br +0.5) *( bl +0.5) )/
          (( tl +0.5) *( tr +0.5) +
             ( br +0.5) *( bl +0.5) -0.5) }
    }
  }
  colnames ( jointness ) <- cov.nam
  rownames ( jointness ) <- cov.nam
  jointness <- round(jointness, 4)
  diag ( jointness ) <- "."
  
  return ( jointness )
}

# Function to compute theil statistic

my_theil = function(realized, predicted){ 
  
  round(TheilU(realized, predicted, na.rm = T, type = 2),4)
  
}

# Function to compute t-tests 

my.ttest = function(mycountry, myvar){ 
  
  
  
  t.test(synch.data %>% 
           filter(country == mycountry, {{myvar}} == 1) %>% 
           pull(synch), 
         synch.data %>% 
           filter(country == mycountry, {{myvar}} == 0) %>% 
           pull(synch)
  )
  
  
  
  
}

# Function to compute results without the year 2020

without.2020 = function(model_object){ 
  
  model_object %>% 
    filter(!date == "2018-10-01", date < "2020-01-01")
  
}

# Function to add Draghi dummies and interactions for synch dataset

add.draghi.synch = function(my_data){ 
  
  my_data %>% 
    dplyr::mutate(draghi = dplyr::if_else(date >= '2012-07-01', 1, 0), 
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
  
  
}

# Function to add Draghi dummies and interactions for spread dataset

add.draghi.spread = function(my_data){ 
  
  my_data %>% 
    dplyr::mutate(draghi = dplyr::if_else(date >= '2012-07-01', 1, 0), 
                  draghispread_lag1 = draghi * spread_lag1, 
                  draghispread_lag2 = draghi * spread_lag2, 
                  draghispread_lag3 = draghi * spread_lag3, 
                  draghispread_lag4 = draghi * spread_lag4, 
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
  
  
}

# Function to rename synch variables to comply with BMS package 

rename.synch = function(my_data){
  
  my_data %>% 
    dplyr::rename("rec#synch_lag1" = recsynch_lag1,
                  "rec#synch_lag2" = recsynch_lag2,
                  "rec#synch_lag3" = recsynch_lag3,
                  "rec#synch_lag4" = recsynch_lag4,
                  "rec#uncert_lag1" = recuncert_lag1,
                  "rec#uncert_lag2" = recuncert_lag2,
                  "rec#uncert_lag3" = recuncert_lag3,
                  "rec#uncert_lag4" = recuncert_lag4,
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
                  "pigs#uncert_lag1" = pigsuncert_lag1,
                  "pigs#uncert_lag2" = pigsuncert_lag2,
                  "pigs#uncert_lag3" = pigsuncert_lag3,
                  "pigs#uncert_lag4" = pigsuncert_lag4,
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
                  "zlb#uncert_lag1" = zlbuncert_lag1,
                  "zlb#uncert_lag2" = zlbuncert_lag2,
                  "zlb#uncert_lag3" = zlbuncert_lag3,
                  "zlb#uncert_lag4" = zlbuncert_lag4,
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
                  "draghi#uncert_lag1" = draghiuncert_lag1,
                  "draghi#uncert_lag2" = draghiuncert_lag2,
                  "draghi#uncert_lag3" = draghiuncert_lag3,
                  "draghi#uncert_lag4" = draghiuncert_lag4,
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
  
  
}


# Function to rename spread variables to comply with BMS package 

rename.spread = function(my_data){
  
  heredity_data %>% 
    dplyr::rename("rec#spread_lag1" = recspread_lag1,
                  "rec#spread_lag2" = recspread_lag2,
                  "rec#spread_lag3" = recspread_lag3,
                  "rec#spread_lag4" = recspread_lag4,
                  "rec#uncert_lag1" = recuncert_lag1,
                  "rec#uncert_lag2" = recuncert_lag2,
                  "rec#uncert_lag3" = recuncert_lag3,
                  "rec#uncert_lag4" = recuncert_lag4,
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
                  "pigs#spread_lag1" = pigsspread_lag1,
                  "pigs#spread_lag2" = pigsspread_lag2,
                  "pigs#spread_lag3" = pigsspread_lag3,
                  "pigs#spread_lag4" = pigsspread_lag4,
                  "pigs#uncert_lag1" = pigsuncert_lag1,
                  "pigs#uncert_lag2" = pigsuncert_lag2,
                  "pigs#uncert_lag3" = pigsuncert_lag3,
                  "pigs#uncert_lag4" = pigsuncert_lag4,
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
                  "zlb#spread_lag1" = zlbspread_lag1,
                  "zlb#spread_lag2" = zlbspread_lag2,
                  "zlb#spread_lag3" = zlbspread_lag3,
                  "zlb#spread_lag4" = zlbspread_lag4,
                  "zlb#uncert_lag1" = zlbuncert_lag1,
                  "zlb#uncert_lag2" = zlbuncert_lag2,
                  "zlb#uncert_lag3" = zlbuncert_lag3,
                  "zlb#uncert_lag4" = zlbuncert_lag4,
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
                  
                  "draghi#spread_lag1" = draghispread_lag1,
                  "draghi#spread_lag2" = draghispread_lag2,
                  "draghi#spread_lag3" = draghispread_lag3,
                  "draghi#spread_lag4" = draghispread_lag4,
                  "draghi#uncert_lag1" = draghiuncert_lag1,
                  "draghi#uncert_lag2" = draghiuncert_lag2,
                  "draghi#uncert_lag3" = draghiuncert_lag3,
                  "draghi#uncert_lag4" = draghiuncert_lag4,
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
  
  
  
}

# Functions to fit BMS models 

fit.bms = function(my_data, model_type){
  
  if (model_type==1) { # Model with fixed effects 
    
    BMS::bms(my_data, burn = n.burn.bma, iter = n.iter.bma, g = "BRIC", mprior = "random", 
             nmodel = 10000, mcmc = "bd", user.int = F, 
             fixed.reg = c(year_dummy_names, country_dummy_names), randomizeTimer = F)
    
  } else if (model_type==2) { # Standard BMA model (no fixed effects)
    
    BMS::bms(my_data, burn = n.burn.bma, iter = n.iter.bma, g = "BRIC", mprior = "random", 
             nmodel = 10000, mcmc = "bd", user.int = F, 
             fixed.reg = year_dummy_names, randomizeTimer = F)
  
  } else if (model_type==3){ # Heredity model
    
    
    BMS::bms(my_data, burn = n.burn.bma, iter = n.iter.bma, g = "BRIC", mprior = "random", 
             nmodel = 10000, mcmc = "bd.int", user.int = F, 
             fixed.reg = year_dummy_names, randomizeTimer = F)
    
    
  }
  
  
  
}