jointness.score <- function (model_object, method) {
  
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