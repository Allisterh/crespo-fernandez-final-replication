# File with the specifications to replicate the paper 

# String with the data path

data_path = "data"

# Number of iterations and burn-in for BMA models 

n.burn.bma = 10000
n.iter.bma = 1e+07

# Number of iterations and burn-in for BMA models (forecast section)

n.burn.fcast = 10000
n.iter.fcast = 500000

# Width and height for jointness plots

width = 1600
height = 900

# Create PI(I)GS indicator variable to subset the forecasted measures #

piigs_subset = c("spain", "portugal", "ireland", "greece", "italy")

