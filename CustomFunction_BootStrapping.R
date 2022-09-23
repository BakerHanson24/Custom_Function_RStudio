## I got a fever, and the only prescription 
  #is a custom-made Bootstrap function...

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Create original data, then round to 2 decimal points.
original <- runif(8,-5,5)
original

# Mean of original data
mean_original <- mean(original)
mean_original

# Create 10,000 Bootstrap number lines
Boot <- replicate(10000,sample(original,8, replace = TRUE))
Boot
View(Boot)
  
# Transpose Matrix
Boot <- t(Boot)
View(Boot)

# Change Boot from Matrix to Data.Frame
Boot_DF <- as.data.frame(Boot)

# Extract the 10,000 means created while Bootstrapping
Boot_Means <- colMeans(Boot_DF)

# Must be a single row (or column) Data.Frame (not Vector),
  # in order to graph as a Histogram
Boot_Means <- as.data.frame(Boot_Means)
Boot_Means


## histogram of means

Dist_Means <- ggplot(Boot_Means, aes(Boot_Means)) +
                geom_histogram(binwidth = 0.1)
Dist_Means

## ---------------------- INTERMISSION --------------------------- ##
## --- ABOVE:  the long way to pull off Bootstrapping,
## --- BELOW:  my new function that will make Bootstrapping quicker.
## ------------------------ RESUME ------------------------------- ##


original <- runif(8,-5,5)

my_rad_bootstrap_function <- function(og_vector, num_bootstraps, round_value = NA) {
  Boot <- replicate(num_bootstraps,
                    sample(og_vector,length(og_vector),replace = TRUE))
  Boot <- as.data.frame(Boot)
  Boot_Calcs <- colMeans(Boot)
  Boot_Calcs <- as.vector(Boot_Calcs)
  Boot_Calcs_Rounded <- round(Boot_Calcs,round_value)
  Boot_Calcs_Rounded <- as.data.frame(Boot_Calcs_Rounded)
  Boot_Plot <- ggplot(Boot_Calcs_Rounded, aes(Boot_Calcs_Rounded)) +
     geom_histogram(binwidth = 0.1)
}


test_my_function <- my_rad_bootstrap_function(original,10000,2)
test_my_function


