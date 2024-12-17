
# Preamble
library(terra)
library(dplyr)
library(ggplot2)
library(data.table)
library(future)
library(future.apply)

source(file.path(wd, "scripts", "functions", "compute_optimality.R"))

options(future.globals.maxSize = 14000 * 1024^2)
set.seed(1996)

# Define Europe
countries <- c("Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany",
               "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Moldova", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
               "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom","Ukraine", "Bosnia and Herzegovina", "Republic of Serbia", 
               "North Macedonia", "Greece", "Kosovo", "Albania", "Montenegro", "Russia", "Liechtenstein", "Luxembourg", "Andorra", "Iceland", "Turkey",
               "Algeria", "Tunisia")
world_map <- rnaturalearth::ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(sovereignt %in% countries) %>% vect() %>% crop(ext(c(-11.5, 34, 35, 71)))

# world_map %>% filter(sovereignt %in% countries) %>% vect() %>% project("EPSG:3035") %>% crop(c(2.2e6, 6e6, 1.4e6, 5.4e6)) %>% 
#   project("EPSG:4326")

# Sample sites - EOBS
# eobs_r <- subset(rast(file.path(wd, "data/eobs", "tg_ens_mean_0.1deg_reg_v29.0e.nc")),1)
# mask <- as.data.frame(crop(eobs_r, ext(c(-11.5, 34, 36, 71))), xy = TRUE)
# mask[mask$x > 0 & mask$x < 11 & mask$y > 36 & mask$y < 38,"tg_1"] <- NA
# mask_r <- rast(mask)
# rm(mask)
# sites <- spatSample(mask_r, size = 900, "regular", ext = ext(c(-11.5, 34, 36, 71)),
#                     cells=FALSE, xy=TRUE, values=FALSE, na.rm = TRUE, exhaustive = TRUE) %>% vect()

