
# Preamble
library(terra)
library(dplyr)
library(ggplot2)
library(data.table)
library(future)
library(future.apply)

source(file.path(wd, "scripts", "functions", "compute_optimality.R"))

options(future.globals.maxSize = 8000 * 1024^2)
set.seed(1996)
