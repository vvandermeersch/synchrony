#--------------------------------------------------#
# GDD optimum in historical conditions (ERA5-LAND) #
#--------------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# GDD definition
tlower <- 5
tupper <- 35

# Load climate data (ERA5-Land post-processed daily statistics - in Kelvin)
data_dir <- "D:/climate"
era5_r <- rast(lapply(1951:2020, function(yr){
  er <- round(rast(file.path(data_dir, "ERA5-Land/raw_daily", paste0("tmp_",yr,".nc")))-273.15,2)
  time(er) <- seq(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-12-31")), by="days")
  er
  }))
gc()

# Compute GDD and optimality
years <- c(1951:2020)
rerun <- TRUE # switch to avoid to recompute everything
if(rerun){
  
  # Sample sites (keep only Europe-ish)
  temp <- subset(era5_r,1) %>% mask(eu_map)
  mask <- as.data.frame(crop(temp, ext(c(-11.5, 34, 36, 71))), xy = TRUE)
  # mask[mask$x > 0 & mask$x < 11 & mask$y > 36 & mask$y < 38,3] <- NA
  mask_r <- rast(mask)
  rm(mask)
  sites <- spatSample(mask_r, size = 900, "regular", ext = ext(c(-11.5, 34, 36, 71)),
                      cells=FALSE, xy=TRUE, values=FALSE, na.rm = TRUE, exhaustive = TRUE) %>% vect()
  crs(mask_r) <- crs(sites) <- "EPSG:4326"
  
  # find the maximum aggregation factor (to reduce size of rasters and computation time thereafter)
  agf <- 1
  nsites <- length(values(aggregate(mask(temp, sites),agf,na.rm=TRUE), na.rm = TRUE))
  while(length(values(aggregate(mask(temp, sites),agf,na.rm=TRUE), na.rm = TRUE)) == nsites){
    agf <- agf + 1
  }
  agf <- agf - 1
  rm(temp);gc()
  
  gdd <- rast(lapply(years, function(yr){
    tmean <- aggregate(mask(crop(subset(subset(era5_r, which(time(era5_r, format = "years") == yr)),1:365),
                                 ext(c(-11.5, 34, 36, 71))), sites),agf,na.rm=TRUE)
    tmean <- ifel(tmean < tlower, tlower, ifel(tmean > tupper, tupper, tmean)) # apply lower and upper bound
    gdd <- cumsum(tmean-tlower)
    time(gdd) <- 1:365
    gdd
  }))
  gc()
  saveRDS(gdd, file = file.path(wd, "data/processed/era5land", paste0("gdd_", min(years) ,"_", max(years), "_", "tlow", tlower, "_tupp", tupper, ".rds")))
  
  #  Compute optimum
  optimality <- compute_optimality(gdd, ncores = 8)
  optimality$period <- "1951_2020"
  optimality$tbase <- tlower
  optimality$tupper <- tupper
  
  saveRDS(optimality, file = file.path(wd, "data/processed/era5land", paste0("optimality_", min(years) ,"_", max(years), "_", "tlow", tlower, "_tupp", tupper, ".rds")))
  
  sites$id <- rev(which(!is.na(values(subset(gdd,1)))))
  saveRDS(sites, file = file.path(wd, "data/processed", "sites.rds"))
  saveRDS(mask_r, file = file.path(wd, "data/processed", "mask.rds"))
  
}

optimality_historical <- readRDS(file.path(wd, "data/processed/era5land",  paste0("optimality_", min(years) ,"_", max(years), "_", "tlow", tlower, "_tupp", tupper, ".rds")))

global_optimum <- optimality_historical %>% 
  group_by(doy) %>%
  summarise(opt = mean(opt)) %>%
  mutate(qt = quantile(opt, 0.95), opt_period = opt > qt)

# optimality_future_summ <- optimality_future %>%
#   group_by(ssp, period, doy, id) %>%
#   summarise(opt = mean(opt)) # average across GCMs for each cell

local_optima <- optimality_historical %>%
  group_by(id) %>%
  mutate(qt = quantile(opt, 0.95)) %>%
  dplyr::filter(opt > qt)

optimum_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.35) +
  geom_boxplot(aes(x = doy, y = max(global_optimum$opt) +0.03),
               width = 0.015, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = local_optima) +
  # geom_point(aes(x = mean(local_optima$doy), y = max(global_optimum$opt) +0.03), color = "#c1121f") +
  # geom_line(aes(y = opt, x = doy), 
  #           data = global_optimum,
  #           color = "white", linewidth = 1.5) +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#457b9d", "#c1121f")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)
  ) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), max(global_optimum$opt) +0.05), 
                  expand = FALSE)
