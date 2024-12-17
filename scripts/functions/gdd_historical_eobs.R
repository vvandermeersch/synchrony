
#------------------------------------------------------#
# GDD optimum in historical conditions (based on EOBS) #
#------------------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# GDD definition
tbase <- 5
tupper <- 35

# Load climate data (EOBS)
eobs_r <- rast(file.path(wd, "data/eobs", "tg_ens_mean_0.1deg_reg_v29.0e.nc"))
gc()

# Compute GDD and optimality
years <- c(1951:2020)
rerun <- TRUE # switch to avoid to recompute everything
if(rerun){
  
  # Sample sites (keep only Europe-ish)
  temp <- subset(eobs_r,1) %>% mask(eu_map)
  mask <- as.data.frame(crop(temp, ext(c(-11.5, 34, 36, 71))), xy = TRUE)
  mask[mask$x > 0 & mask$x < 11 & mask$y > 36 & mask$y < 38,3] <- NA
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
  rm(file, temp);gc()
  
  gdd <- rast(lapply(years, function(yr){
    tmean <- aggregate(mask(crop(subset(subset(eobs_r, which(time(eobs_r, format = "years") == yr)),1:365),
                                 ext(c(-10.5, 34, 36, 71))), sites),agf,na.rm=TRUE)
    tmean <- ifel(tmean < tbase, tbase, ifel(tmean > tupper, tupper, tmean)) # apply lower and upper bound
    gdd <- cumsum(tmean-tbase)
    time(gdd) <- 1:365
    gdd
  }))
  gc()
  sites$id <- rev(which(!is.na(values(subset(gdd,1)))))
  
  #  Compute optimum
  optimality <- compute_optimality(gdd, ncores = 6)
  optimality$period <- "1951_2020"
  optimality$tbase <- tbase
  optimality$tupper <- tupper
  
  saveRDS(optimality, file = file.path(wd, "data/processed/eobs", paste0("optimality_","1951_2020",".rds")))
  
}

optimality_historical <- readRDS(file.path(wd, "data/processed/eobs", paste0("optimality_","1951_2020",".rds")))

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

cowplot::ggsave2(filename = file.path(wd, "figures", "optimality.pdf"),
                 plot = optimum_plot, device = cairo_pdf, width = 68, height = 58, unit = "mm")

