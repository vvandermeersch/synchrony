#--------------------------------------#
# GDD optimum in historical conditions #
#--------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# GDD definition
tbase <- 5
tupper <- 35

# Directory where climate data are stored (ERA5-Land)
data_dir <- "D:/climate/ERA5-Land/phenofit_format/transformed"

# Compute GDD and optimality
years <- c(1951:2020)
period <- paste0(years[1],"_",years[length(years)])
rerun <- TRUE # switch to avoid to recompute everything
if(rerun){
  
  # sample sites
  file <- data.frame(fread(file.path(data_dir, paste0("ERA5LAND_tmp_",years[1],"_dly.fit"))))
  temp <- rast(file[,c(2,1,3)])
  sites <- spatSample(temp, size = 800, "regular", ext = ext(c(-10.5, 34, 36, 71)),
                      cells=FALSE, xy=TRUE, values=FALSE, na.rm = TRUE, exhaustive = TRUE) %>% vect()
  
  # find the maximum aggregation factor (to reduce size of rasters and computation time thereafter)
  agf <- 1
  while(length(values(aggregate(mask(temp, sites),agf,na.rm=TRUE), na.rm = TRUE)) == length(sites)){
    agf <- agf + 1
  }
  agf <- agf - 1
  rm(file, temp);gc()
  
  plan(multisession, workers = 10)
  .sites <- wrap(sites)
  gdd <- future_lapply(years, function(yr){
    cat(paste0(yr, "\n"))
    sites <- unwrap(.sites)
    file <- data.frame(fread(file.path(data_dir, paste0("ERA5LAND_tmp_",yr,"_dly.fit"))))
    tmean <- rast(lapply(3:367, function(i) aggregate(mask(rast(file[,c(2,1,i)]), sites),agf,na.rm=TRUE)))
    tmean <- ifel(tmean < tbase, tbase, ifel(tmean > tupper, tupper, tmean)) # apply lower and upper bound
    gdd <- mask(cumsum(tmean-tbase),sites)
    time(gdd) <- 1:365
    cat(paste0(ext(gdd), "\n"))
    wrap(gdd)
  })
  plan(sequential);gc()
  gdd <- rast(lapply(gdd, rast))
  gc()
  
  #  Compute optimum
  optimality <- compute_optimality(gdd, ncores = 10)
  optimality$period <- period
  optimality$tbase <- tbase
  optimality$tupper <- tupper
  
  saveRDS(optimality, file = file.path(wd, "data/processed/era5_land", paste0("optimality_",period,".rds")))
  
}
optimality_historical <- readRDS(file.path(wd, "data/processed/era5_land", paste0("optimality_","1951_2020",".rds")))

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
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), max(global_optimum$opt) +0.05), 
                  expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_historical_1950_2020.pdf"),
                 plot = optimum_plot, device = cairo_pdf, width = 68, height = 58, unit = "mm")
