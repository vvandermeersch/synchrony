
#---------------------------#
# GDD optimum in the future #
#---------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# GDD definition
tbase <- 0
tupper <- 35

# Directory where climate data are stored (from Noel et al. 2022)
data_dir <- "D:/climate/CMIP6_Adjust"

# Compute GDD for 5000BP
ssp <- "ssp245"
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
years <- c(2080:2100)
rerun <- FALSE # switch to avoid to recompute everything
if(rerun){
  for(gcm in models){
    gdd <- rast(lapply(years, function(yr){
      file <- data.frame(fread(file.path(data_dir, scenario, gcm, "phenofit_format", paste0(gcm,"_tmp_",yr,"_dly.fit"))))
      tmean <- rast(lapply(3:367, function(i) rast(file[,c(2,1,i)])))
      tmean <- ifel(tmean < tbase, tbase, ifel(tmean > tupper, tupper, tmean)) # apply lower and upper bound
      gdd <- cumsum(tmean-tbase)
      time(gdd) <- 1:365
      gdd
    }))
    gc()
    
    # Sample random sites on a regular grid
    sites <- spatSample(subset(gdd,1), size = 1000, "regular", ext = ext(c(-14, 37, 36, 71)), 
                        cells=FALSE, xy=TRUE, values=FALSE, na.rm = TRUE, exhaustive = TRUE) %>% vect()
    
    #  Compute optimum
    optimality <- compute_optimality(gdd, sites, ncores = 2)
    optimality$period <- period
    
    saveRDS(optimality, file = file.path(wd, "data/processed/future", paste0("optimality_",ssp,"_",gcm,".rds")))
  }
}
optimality_future <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/future", paste0("optimality_",ssp,"_",m,".rds"))))
optimality_future <- as.data.frame(do.call(rbind, optimality_future))
optimality_future$ssp <- ifelse(optimality_future$ssp == "ssp245", "SSP2-4.5", "SSP5-8.5")

local_optima <- optimality_future %>%
  group_by(ssp, id) %>%
  mutate(q95 = quantile(opt, 0.95)) %>%
  dplyr::filter(opt > q95) 

global_optimum <- optimality_future %>% 
  group_by(ssp, doy) %>%
  summarise(opt = median(opt)) %>%
  mutate(q95 = quantile(opt, 0.95), opt_period = opt > q95)

optimality_future_summ <- optimality_future %>%
  group_by(ssp, doy, id) %>%
  summarise(opt = mean(opt)) # average across GCMs for each cell

optimum_plot <- ggplot() +
  facet_wrap(~ ssp, nrow = 1) +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(x = doy, y = max(optimality_future_summ$opt) +0.03),
               width = 0.025, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = local_optima) +
  geom_line(aes(y = opt, x = doy, group = paste0(id, ssp)), 
            data = optimality_future_summ,
            alpha = 0.1, color = "#a7c5d8",
            linewidth = 0.2, lineend = "round") +
  geom_line(aes(y = opt, x = doy, group = 1), 
            data = global_optimum, color = "white",
            linewidth = 1.1, lineend = "round") +
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
                  ylim = c(min(global_optimum$opt), max(optimality_future_summ$opt) +0.07), 
                  expand = FALSE)
  
cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_future.pdf"),
        plot = optimum_plot, device = cairo_pdf, width = 58, height = 58, unit = "mm")

