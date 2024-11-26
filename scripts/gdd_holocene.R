
#-------------------------------#
# GDD optimum over the Holocene #
#-------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# GDD definition
tbase <- 0
tupper <- 35

# Directory where climate data are stored (from Van der Meersch et al. 2024)
data_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/025deg"

# Compute GDD and optimality
periods <- seq(2000,11000,3000)
rerun <- TRUE # switch to avoid to recompute everything
if(rerun){
  for(period in periods){
    gdd <- rast(lapply((period-15):(period+15), function(yr){
      file <- data.frame(fread(file.path(data_dir, paste0(period, "BP"), paste0("hadCM3B_tmp_-",yr,"_dly.fit"))))
      tmean <- rast(lapply(3:367, function(i) rast(file[,c(2,1,i)])))
      tmean <- ifel(tmean < tbase, tbase, ifel(tmean > tupper, tupper, tmean)) # apply lower and upper bound
      gdd <- cumsum(tmean-tbase)
      time(gdd) <- 1:365
      gdd
    }))
    
    # Sample random sites on a regular grid
    sites <- spatSample(subset(gdd,1), size = 1000, "regular", ext = ext(c(-14, 40, 34, 71)), 
                        cells=FALSE, xy=TRUE, values=FALSE, na.rm = TRUE, exhaustive = TRUE) %>% vect()
    
    #  Compute optimum
    optimality <- compute_optimality(gdd, sites, ncores = 2)
    optimality$period <- period
    
    saveRDS(optimality, file = file.path(wd, "data/processed/holocene", paste0("optimality_",period,"BP.rds")))
  }
}
optimality_holocene <- lapply(periods, function(p) readRDS(file.path(wd, "data/processed/holocene", paste0("optimality_",p,"BP.rds"))))
optimality_holocene <- as.data.frame(do.call(rbind, optimality_holocene))
optimality_holocene$period <- factor(paste(optimality_holocene$period, "BP"), levels = paste(periods, "BP"))

local_optima <- optimality_holocene %>%
  group_by(period, id) %>%
  mutate(q95 = quantile(opt, 0.95)) %>%
  dplyr::filter(opt > q95) 

global_optimum <- optimality_holocene %>% 
  group_by(period, doy) %>%
  summarise(opt = median(opt), env_pred = median(env_pred), growth_pot=median(growth_pot)) %>%
  mutate(q95 = quantile(opt, 0.95), opt_period = opt > q95)

optimum_plot <- ggplot() +
  facet_wrap(~ period, nrow = 1) +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.4) +
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

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_holocene.pdf"),
        plot = optimum_plot, device = cairo_pdf, width = 180, height = 58, unit = "mm")

