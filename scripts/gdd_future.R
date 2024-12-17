
#---------------------------#
# GDD optimum in the future #
#---------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

# GDD definition
tbase <- 5
tupper <- 35

# Directory where climate data are stored (from Noel et al. 2022)
data_dir <- "D:/climate/CMIP6_Adjust"

# Compute GDD and optimality
ssp <- "ssp585"
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
years <- c(2071:2100)
rerun <- TRUE # switch to avoid to recompute everything
if(rerun){
  for(gcm in models){
    print(gcm)
    
    # find the maximum aggregation factor (to reduce size of rasters and computation time thereafter)
    sites <- readRDS(file = file.path(wd, "data/processed", "sites.rds"))
    file <- data.frame(fread(file.path(data_dir, ssp, gcm, "phenofit_format", paste0(gcm,"_tmp_",2071,"_dly.fit"))))
    temp <- rast(lapply(3, function(i) rast(file[,c(2,1,i)])))
    agf <- 1
    nsites <- length(values(aggregate(mask(temp, sites),agf,na.rm=TRUE), na.rm = TRUE))
    while(length(values(aggregate(mask(temp, sites),agf,na.rm=TRUE), na.rm = TRUE)) == nsites){
      agf <- agf + 1
    }
    agf <- agf - 1
    rm(temp);gc()
    
    gdd <- rast(lapply(years, function(yr){
      file <- data.frame(fread(file.path(data_dir, ssp, gcm, "phenofit_format", paste0(gcm,"_tmp_",yr,"_dly.fit"))))
      tmean <- aggregate(mask(crop(rast(lapply(3:367, function(i) rast(file[,c(2,1,i)]))),
                                   ext(c(-11.5, 34, 36, 71))), sites),agf,na.rm=TRUE)
      tmean <- ifel(tmean < tbase, tbase, ifel(tmean > tupper, tupper, tmean)) # apply lower and upper bound
      gdd <- cumsum(tmean-tbase)
      time(gdd) <- 1:365
      gdd
    }))
    gc()
    
    #  Compute optimum
    optimality <- compute_optimality(gdd, ncores = 8)
    optimality$period <- "2071_2100"
    optimality$tbase <- tbase
    optimality$tupper <- tupper
    optimality$gcm <- gcm
    
    saveRDS(optimality, file = file.path(wd, "data/processed/cmip6", paste0("optimality_",ssp,"_",gcm,".rds")))
  }
}
optimality_future_ssp2 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6", "2071_2100", paste0("optimality_","ssp245","_",m,".rds"))))
optimality_future_ssp2 <- as.data.frame(do.call(rbind, optimality_future_ssp2))
optimality_future_ssp5 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6", "2071_2100", paste0("optimality_","ssp585","_",m,".rds"))))
optimality_future_ssp5 <- as.data.frame(do.call(rbind, optimality_future_ssp5))
optimality_future <- rbind(optimality_future_ssp2, optimality_future_ssp5)

# optimality_historical <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6", "1951_2020", paste0("optimality_","ssp245","_",m,".rds"))))
# optimality_historical <- as.data.frame(do.call(rbind, optimality_historical))
# 
# 
# optimality_future <- rbind(optimality_future, optimality_historical, optimality_historical %>% mutate(ssp = "ssp585"))
optimality_future$ssp <- ifelse(optimality_future$ssp == "ssp245", "SSP2-4.5", "SSP5-8.5")

global_optimum <- optimality_future %>% 
  group_by(ssp, period, doy) %>%
  summarise(opt = mean(opt)) %>%
  mutate(qt = quantile(opt, 0.9), opt_period = opt > qt)

# optimality_future_summ <- optimality_future %>%
#   group_by(ssp, period, doy, id) %>%
#   summarise(opt = mean(opt)) # average across GCMs for each cell

local_optima <- optimality_future %>%
  group_by(ssp, period, id, gcm) %>%
  mutate(qt = quantile(opt, 0.9)) %>%
  dplyr::filter(opt > qt) 

optimum_plot <- ggplot() +
  facet_grid(~ ssp) +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(x = doy, y = 1.2),
               width = 0.025, color = "#c1121f",
               linewidth = 0.3, outliers = FALSE,
               data = local_optima %>% filter(period == "2071_2100")) +
  geom_line(aes(y = opt, x = doy, group = paste0(gcm, id, ssp)), 
            data = optimality_future%>% filter(period == "2071_2100"),
            alpha = 0.05, color = "#7fbf9a",
            linewidth = 0.05, lineend = "round") +
  geom_line(aes(y = opt, x = doy, group = 1), 
            data = global_optimum  %>% filter(period == "2071_2100"), color = "white",
            linewidth = 1.1, lineend = "round") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum %>% filter(period == "2071_2100"),
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#17a353", "#c1121f")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), 1.25), 
                  expand = FALSE) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3))
  
cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_future.pdf"),
        plot = optimum_plot, device = cairo_pdf, width = 105, height = 58, unit = "mm")

# optimum_plot <- ggplot() +
#   facet_grid(~ ssp) +
#   geom_vline(xintercept = 172, linetype = "dashed", 
#              color = "grey70", linewidth = 0.3) +
#   # geom_line(aes(y = opt, x = doy, group = paste0(id, ssp, gcm), color = paste0(period,ssp)),
#   #           data = optimality_future %>% filter(period == "2071_2100"),
#   #           alpha = 0.02,
#   #           linewidth = 0.1, lineend = "round") +
#   geom_boxplot(aes(x = doy, y = 1.21, color = paste0(period, ssp)),
#                width = 0.025,
#                linewidth = 0.3, outliers = FALSE,
#                data = local_optima %>% filter(period == "2071_2100")) +
#   geom_boxplot(aes(x = doy, y = 1.17, color = paste0(period, ssp)),
#                width = 0.025,
#                linewidth = 0.3, outliers = FALSE,
#                data = local_optima %>% filter(period == "1951_2020")) +
#   geom_line(aes(y = opt, x = doy, group = period), 
#             data = global_optimum, color = "white",
#             linewidth = 1.1, lineend = "round") +
#   geom_line(aes(y = opt, x = doy, color = paste0(period, ssp), group = period), 
#             data = global_optimum,
#             linewidth = 0.6, lineend = "round") +
#   scale_color_manual(values = c("#7b9d45", "#7b9d45", "#f6a511", "#ab1a1e")) +
#   theme_bw() +
#   theme(legend.position = 'none', panel.grid = element_blank(),
#         strip.background = element_blank(), 
#         axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)) +
#   labs(y = "Optimality", x= "DOY") +
#   coord_cartesian(xlim = c(0,365), 
#                   ylim = c(min(global_optimum$opt), 1.25), 
#                   expand = FALSE)
# 
# cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_future_withhistorical.pdf"),
#                  plot = optimum_plot, device = cairo_pdf, width = 105, height = 58, unit = "mm")

