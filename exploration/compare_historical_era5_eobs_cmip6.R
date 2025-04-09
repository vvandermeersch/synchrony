

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"

models <- c("GFDL-ESM4")
optimality_hist_cmip6 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6", "1951_2020", paste0("optimality_","ssp245","_",m,".rds"))))
optimality_hist_cmip6 <- as.data.frame(do.call(rbind, optimality_hist_cmip6))
optimality_hist_cmip6$type <- "CMIP6"
optimality_hist_cmip6$period <- "1951_2020"

optimality_hist_era5land <- readRDS(file.path(wd, "data/processed/era5land", paste0("optimality_","1951_2020",".rds")))
optimality_hist_era5land$type <- "ERA5-Land"
optimality_hist_era5land$ssp <- "none"
optimality_hist_era5land$gcm <- "none"

optimality_hist_eobs <- readRDS(file.path(wd, "data/processed/eobs", paste0("optimality_","1951_2020",".rds")))
optimality_hist_eobs$type <- "EOBS"
optimality_hist_eobs$ssp <- "none"
optimality_hist_eobs$gcm <- "none"


optimality_historical <- rbind(optimality_hist_cmip6, optimality_hist_era5land, optimality_hist_eobs)

global_optimum <- optimality_historical %>% 
  group_by(type, ssp, period, gcm, doy) %>%
  summarise(opt = mean(opt)) %>%
  mutate(qt = quantile(opt, 0.95), opt_period = opt > qt)

local_optima <- optimality_historical %>%
  group_by(type, ssp, period, id, gcm) %>%
  mutate(qt = quantile(opt, 0.95)) %>%
  dplyr::filter(opt > qt) 

optimum_plot <- ggplot() +
  geom_vline(xintercept = 172, linetype = "dashed", 
             color = "grey70", linewidth = 0.3) +
  # geom_line(aes(y = opt, x = doy, group = paste0(id, ssp, gcm), color = paste0(period,ssp)),
  #           data = optimality_future %>% filter(period == "2071_2100"),
  #           alpha = 0.02,
  #           linewidth = 0.1, lineend = "round") +
  geom_boxplot(aes(x = doy, y = 0.97, color = type),
               width = 0.02,
               linewidth = 0.4, outliers = FALSE,
               data = local_optima %>% filter(type == "ERA5-Land"), show.legend = FALSE) +
  geom_boxplot(aes(x = doy, y = 1.01, color = type),
               width = 0.02,
               linewidth = 0.4, outliers = FALSE,
               data = local_optima %>% filter(type == "EOBS"), show.legend = FALSE) +
  geom_boxplot(aes(x = doy, y = 1.07, color = type, group = gcm),
               width = 0.025*5/2,
               linewidth = 0.4, outliers = FALSE,
               data = local_optima %>% filter(type == "CMIP6"), show.legend = FALSE) +
  # geom_line(aes(y = opt, x = doy, group = type), 
  #           data = global_optimum, color = "white",
  #           linewidth = 1.1, lineend = "round") +
  geom_line(aes(y = opt, x = doy, color = type, group = paste(gcm,type)), 
            data = global_optimum,
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#f6a511", "#7b9d45", "#457b9d")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.84, 0.87),
        legend.title = element_blank(),
        legend.text = element_text(size = 4.5, margin = margin(r = 2, l = 2)),
        legend.margin=margin(t = 0, unit='cm'),
        legend.box.background = element_rect(colour = "black", linewidth = 0.5),
        strip.background = element_blank(), 
        legend.key.size = unit(3,"mm"),
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), 1.15), 
                  expand = FALSE)

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_comparison_cmip6_era5land_19812020.pdf"),
                 plot = optimum_plot, device = cairo_pdf, width = 68, height = 58, unit = "mm")
