
#-------------------------------------#
# Supp figure S3: Holocene optimality #
#-------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
optimality_future_ssp2 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6",  paste0("optimality_","ssp245","_",m,".rds"))))
optimality_future_ssp2 <- as.data.frame(do.call(rbind, optimality_future_ssp2))
optimality_future_ssp2$ssp <-"SSP2-4.5"
optimality_future_ssp5 <- lapply(models, function(m) readRDS(file.path(wd, "data/processed/cmip6",  paste0("optimality_","ssp585","_",m,".rds"))))
optimality_future_ssp5 <- as.data.frame(do.call(rbind, optimality_future_ssp5))
optimality_future_ssp5$ssp <-"SSP5-8.5"
optimality_future <- rbind(optimality_future_ssp2, optimality_future_ssp5)


global_optimum <- optimality_future %>% 
  group_by(ssp, period, doy) %>%
  summarise(opt = mean(opt)) %>%
  mutate(qt = quantile(opt, 0.9), opt_period = opt > qt)

global_optimum_pergcm <- optimality_future %>% 
  group_by(ssp, gcm, period, doy) %>%
  summarise(opt = mean(opt)) %>%
  mutate(qt = quantile(opt, 0.9), opt_period = opt > qt)

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
  geom_line(aes(y = opt, x = doy, group = paste0(gcm, ssp)), 
            data = global_optimum_pergcm %>% filter(period == "2071_2100"),
            alpha = 0.3, color = "#f59c10",
            linewidth = 0.2, lineend = "round") +
  geom_line(aes(y = opt, x = doy, group = 1), 
            data = global_optimum  %>% filter(period == "2071_2100"), color = "white",
            linewidth = 1.1, lineend = "round") +
  geom_line(aes(y = opt, x = doy, color = opt_period, group = 1), 
            data = global_optimum %>% filter(period == "2071_2100"),
            linewidth = 0.6, lineend = "round") +
  scale_color_manual(values = c("#f59c10", "#c1121f")) +
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
