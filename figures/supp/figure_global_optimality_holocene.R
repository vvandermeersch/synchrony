
#-------------------------------------#
# Supp figure S3: Holocene optimality #
#-------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/synchrony"
source(file.path(wd, "scripts", "preamble.R"))

periods <- seq(2000,11000,3000)

optimality_holocene <- lapply(periods, function(p) readRDS(file.path(wd, "data/processed/holocene", paste0("optimality_",p,"BP.rds"))))
optimality_holocene <- as.data.frame(do.call(rbind, optimality_holocene))
optimality_holocene$period <- factor(paste(optimality_holocene$period, "BP"), levels = paste(periods, "BP"))

local_optima <- optimality_holocene %>%
  group_by(period, id) %>%
  mutate(q95 = quantile(opt, 0.9)) %>%
  dplyr::filter(opt > q95) 

global_optimum <- optimality_holocene %>% 
  group_by(period, doy) %>%
  summarise(opt = median(opt), env_pred = median(env_pred), growth_pot=median(growth_pot)) %>%
  mutate(q95 = quantile(opt, 0.9), opt_period = opt > q95)

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
  scale_color_manual(values = c("#87c7ee", "#c1121f")) +
  theme_bw() +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9)) +
  labs(y = "Optimality", x= "DOY") +
  coord_cartesian(xlim = c(0,365), 
                  ylim = c(min(global_optimum$opt), max(global_optimum$opt) +0.05), 
                  expand = FALSE) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        strip.background = element_blank(), 
        axis.title = element_text(size = 7.5, color = "grey20"),
        axis.text = element_text(size = 6.5, color = "grey30"),
        panel.border=element_rect(color = "grey30"),
        axis.ticks = element_line(color = "grey30", linewidth = 0.3))

cowplot::ggsave2(filename = file.path(wd, "figures/supp", "optimality_holocene.pdf"),
                 plot = optimum_plot, device = cairo_pdf, width = 180, height = 58, unit = "mm")
